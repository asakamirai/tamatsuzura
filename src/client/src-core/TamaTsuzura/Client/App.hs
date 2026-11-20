{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.App
    ( -- * ランタイム環境・状態・アクション型
      ClientEnv(..)
    , defaultClientEnv
    , ConnectionStatus(..)
    , MobilePane(..)
    , EditorViewMode(..)
    , PendingNavigation(..)
    , FilterState(..)
    , defaultFilterState
    , EditorDraft(..)
    , emptyEditorDraft
    , draftFromDocument
    , editorDraftFromStored
    , toStoredDraft
    , DraftReconcileResult(..)
    , reconcileDocumentWithDraft
    , buildRecoveryRequestFromStoredDraft
    , SearchPanelState(..)
    , emptySearchPanelState
    , ClientState(..)
    , ClientAction(..)
      -- * 純粋な変換・検証・判定関数
    , parseFolderInput
    , parseTagsInput
    , formatTagsInput
    , validateEditorDraft
    , mergeDraftAfterSave
    , isDraftDirty
    , isCurrentDocumentDirty
    , buildListQueryFromFilter
    , hasActiveFilters
    , buildEditorSyncPayload
      -- * 純粋な状態初期化・遷移関数
    , initialClientState
    , applyExtensionById
    , applyHealthResult
    , applyDocumentsResult
    , applyTagsResult
    , applyFoldersResult
    , applyDraftsLoaded
    , applyDraftStorageEvent
    , applyDocumentLoaded
    , applyDocumentCreated
    , applyDocumentSaved
    , applyDocumentSoftDeleted
    , applyDocumentRestored
    , applyDocumentPermanentlyDeleted
    , applyEditorEvent
    , dismissError
    , dismissNotice
      -- * Miso MVU 更新ロジック
    , updateClient
    , mkClientComponent
    ) where

import           Control.Monad                   (when)
import           Control.Monad.State.Class       (get)
import           Data.Either                     (fromRight)
import           Data.Foldable                   (for_)
import           Data.List                       (find)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (isJust, isNothing)
import           Data.Text                       (Text)
import qualified Data.Text                       as Txt
import           Data.Time.Clock                 (UTCTime, getCurrentTime)
import           Miso                            (App, Component(..), Effect, View, component, io, io_, modify)
import           TamaTsuzura.Client.Api
    ( ApiClient
    , ApiClientError(..)
    , createDocument
    , fetchHealth
    , getDocument
    , listDocuments
    , listFolders
    , listTags
    , permanentDeleteDocument
    , restoreDocument
    , softDeleteDocument
    , updateDocument
    )
import           TamaTsuzura.Client.DraftStorage
    ( DraftStorage(..)
    , DraftStorageEvent(..)
    , DraftSyncState(..)
    , StoredDraft(..)
    , browserDraftStorage
    , draftStorageEventsSub
    , latestStoredDraft
    )
import           TamaTsuzura.Client.EditorBridge
    ( EditorCommand(..)
    , EditorEvent(..)
    , EditorSearchParams(..)
    , EditorSearchStatus(..)
    , EditorStats(..)
    , EditorSyncPayload(..)
    , computeTextStats
    , dispatchEditorCommand
    , editorEventsSub
    , emptyEditorStats
    )
import           TamaTsuzura.Client.FileTransfer
    ( ExportSpec(..)
    , FilePickResult(..)
    , FileTransferClient(..)
    , ImportedFile(..)
    , browserFileTransferClient
    , buildExportSpec
    , prepareImportedDocumentRequest
    )
import           TamaTsuzura.Client.I18n
    ( UiMessages(..)
    , formatApiClientError
    , formatFileImportError
    , formatValidationError
    )
import           TamaTsuzura.Common.Api
    ( CreateDocumentRequest(..)
    , DocumentListQuery(..)
    , DocumentListResponse(..)
    , DocumentStatusFilter(..)
    , FolderListResponse(..)
    , HealthStatus
    , TagListResponse(..)
    , UpdateDocumentRequest(..)
    , defaultCreateDocumentRequest
    , defaultDocumentListQuery
    , mkSearchQuery
    )
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody(..)
    , DocumentFormat(..)
    , DocumentId
    , DocumentSummary(..)
    , DocumentTitle
    , FolderPath
    , Tag
    , TagSet
    , defaultDocumentFormat
    , emptyDocumentBody
    , emptyDocumentTitle
    , emptyTagSet
    , isTrashed
    , mkDocumentTitle
    , mkFolderPath
    , mkTagSetFromTexts
    , tagSetToList
    , toDocumentSummary
    , validateDocumentBody
    )
import           TamaTsuzura.Common.Text         (ToText(..))
import           TamaTsuzura.Common.Validation   (ValidationError)
import           TamaTsuzura.ExtensionApi        (Extension(..), ExtensionId, ExtensionMetadata(..))

-- | Miso コンポーネント実行時に外部から注入される不変の依存環境。
-- 関数を含む 'Extension' や 'HttpTransport'・エディタコマンド送出器・ローカルストレージ・
-- ファイル入出力クライアントを 'ClientState' (Model) から切り離すことで、
-- Model の純粋な同値比較 ('Eq') と単体テスト容易性を両立する。
data ClientEnv = ClientEnv
    { apiClient         :: !ApiClient
      -- ^ M1 REST API 通信クライアント
    , extensions        :: ![Extension]
      -- ^ 結線層から注入された同梱拡張一覧
    , messages          :: !UiMessages
      -- ^ UI 表示文言カタログ
    , sendEditorCmd     :: !(EditorCommand -> IO ())
      -- ^ CodeMirror 6 ブリッジへのコマンド送出関数 (テスト時はモック注入可)
    , draftStorage      :: !DraftStorage
      -- ^ IndexedDB ドラフト自動退避クライアント (テスト時はインメモリ注入可)
    , fileTransfer      :: !FileTransferClient
      -- ^ ローカルファイルのエクスポート・インポートクライアント (テスト時はモック注入可)
    , getCurrentUtcTime :: !(IO UTCTime)
      -- ^ ドラフト退避時刻の取得関数 (テスト時は決定的クロック注入可)
    }

-- | 標準のブラウザ JSFFI 実装群を用いる 'ClientEnv' を構築する。
defaultClientEnv :: ApiClient -> [Extension] -> UiMessages -> ClientEnv
defaultClientEnv client exts msgs = ClientEnv
    { apiClient         = client
    , extensions        = exts
    , messages          = msgs
    , sendEditorCmd     = dispatchEditorCommand
    , draftStorage      = browserDraftStorage
    , fileTransfer      = browserFileTransferClient
    , getCurrentUtcTime = getCurrentTime
    }

-- | サーバとの接続状態。
data ConnectionStatus
    = ConnChecking
      -- ^ 初回または再確認のヘルスチェック実行中
    | ConnOnline
      -- ^ サーバ疎通確認済み
    | ConnError !Text
      -- ^ 接続失敗 (ユーザー向け説明メッセージ)
    deriving (Eq, Show)

-- | モバイル幅表示時のアクティブペイン。
data MobilePane
    = PaneList
      -- ^ 文書一覧・整理フィルタ画面
    | PaneEditor
      -- ^ エディタ・プレビュー画面
    deriving (Eq, Show)

-- | エディタ領域の表示モード。
data EditorViewMode
    = ModeEdit
      -- ^ エディタ単体表示 (既定値)
    | ModeSplit
      -- ^ エディタ + Markdown プレビューの左右分割表示 (広幅画面向け)
    | ModePreview
      -- ^ Markdown プレビュー単体表示
    deriving (Eq, Show)

-- | 未保存変更がある状態で画面遷移を試みた際に確認待ちとする遷移先。
data PendingNavigation
    = NavOpenDocument !DocumentId
      -- ^ 別の文書を開く
    | NavCreateDocument
      -- ^ 新規文書を作成する
    | NavSwitchStatus !DocumentStatusFilter
      -- ^ 稼働中 / ゴミ箱タブを切り替える
    | NavReturnToList
      -- ^ モバイルで一覧画面へ戻る
    | NavImportDocument
      -- ^ ローカルファイルから新規文書をインポートする
    | NavRecoverOrphanedDraft
      -- ^ 孤立ドラフトを新規文書として復元する
    deriving (Eq, Show)

-- | 文書一覧の検索・整理フィルタ状態。
data FilterState = FilterState
    { status            :: !DocumentStatusFilter
      -- ^ 稼働中 / ゴミ箱 / すべて
    , searchInput       :: !Text
      -- ^ 検索入力欄の現在値
    , selectedTag       :: !(Maybe Tag)
      -- ^ 絞り込み対象タグ
    , selectedFolder    :: !(Maybe FolderPath)
      -- ^ 絞り込み対象フォルダ
    , includeSubfolders :: !Bool
      -- ^ フォルダ絞り込み時にサブフォルダを含めるか
    } deriving (Eq, Show)

-- | 既定のフィルタ状態 (稼働中・条件なし)。
defaultFilterState :: FilterState
defaultFilterState = FilterState
    { status            = StatusActive
    , searchInput       = ""
    , selectedTag       = Nothing
    , selectedFolder    = Nothing
    , includeSubfolders = True
    }

-- | エディタで現在編集中の未確定ドラフト状態。
-- タイトル・フォルダ・タグは入力途中の未完成文字列を壊さずに保持するため生 'Text' とし、
-- 保存時および変更検出時に 'validateEditorDraft' でドメイン型へ厳格検証する。
data EditorDraft = EditorDraft
    { titleInput  :: !Text
      -- ^ 編集中タイトル入力文字列
    , body        :: !DocumentBody
      -- ^ 編集中本文
    , format      :: !DocumentFormat
      -- ^ 編集中フォーマット種別
    , folderInput :: !Text
      -- ^ 編集中フォルダパス入力文字列 (空欄でルート)
    , tagsInput   :: !Text
      -- ^ 編集中タグ一覧入力文字列 (カンマ区切り)
    } deriving (Eq, Show)

-- | 空の編集ドラフト。
emptyEditorDraft :: EditorDraft
emptyEditorDraft = EditorDraft
    { titleInput  = ""
    , body        = emptyDocumentBody
    , format      = defaultDocumentFormat
    , folderInput = ""
    , tagsInput   = ""
    }

-- | サーバから取得した 'Document' から編集ドラフトを初期化する。
draftFromDocument :: Document -> EditorDraft
draftFromDocument doc = EditorDraft
    { titleInput  = toText doc.title
    , body        = doc.body
    , format      = doc.format
    , folderInput = maybe "" toText doc.folder
    , tagsInput   = formatTagsInput doc.tags
    }

-- | IndexedDB に退避された 'StoredDraft' をエディタ上の 'EditorDraft' へ復元変換する。
editorDraftFromStored :: StoredDraft -> EditorDraft
editorDraftFromStored sd = EditorDraft
    { titleInput  = sd.titleInput
    , body        = sd.body
    , format      = sd.format
    , folderInput = sd.folderInput
    , tagsInput   = sd.tagsInput
    }

-- | 現在の保存済み 'Document' と編集中 'EditorDraft' から IndexedDB 退避用 'StoredDraft' を構築する。
toStoredDraft :: Document -> EditorDraft -> UTCTime -> StoredDraft
toStoredDraft doc currentDraft now = StoredDraft
    { docId         = doc.id
    , titleInput    = currentDraft.titleInput
    , body          = currentDraft.body
    , format        = currentDraft.format
    , folderInput   = currentDraft.folderInput
    , tagsInput     = currentDraft.tagsInput
    , baseUpdatedAt = doc.updatedAt
    , savedAt       = now
    }

-- | サーバから取得した 'Document' とローカル 'StoredDraft' の照合結果。
data DraftReconcileResult
    = NoLocalDraft
      -- ^ ローカルドラフトは存在しない
    | RedundantLocalDraft
      -- ^ ローカルドラフトの内容がサーバ版と同一のため自動削除してよい
    | RestoreLocalDraft !StoredDraft
      -- ^ 同一ベース版からの未保存変更であり安全に自動復元できる
    | PromptStaleLocalDraft !StoredDraft
      -- ^ サーバ側が更新済み、またはゴミ箱内文書のため自動上書きせずユーザー判断を促す
    deriving (Eq, Show)

-- | サーバ文書とローカルドラフトを照合し、自動復元・自動掃除・警告提示のいずれを取るか判定する。
reconcileDocumentWithDraft :: Document -> Maybe StoredDraft -> DraftReconcileResult
reconcileDocumentWithDraft _ Nothing = NoLocalDraft
reconcileDocumentWithDraft doc (Just sd)
    | isTrashed doc = PromptStaleLocalDraft sd
    | not (isDraftDirty doc (editorDraftFromStored sd)) = RedundantLocalDraft
    | sd.baseUpdatedAt == doc.updatedAt = RestoreLocalDraft sd
    | otherwise = PromptStaleLocalDraft sd

-- | サーバ上で削除済み (404) となった孤立ドラフトから新規作成リクエストを安全に構築する。
-- タイトル・フォルダ・タグに編集途中の不正値が含まれていた場合でも本文救出を優先して既定値へフォールバックする。
buildRecoveryRequestFromStoredDraft :: StoredDraft -> Either ValidationError CreateDocumentRequest
buildRecoveryRequestFromStoredDraft sd = do
    validBody <- validateDocumentBody sd.body
    let validTitle :: DocumentTitle
        validTitle = fromRight emptyDocumentTitle (mkDocumentTitle sd.titleInput)

        validFolder :: Maybe FolderPath
        validFolder = fromRight Nothing (parseFolderInput sd.folderInput)

        validTags :: TagSet
        validTags = fromRight emptyTagSet (parseTagsInput sd.tagsInput)
    Right CreateDocumentRequest
        { title  = validTitle
        , body   = validBody
        , format = sd.format
        , folder = validFolder
        , tags   = validTags
        }

-- | エディタ内検索・置換パネルの状態。
data SearchPanelState = SearchPanelState
    { isOpen        :: !Bool
      -- ^ 検索バーを展開中か
    , showReplace   :: !Bool
      -- ^ 置換入力行を展開中か
    , query         :: !Text
      -- ^ 検索文字列または正規表現
    , replacement   :: !Text
      -- ^ 置換後文字列
    , caseSensitive :: !Bool
      -- ^ 大文字・小文字を区別するか
    , useRegex      :: !Bool
      -- ^ 正規表現モードを有効にするか
    , matchCount    :: !Int
      -- ^ 一致件数
    , activeMatch   :: !Int
      -- ^ 現在選択中の一致番号 (1 始まり、未選択時は 0)
    , regexError    :: !(Maybe Text)
      -- ^ 正規表現エラー詳細
    } deriving (Eq, Show)

-- | 閉じた状態の初期検索パネル。
emptySearchPanelState :: SearchPanelState
emptySearchPanelState = SearchPanelState
    { isOpen        = False
    , showReplace   = False
    , query         = ""
    , replacement   = ""
    , caseSensitive = False
    , useRegex      = False
    , matchCount    = 0
    , activeMatch   = 0
    , regexError    = Nothing
    }

-- | クライアントコアの状態モデル (Miso Model)。
-- 本モジュールは 'tamatsuzura-common' と 'tamatsuzura-extension-api' のみに依存し、
-- 同梱拡張の具体実装には依存しない。
data ClientState = ClientState
    { connectionStatus             :: !ConnectionStatus
      -- ^ API サーバの疎通状態
    , mobilePane                   :: !MobilePane
      -- ^ モバイル画面での現在表示ペイン
    , editorViewMode               :: !EditorViewMode
      -- ^ 編集 / 分割 / プレビューの表示モード
    , filterState                  :: !FilterState
      -- ^ 文書一覧のフィルタ・検索状態
    , availableTags                :: ![Tag]
      -- ^ サーバから取得した既存タグ一覧
    , availableFolders             :: ![FolderPath]
      -- ^ サーバから取得した既存フォルダ一覧
    , documentSummaries            :: ![DocumentSummary]
      -- ^ 取得済みの文書サマリ一覧
    , totalDocuments               :: !Int
      -- ^ サーバ側の総一致文書件数
    , listRequestSeq               :: !Int
      -- ^ 文書一覧取得リクエストの世代番号 (古い非同期応答の破棄に使用)
    , isLoadingDocuments           :: !Bool
      -- ^ 文書一覧読み込み中フラグ
    , isCreatingDocument           :: !Bool
      -- ^ 新規文書作成中フラグ
    , isImportingDocument          :: !Bool
      -- ^ ローカルファイルインポート中フラグ
    , pendingListReloadAfterCreate :: !Bool
      -- ^ ゴミ箱タブからの新規作成完了後に稼働中一覧を再取得するフラグ (POST/GET レース防止)
    , pendingListReloadAfterImport :: !Bool
      -- ^ ゴミ箱タブからのインポート完了後に稼働中一覧を再取得するフラグ
    , openingDocumentId            :: !(Maybe DocumentId)
      -- ^ 現在 GET 中の文書 ID (レース時の古い応答を無視するためのガード)
    , autoOpeningDraftId           :: !(Maybe DocumentId)
      -- ^ 起動時の自動ドラフト復元で GET 中の文書 ID
    , savingDocument               :: !(Maybe (DocumentId, EditorDraft))
      -- ^ 現在 PUT 保存中の (文書 ID, 送信時点のドラフト) (二重送信防止および保存中追加入力の保持)
    , mutatingDocumentId           :: !(Maybe DocumentId)
      -- ^ 現在ゴミ箱移動・復元・完全削除中の文書 ID
    , selectedDocument             :: !(Maybe Document)
      -- ^ 現在開いている保存済み文書スナップショット
    , draft                        :: !EditorDraft
      -- ^ 現在編集中のドラフト内容
    , storedDrafts                 :: !(Map DocumentId StoredDraft)
      -- ^ IndexedDB に退避されている全未保存ドラフト (文書 ID キー)
    , draftSyncState               :: !DraftSyncState
      -- ^ 現在編集中文書のローカルドラフト退避ステータス
    , restoredDraftDocId           :: !(Maybe DocumentId)
      -- ^ 未保存ドラフト復元バナーを表示中の対象文書 ID
    , staleDraftPrompt             :: !(Maybe StoredDraft)
      -- ^ サーバ側更新済みまたはゴミ箱内文書に残存する未適用ドラフト
    , orphanedDraft                :: !(Maybe StoredDraft)
      -- ^ サーバ側で 404 だった自動復元対象ドラフト (新規文書として救出可能)
    , editorSyncVersion            :: !Int
      -- ^ CodeMirror 6 へ外部変更を同期するたびにインクリメントされるバージョン番号
    , editorStats                  :: !EditorStats
      -- ^ エディタの行数・文字数・カーソル位置
    , canUndo                      :: !Bool
      -- ^ エディタで Undo 可能か
    , canRedo                      :: !Bool
      -- ^ エディタで Redo 可能か
    , metadataPanelOpen            :: !Bool
      -- ^ フォルダ・タグ・形式の整理パネルを開いているか
    , searchPanel                  :: !SearchPanelState
      -- ^ エディタ内検索・置換パネルの状態
    , pendingNavigation            :: !(Maybe PendingNavigation)
      -- ^ 未保存変更破棄の確認ダイアログ待ち遷移
    , confirmPermanentDelete       :: !(Maybe DocumentId)
      -- ^ 完全削除の二段階確認対象文書 ID
    , lastError                    :: !(Maybe Text)
      -- ^ 直近発生したユーザー向けエラーメッセージ
    , statusNotice                 :: !(Maybe Text)
      -- ^ 保存・復元などの完了通知メッセージ
    , extensionMetadatas           :: ![ExtensionMetadata]
      -- ^ 結線層から登録された拡張機能の表示用メタデータ一覧
    } deriving (Eq, Show)

-- | クライアント MVU ループで処理するアクション。
data ClientAction
    = Initialize
      -- ^ マウント時の初回疎通・一覧・タグ・フォルダ取得トリガ
    | RefreshAll
      -- ^ ヘルスチェック・一覧・タグ・フォルダの手動再取得
    | HealthChecked !(Either ApiClientError HealthStatus)
      -- ^ @GET /api/health@ の結果受信
    | DocumentsLoaded !Int !(Either ApiClientError DocumentListResponse)
      -- ^ @GET /api/documents@ の結果受信 (リクエスト世代番号, 結果)
    | TagsLoaded !(Either ApiClientError TagListResponse)
      -- ^ @GET /api/tags@ の結果受信
    | FoldersLoaded !(Either ApiClientError FolderListResponse)
      -- ^ @GET /api/folders@ の結果受信
    | RequestStatusFilter !DocumentStatusFilter
      -- ^ 稼働中 / ゴミ箱タブの切替要求 (未保存変更があれば確認ダイアログ)
    | UpdateListSearchInput !Text
      -- ^ 文書一覧の検索入力欄の変更
    | SubmitListSearch
      -- ^ 文書一覧の検索実行
    | SelectFilterTag !(Maybe Tag)
      -- ^ タグフィルタの変更
    | SelectFilterFolder !(Maybe FolderPath)
      -- ^ フォルダフィルタの変更
    | ToggleIncludeSubfolders !Bool
      -- ^ サブフォルダ含むオプションの変更
    | ClearFilters
      -- ^ 検索・タグ・フォルダフィルタの一括解除
    | RequestOpenDocument !DocumentId
      -- ^ 文書を開く要求 (未保存変更があれば確認ダイアログ)
    | DocumentLoaded !DocumentId !(Either ApiClientError Document)
      -- ^ @GET /api/documents/{id}@ の結果受信
    | RequestCreateDocument
      -- ^ 新規文書作成要求 (未保存変更があれば確認ダイアログ)
    | DocumentCreated !(Either ApiClientError Document)
      -- ^ @POST /api/documents@ の結果受信
    | ReturnToList
      -- ^ モバイルで一覧画面へ戻る要求 (未保存変更があれば確認ダイアログ)
    | ConfirmDiscardNavigation
      -- ^ 未保存変更破棄を確認して保留中の遷移を実行する
    | CancelPendingNavigation
      -- ^ 未保存変更破棄の確認ダイアログを閉じる
    | UpdateDraftTitle !Text
      -- ^ タイトル入力変更
    | UpdateDraftFormat !DocumentFormat
      -- ^ 文書フォーマット切替
    | UpdateDraftFolder !Text
      -- ^ フォルダ入力変更
    | UpdateDraftTags !Text
      -- ^ タグ入力変更
    | ToggleMetadataPanel
      -- ^ 整理メタデータパネルの開閉
    | SetEditorViewMode !EditorViewMode
      -- ^ 編集 / 分割 / プレビューモードの切替
    | SaveCurrentDocument
      -- ^ 現在文書の明示的保存 (@PUT /api/documents/{id}@)
    | DocumentSaved !DocumentId !(Either ApiClientError Document)
      -- ^ @PUT /api/documents/{id}@ の結果受信
    | EditorUndoClicked
      -- ^ Undo ボタン押下
    | EditorRedoClicked
      -- ^ Redo ボタン押下
    | EditorEventReceived !EditorEvent
      -- ^ CodeMirror 6 ブリッジからのイベント受信
    | ToggleSearchPanel !Bool
      -- ^ 検索/置換パネルの開閉 (置換欄同時展開フラグ)
    | ToggleReplaceRow
      -- ^ 検索パネル内の置換入力行の開閉
    | CloseSearchPanel
      -- ^ 検索/置換パネルを閉じてハイライトを解除する
    | UpdateSearchQuery !Text
      -- ^ エディタ内検索語の入力変更
    | UpdateReplaceText !Text
      -- ^ エディタ内置換語の入力変更
    | ToggleSearchCaseSensitive
      -- ^ 大文字・小文字区別トグル
    | ToggleSearchRegex
      -- ^ 正規表現モードトグル
    | FindNextMatch
      -- ^ 次の一致へ移動
    | FindPrevMatch
      -- ^ 前の一致へ移動
    | ReplaceCurrentMatch
      -- ^ 現在一致を置換
    | ReplaceAllMatches
      -- ^ すべて置換
    | SoftDeleteDocument !DocumentId
      -- ^ 文書をゴミ箱へ移動 (@DELETE /api/documents/{id}@)
    | DocumentSoftDeleted !DocumentId !(Either ApiClientError Document)
      -- ^ 論理削除の結果受信
    | RestoreDocument !DocumentId
      -- ^ ゴミ箱内の文書を復元 (@POST /api/documents/{id}/restore@)
    | DocumentRestored !DocumentId !(Either ApiClientError Document)
      -- ^ 復元の結果受信
    | RequestPermanentDelete !DocumentId
      -- ^ 完全削除の確認ダイアログを表示する
    | CancelPermanentDelete
      -- ^ 完全削除の確認ダイアログを閉じる
    | ConfirmPermanentDelete !DocumentId
      -- ^ 完全削除を確定実行する (@DELETE /api/documents/{id}/permanent@)
    | DocumentPermanentlyDeleted !DocumentId !(Either ApiClientError ())
      -- ^ 完全削除の結果受信
    | ApplyExtension !ExtensionId
      -- ^ 指定した拡張機能を現在の編集本文へ適用する
    | DraftsLoaded !(Either Text [StoredDraft])
      -- ^ 起動時の IndexedDB 未保存ドラフト一覧の読み込み結果
    | DraftStorageEventReceived !DraftStorageEvent
      -- ^ IndexedDB ドラフト自動退避の完了/失敗通知
    | ApplyStaleDraft
      -- ^ サーバ側更新済み文書に対して残存ローカルドラフトを手動適用する
    | DiscardCurrentDraft
      -- ^ 現在文書に紐づくローカル未保存ドラフトを破棄してサーバ版へ戻す
    | RecoverOrphanedDraft
      -- ^ サーバ削除済み文書の孤立ドラフトを新規文書として復元する
    | OrphanDraftRecovered !DocumentId !(Either ApiClientError Document)
      -- ^ 孤立ドラフトからの新規文書作成応答
    | DiscardOrphanedDraft
      -- ^ 孤立ドラフトをローカルから破棄する
    | ExportCurrentDocument
      -- ^ 現在編集中の文書を Markdown またはプレーンテキストとしてダウンロードする
    | ExportFinished !Text !(Either Text ())
      -- ^ エクスポート処理の結果受信 (ファイル名, 成否)
    | RequestImportDocument
      -- ^ ローカルファイルからのインポート開始要求 (未保存変更があれば確認ダイアログ)
    | ImportFilePicked !FilePickResult
      -- ^ ファイル選択ダイアログからの読み込み結果受信
    | DocumentImported !Text !(Either ApiClientError Document)
      -- ^ インポートに伴う @POST /api/documents@ の結果受信 (元ファイル名, 結果)
    | DismissError
      -- ^ 表示中のエラーバナーを閉じる
    | DismissNotice
      -- ^ 表示中の通知バナーを閉じる
    deriving (Eq, Show)

-- | 拡張一覧のメタデータを取り込んで初期クライアント状態を構築する。
initialClientState :: [Extension] -> ClientState
initialClientState exts = ClientState
    { connectionStatus             = ConnChecking
    , mobilePane                   = PaneList
    , editorViewMode               = ModeEdit
    , filterState                  = defaultFilterState
    , availableTags                = []
    , availableFolders             = []
    , documentSummaries            = []
    , totalDocuments               = 0
    , listRequestSeq               = 0
    , isLoadingDocuments           = False
    , isCreatingDocument           = False
    , isImportingDocument          = False
    , pendingListReloadAfterCreate = False
    , pendingListReloadAfterImport = False
    , openingDocumentId            = Nothing
    , autoOpeningDraftId           = Nothing
    , savingDocument               = Nothing
    , mutatingDocumentId           = Nothing
    , selectedDocument             = Nothing
    , draft                        = emptyEditorDraft
    , storedDrafts                 = Map.empty
    , draftSyncState               = DraftSyncIdle
    , restoredDraftDocId           = Nothing
    , staleDraftPrompt             = Nothing
    , orphanedDraft                = Nothing
    , editorSyncVersion            = 0
    , editorStats                  = emptyEditorStats
    , canUndo                      = False
    , canRedo                      = False
    , metadataPanelOpen            = False
    , searchPanel                  = emptySearchPanelState
    , pendingNavigation            = Nothing
    , confirmPermanentDelete       = Nothing
    , lastError                    = Nothing
    , statusNotice                 = Nothing
    , extensionMetadatas           = map (\ ext -> ext.metadata) exts
    }

-- | フォルダ入力欄の生文字列を検証する。空白のみの入力はルート ('Nothing') とみなす。
parseFolderInput :: Text -> Either ValidationError (Maybe FolderPath)
parseFolderInput raw =
    let trimmed :: Text
        trimmed = Txt.strip raw
    in  if Txt.null trimmed
            then Right Nothing
            else Just <$> mkFolderPath trimmed

-- | カンマ区切り (全角読点・全角カンマ含む) のタグ入力文字列を検証して 'TagSet' へ変換する。
-- 空入力および区切り文字のみによる空要素は除去する。
parseTagsInput :: Text -> Either ValidationError TagSet
parseTagsInput raw =
    let parts :: [Text]
        parts = filter (not . Txt.null) (map Txt.strip (Txt.split isTagDelimiter raw))
    in  if null parts
            then Right emptyTagSet
            else mkTagSetFromTexts parts
  where
    isTagDelimiter :: Char -> Bool
    isTagDelimiter ch = ch == ',' || ch == '、' || ch == '，'

-- | 'TagSet' を編集フォーム用のカンマ区切りテキストへ整形する。
formatTagsInput :: TagSet -> Text
formatTagsInput tags = Txt.intercalate ", " (map toText (tagSetToList tags))

-- | 現在の 'EditorDraft' を厳格検証し、@PUT /api/documents/{id}@ 用 'UpdateDocumentRequest' を生成する。
validateEditorDraft :: EditorDraft -> Either ValidationError UpdateDocumentRequest
validateEditorDraft currentDraft = do
    validTitle  <- mkDocumentTitle currentDraft.titleInput
    validBody   <- validateDocumentBody currentDraft.body
    validFolder <- parseFolderInput currentDraft.folderInput
    validTags   <- parseTagsInput currentDraft.tagsInput
    Right UpdateDocumentRequest
        { title  = validTitle
        , body   = validBody
        , format = currentDraft.format
        , folder = validFolder
        , tags   = validTags
        }

-- | 保存送信時点のドラフト (@sentDraft@)、サーバ正規化済みドラフト (@savedDraft@)、
-- および応答到着時点の現在ドラフト (@currentDraft@) をフィールド単位で合成する。
-- 保存通信中にユーザーが追記したフィールドは保持し、未変更フィールドのみサーバ正規化値へ揃えることで
-- Model と CodeMirror 6 の乖離や追加入力の無言消失を防ぐ。
mergeDraftAfterSave :: EditorDraft -> EditorDraft -> EditorDraft -> EditorDraft
mergeDraftAfterSave sentDraft savedDraft currentDraft = EditorDraft
    { titleInput  =
        if currentDraft.titleInput == sentDraft.titleInput
            then savedDraft.titleInput
            else currentDraft.titleInput
    , body        =
        if currentDraft.body == sentDraft.body
            then savedDraft.body
            else currentDraft.body
    , format      =
        if currentDraft.format == sentDraft.format
            then savedDraft.format
            else currentDraft.format
    , folderInput =
        if currentDraft.folderInput == sentDraft.folderInput
            then savedDraft.folderInput
            else currentDraft.folderInput
    , tagsInput   =
        if currentDraft.tagsInput == sentDraft.tagsInput
            then savedDraft.tagsInput
            else currentDraft.tagsInput
    }

-- | 保存済み 'Document' と現在の 'EditorDraft' を比較し、未保存変更があるか判定する。
-- ゴミ箱内の文書は読み取り専用のため常に 'False' を返す。
-- タイトル・フォルダ・タグに未検証/不正な入力がある場合も変更あり ('True') とみなす。
isDraftDirty :: Document -> EditorDraft -> Bool
isDraftDirty doc currentDraft
    | isTrashed doc = False
    | otherwise =
        currentDraft.body /= doc.body
            || currentDraft.format /= doc.format
            || mkDocumentTitle currentDraft.titleInput /= Right doc.title
            || parseFolderInput currentDraft.folderInput /= Right doc.folder
            || parseTagsInput currentDraft.tagsInput /= Right doc.tags

-- | 現在選択中の文書に未保存変更があるかどうかを返す。
isCurrentDocumentDirty :: ClientState -> Bool
isCurrentDocumentDirty state = case state.selectedDocument of
    Nothing  -> False
    Just doc -> isDraftDirty doc state.draft

-- | 現在のフィルタ状態から @GET /api/documents@ 用の検証済み 'DocumentListQuery' を構築する。
buildListQueryFromFilter :: FilterState -> Either ValidationError DocumentListQuery
buildListQueryFromFilter flt = do
    mSearch <- mkSearchQuery flt.searchInput
    Right defaultDocumentListQuery
        { status            = flt.status
        , tag               = flt.selectedTag
        , folder            = flt.selectedFolder
        , includeSubfolders = flt.includeSubfolders
        , search            = mSearch
        }

-- | 検索語・タグ・フォルダのいずれかのフィルタが適用されているか判定する。
hasActiveFilters :: FilterState -> Bool
hasActiveFilters flt =
    not (Txt.null (Txt.strip flt.searchInput))
        || isJust flt.selectedTag
        || isJust flt.selectedFolder

-- | 現在のクライアント状態から CodeMirror 6 同期ペイロードを構築する。
buildEditorSyncPayload :: UiMessages -> Bool -> ClientState -> Maybe EditorSyncPayload
buildEditorSyncPayload msgs shouldResetHistory state = case state.selectedDocument of
    Nothing  -> Nothing
    Just doc -> Just EditorSyncPayload
        { docId        = toText doc.id
        , version      = state.editorSyncVersion
        , body         = toText state.draft.body
        , format       = state.draft.format
        , readOnly     = isTrashed doc
        , placeholder  = msgs.editorBodyPlaceholder
        , resetHistory = shouldResetHistory
        }

-- | 指定した ID の拡張を現在の本文へ適用する。
-- 該当拡張が存在しない、またはゴミ箱内の読み取り専用文書を開いている場合は状態を変更しない。
applyExtensionById :: [Extension] -> ExtensionId -> ClientState -> ClientState
applyExtensionById exts targetId state
    | maybe False isTrashed state.selectedDocument = state
    | otherwise = case find hasTargetId exts of
        Nothing  -> state
        Just ext ->
            let nextBody :: DocumentBody
                nextBody = ext.transformBuffer state.draft.body
            in  state
                    { draft             = setDraftBody nextBody state.draft
                    , editorSyncVersion = state.editorSyncVersion + 1
                    , editorStats       = computeTextStats (toText nextBody)
                    }
  where
    hasTargetId :: Extension -> Bool
    hasTargetId ext = ext.metadata.extensionId == targetId

-- | ヘルスチェック応答をクライアント状態へ純粋に反映する。
applyHealthResult ::
       UiMessages
    -> Either ApiClientError HealthStatus
    -> ClientState
    -> ClientState
applyHealthResult _ (Right _health) state =
    state { connectionStatus = ConnOnline }
applyHealthResult msgs (Left err) state =
    let formatted :: Text
        formatted = formatApiClientError msgs err
    in  state
            { connectionStatus = ConnError formatted
            , lastError        = Just formatted
            }

-- | 文書一覧応答をクライアント状態へ純粋に反映する。
-- リクエスト世代番号が最新と一致しない古い応答は無視し、素早いタブ/フィルタ切替での上書きを防ぐ。
applyDocumentsResult ::
       UiMessages
    -> Int
    -> Either ApiClientError DocumentListResponse
    -> ClientState
    -> ClientState
applyDocumentsResult msgs reqSeq result state
    | reqSeq /= state.listRequestSeq = state
    | otherwise = case result of
        Right res ->
            let nextError :: Maybe Text
                nextError = case state.connectionStatus of
                    ConnError{} -> state.lastError
                    _           -> Nothing
            in  state
                    { documentSummaries  = res.documents
                    , totalDocuments     = res.total
                    , isLoadingDocuments = False
                    , lastError          = nextError
                    }
        Left err ->
            state
                { isLoadingDocuments = False
                , lastError          = Just (formatApiClientError msgs err)
                }

-- | タグ一覧応答を反映する。失敗時は一覧表示を阻害しないよう既存タグを維持する。
applyTagsResult :: Either ApiClientError TagListResponse -> ClientState -> ClientState
applyTagsResult (Right res) state = state { availableTags = res.tags }
applyTagsResult (Left _)    state = state

-- | フォルダ一覧応答を反映する。失敗時は一覧表示を阻害しないよう既存フォルダを維持する。
applyFoldersResult :: Either ApiClientError FolderListResponse -> ClientState -> ClientState
applyFoldersResult (Right res) state = state { availableFolders = res.folders }
applyFoldersResult (Left _)    state = state

-- | 起動時の IndexedDB ドラフト一覧読み込み結果をクライアント状態へ純粋に反映する。
-- すでにユーザーが編集開始した現在文書のドラフトは上書きせず保持し、
-- まだ未編集のまま開いている文書と一致するドラフトがあれば復元・警告判定を行う。
applyDraftsLoaded :: UiMessages -> Either Text [StoredDraft] -> ClientState -> ClientState
applyDraftsLoaded _ (Left errMsg) state =
    state { draftSyncState = DraftSyncFailed errMsg }
applyDraftsLoaded msgs (Right drafts) state =
    let loadedMap :: Map DocumentId StoredDraft
        loadedMap = Map.fromList [ (sd.docId, sd) | sd <- drafts ]

        mergedMap :: Map DocumentId StoredDraft
        mergedMap = Map.union state.storedDrafts loadedMap

        baseState :: ClientState
        baseState = state { storedDrafts = mergedMap }
    in  case state.selectedDocument of
            Just doc | not (isDraftDirty doc state.draft) ->
                case reconcileDocumentWithDraft doc (Map.lookup doc.id mergedMap) of
                    NoLocalDraft -> baseState
                    RedundantLocalDraft ->
                        baseState { storedDrafts = Map.delete doc.id mergedMap }
                    RestoreLocalDraft sd ->
                        baseState
                            { draft              = editorDraftFromStored sd
                            , editorSyncVersion  = baseState.editorSyncVersion + 1
                            , editorStats        = computeTextStats (toText sd.body)
                            , editorViewMode     = normalizeViewMode sd.format baseState.editorViewMode
                            , draftSyncState     = DraftSyncSaved sd.savedAt
                            , restoredDraftDocId = Just doc.id
                            , staleDraftPrompt   = Nothing
                            , statusNotice       = Just msgs.noticeDraftRestored
                            }
                    PromptStaleLocalDraft sd ->
                        baseState { staleDraftPrompt = Just sd }
            _ -> baseState

-- | IndexedDB ブリッジからの非同期退避イベントをクライアント状態へ反映する。
applyDraftStorageEvent :: DraftStorageEvent -> ClientState -> ClientState
applyDraftStorageEvent evt state = case evt of
    DraftEvtSaved docId ts ->
        let nextStored :: Map DocumentId StoredDraft
            nextStored = Map.adjust (\ sd -> sd { savedAt = ts }) docId state.storedDrafts

            isSelectedDirty :: Bool
            isSelectedDirty =
                fmap (.id) state.selectedDocument == Just docId
                    && isCurrentDocumentDirty state
                    && state.draftSyncState /= DraftSyncIdle
        in  state
                { storedDrafts   = nextStored
                , draftSyncState = if isSelectedDirty then DraftSyncSaved ts else state.draftSyncState
                }
    DraftEvtSaveFailed mDocId errMsg ->
        let matchesSelected :: Bool
            matchesSelected = case mDocId of
                Nothing    -> isJust state.selectedDocument
                Just docId -> fmap (.id) state.selectedDocument == Just docId
        in  if matchesSelected
                then state { draftSyncState = DraftSyncFailed errMsg }
                else state

-- | 単一文書取得応答を反映する。より新しい選択要求が発生済みの古い応答は無視する。
-- ローカル IndexedDB に未保存ドラフトが存在する場合は 'reconcileDocumentWithDraft' に従って復元または警告提示する。
applyDocumentLoaded ::
       UiMessages
    -> DocumentId
    -> Either ApiClientError Document
    -> ClientState
    -> ClientState
applyDocumentLoaded msgs reqId result state
    | state.openingDocumentId /= Just reqId = state
    | otherwise = case result of
        Right doc ->
            case reconcileDocumentWithDraft doc (Map.lookup doc.id state.storedDrafts) of
                RestoreLocalDraft sd ->
                    state
                        { openingDocumentId  = Nothing
                        , autoOpeningDraftId = Nothing
                        , selectedDocument   = Just doc
                        , draft              = editorDraftFromStored sd
                        , draftSyncState     = DraftSyncSaved sd.savedAt
                        , restoredDraftDocId = Just doc.id
                        , staleDraftPrompt   = Nothing
                        , editorSyncVersion  = state.editorSyncVersion + 1
                        , editorStats        = computeTextStats (toText sd.body)
                        , editorViewMode     = normalizeViewMode sd.format state.editorViewMode
                        , canUndo            = False
                        , canRedo            = False
                        , mobilePane         = PaneEditor
                        , lastError          = Nothing
                        , statusNotice       = Just msgs.noticeDraftRestored
                        }
                PromptStaleLocalDraft sd ->
                    state
                        { openingDocumentId  = Nothing
                        , autoOpeningDraftId = Nothing
                        , selectedDocument   = Just doc
                        , draft              = draftFromDocument doc
                        , draftSyncState     = DraftSyncIdle
                        , restoredDraftDocId = Nothing
                        , staleDraftPrompt   = Just sd
                        , editorSyncVersion  = state.editorSyncVersion + 1
                        , editorStats        = computeTextStats (toText doc.body)
                        , editorViewMode     = normalizeViewMode doc.format state.editorViewMode
                        , canUndo            = False
                        , canRedo            = False
                        , mobilePane         = PaneEditor
                        , lastError          = Nothing
                        , statusNotice       = Nothing
                        }
                RedundantLocalDraft ->
                    state
                        { openingDocumentId  = Nothing
                        , autoOpeningDraftId = Nothing
                        , selectedDocument   = Just doc
                        , draft              = draftFromDocument doc
                        , storedDrafts       = Map.delete doc.id state.storedDrafts
                        , draftSyncState     = DraftSyncIdle
                        , restoredDraftDocId = Nothing
                        , staleDraftPrompt   = Nothing
                        , editorSyncVersion  = state.editorSyncVersion + 1
                        , editorStats        = computeTextStats (toText doc.body)
                        , editorViewMode     = normalizeViewMode doc.format state.editorViewMode
                        , canUndo            = False
                        , canRedo            = False
                        , mobilePane         = PaneEditor
                        , lastError          = Nothing
                        , statusNotice       = Nothing
                        }
                NoLocalDraft ->
                    state
                        { openingDocumentId  = Nothing
                        , autoOpeningDraftId = Nothing
                        , selectedDocument   = Just doc
                        , draft              = draftFromDocument doc
                        , draftSyncState     = DraftSyncIdle
                        , restoredDraftDocId = Nothing
                        , staleDraftPrompt   = Nothing
                        , editorSyncVersion  = state.editorSyncVersion + 1
                        , editorStats        = computeTextStats (toText doc.body)
                        , editorViewMode     = normalizeViewMode doc.format state.editorViewMode
                        , canUndo            = False
                        , canRedo            = False
                        , mobilePane         = PaneEditor
                        , lastError          = Nothing
                        , statusNotice       = Nothing
                        }
        Left err ->
            let wasAutoOpen :: Bool
                wasAutoOpen = state.autoOpeningDraftId == Just reqId

                isNotFound :: Bool
                isNotFound = case err of
                    ClientApiError 404 _ -> True
                    _                    -> False
            in  if wasAutoOpen && isNotFound
                    then state
                        { openingDocumentId  = Nothing
                        , autoOpeningDraftId = Nothing
                        , mobilePane         = PaneList
                        , orphanedDraft      = Map.lookup reqId state.storedDrafts
                        , lastError          = Nothing
                        }
                    else state
                        { openingDocumentId  = Nothing
                        , autoOpeningDraftId = Nothing
                        , mobilePane         = if wasAutoOpen then PaneList else state.mobilePane
                        , lastError          = Just (formatApiClientError msgs err)
                        }

-- | 文書新規作成応答をクライアント状態へ純粋に反映する。
applyDocumentCreated ::
       UiMessages
    -> Either ApiClientError Document
    -> ClientState
    -> ClientState
applyDocumentCreated _ (Right doc) state =
    let summary :: DocumentSummary
        summary = toDocumentSummary doc

        remaining :: [DocumentSummary]
        remaining = filter (\ item -> item.id /= doc.id) state.documentSummaries

        nextTotal :: Int
        nextTotal =
            if length remaining == length state.documentSummaries
                then state.totalDocuments + 1
                else state.totalDocuments
    in  state
            { documentSummaries  = summary : remaining
            , totalDocuments     = nextTotal
            , isCreatingDocument = False
            , openingDocumentId  = Nothing
            , autoOpeningDraftId = Nothing
            , selectedDocument   = Just doc
            , draft              = draftFromDocument doc
            , draftSyncState     = DraftSyncIdle
            , restoredDraftDocId = Nothing
            , staleDraftPrompt   = Nothing
            , editorSyncVersion  = state.editorSyncVersion + 1
            , editorStats        = computeTextStats (toText doc.body)
            , editorViewMode     = ModeEdit
            , canUndo            = False
            , canRedo            = False
            , mobilePane         = PaneEditor
            , lastError          = Nothing
            , statusNotice       = Nothing
            }
applyDocumentCreated msgs (Left err) state =
    state
        { isCreatingDocument           = False
        , pendingListReloadAfterCreate = False
        , lastError                    = Just (formatApiClientError msgs err)
        }

-- | 文書保存 (@PUT /api/documents/{id}@) の応答をクライアント状態へ反映する。
-- 保存通信中にユーザーが追記したフィールドは 'mergeDraftAfterSave' で保持し、
-- ゴミ箱タブ表示中の一覧へ稼働中文書が誤混入することも防ぐ。
applyDocumentSaved ::
       UiMessages
    -> DocumentId
    -> Either ApiClientError Document
    -> ClientState
    -> ClientState
applyDocumentSaved msgs savedId result state = case state.savingDocument of
    Just (inFlightId, sentDraft)
        | inFlightId == savedId -> case result of
            Right doc ->
                let summary :: DocumentSummary
                    summary = toDocumentSummary doc

                    nextSummaries :: [DocumentSummary]
                    nextSummaries = case state.filterState.status of
                        StatusTrashed -> state.documentSummaries
                        _             ->
                            if any (\ item -> item.id == doc.id) state.documentSummaries
                                then summary : filter (\ item -> item.id /= doc.id) state.documentSummaries
                                else state.documentSummaries

                    isSameSelected :: Bool
                    isSameSelected = fmap (.id) state.selectedDocument == Just doc.id

                    nextDraft :: EditorDraft
                    nextDraft =
                        if isSameSelected
                            then mergeDraftAfterSave sentDraft (draftFromDocument doc) state.draft
                            else state.draft

                    stillDirty :: Bool
                    stillDirty = isSameSelected && isDraftDirty doc nextDraft

                    nextStoredDrafts :: Map DocumentId StoredDraft
                    nextStoredDrafts =
                        if stillDirty
                            then state.storedDrafts
                            else Map.delete doc.id state.storedDrafts
                    nextDraftSyncState :: DraftSyncState
                    nextDraftSyncState
                        | not isSameSelected = state.draftSyncState
                        | stillDirty         = DraftSyncPending
                        | otherwise          = DraftSyncIdle

                    nextRestoredDocId :: Maybe DocumentId
                    nextRestoredDocId
                        | not isSameSelected = state.restoredDraftDocId
                        | stillDirty         = state.restoredDraftDocId
                        | otherwise          = Nothing

                    nextStalePrompt :: Maybe StoredDraft
                    nextStalePrompt =
                        if isSameSelected then Nothing else state.staleDraftPrompt
                in  state
                        { savingDocument     = Nothing
                        , documentSummaries  = nextSummaries
                        , selectedDocument   = if isSameSelected then Just doc else state.selectedDocument
                        , draft              = nextDraft
                        , storedDrafts       = nextStoredDrafts
                        , draftSyncState     = nextDraftSyncState
                        , restoredDraftDocId = nextRestoredDocId
                        , staleDraftPrompt   = nextStalePrompt
                        , lastError          = Nothing
                        , statusNotice       = Just msgs.noticeSaved
                        }
            Left err ->
                state
                    { savingDocument = Nothing
                    , lastError      = Just (formatApiClientError msgs err)
                    }
    _ -> state

-- | 論理削除 (ゴミ箱移動) の応答をクライアント状態へ反映する。
applyDocumentSoftDeleted ::
       UiMessages
    -> DocumentId
    -> Either ApiClientError Document
    -> ClientState
    -> ClientState
applyDocumentSoftDeleted msgs targetId result state = case result of
    Right doc ->
        let remaining :: [DocumentSummary]
            remaining = filter (\ item -> item.id /= targetId) state.documentSummaries

            nextSummaries :: [DocumentSummary]
            nextSummaries = case state.filterState.status of
                StatusActive  -> remaining
                StatusTrashed -> toDocumentSummary doc : remaining
                StatusAll     -> toDocumentSummary doc : remaining

            nextTotal :: Int
            nextTotal = case state.filterState.status of
                StatusActive -> max 0 (state.totalDocuments - (length state.documentSummaries - length remaining))
                _            -> state.totalDocuments

            wasSelected :: Bool
            wasSelected = fmap (.id) state.selectedDocument == Just targetId
        in  state
                { mutatingDocumentId = Nothing
                , documentSummaries  = nextSummaries
                , totalDocuments     = nextTotal
                , selectedDocument   = if wasSelected then Nothing else state.selectedDocument
                , draft              = if wasSelected then emptyEditorDraft else state.draft
                , storedDrafts       = Map.delete targetId state.storedDrafts
                , draftSyncState     = if wasSelected then DraftSyncIdle else state.draftSyncState
                , restoredDraftDocId = if wasSelected then Nothing else state.restoredDraftDocId
                , staleDraftPrompt   = if wasSelected then Nothing else state.staleDraftPrompt
                , editorStats        = if wasSelected then emptyEditorStats else state.editorStats
                , canUndo            = not wasSelected && state.canUndo
                , canRedo            = not wasSelected && state.canRedo
                , mobilePane         = if wasSelected then PaneList else state.mobilePane
                , lastError          = Nothing
                , statusNotice       = Just msgs.noticeMovedToTrash
                }
    Left err ->
        state
            { mutatingDocumentId = Nothing
            , lastError          = Just (formatApiClientError msgs err)
            }

-- | ゴミ箱からの復元応答をクライアント状態へ反映する。
applyDocumentRestored ::
       UiMessages
    -> DocumentId
    -> Either ApiClientError Document
    -> ClientState
    -> ClientState
applyDocumentRestored msgs targetId result state = case result of
    Right doc ->
        let remaining :: [DocumentSummary]
            remaining = filter (\ item -> item.id /= targetId) state.documentSummaries

            nextSummaries :: [DocumentSummary]
            nextSummaries = case state.filterState.status of
                StatusTrashed -> remaining
                StatusActive  -> toDocumentSummary doc : remaining
                StatusAll     -> toDocumentSummary doc : remaining

            nextTotal :: Int
            nextTotal = case state.filterState.status of
                StatusTrashed -> max 0 (state.totalDocuments - (length state.documentSummaries - length remaining))
                _             -> state.totalDocuments

            wasSelected :: Bool
            wasSelected = fmap (.id) state.selectedDocument == Just targetId
        in  state
                { mutatingDocumentId = Nothing
                , documentSummaries  = nextSummaries
                , totalDocuments     = nextTotal
                , selectedDocument   = if wasSelected then Just doc else state.selectedDocument
                , draft              = if wasSelected then draftFromDocument doc else state.draft
                , editorSyncVersion  = if wasSelected then state.editorSyncVersion + 1 else state.editorSyncVersion
                , lastError          = Nothing
                , statusNotice       = Just msgs.noticeRestored
                }
    Left err ->
        state
            { mutatingDocumentId = Nothing
            , lastError          = Just (formatApiClientError msgs err)
            }

-- | 完全削除応答をクライアント状態へ反映する。
applyDocumentPermanentlyDeleted ::
       UiMessages
    -> DocumentId
    -> Either ApiClientError ()
    -> ClientState
    -> ClientState
applyDocumentPermanentlyDeleted msgs targetId result state = case result of
    Right () ->
        let remaining :: [DocumentSummary]
            remaining = filter (\ item -> item.id /= targetId) state.documentSummaries

            removedCount :: Int
            removedCount = length state.documentSummaries - length remaining

            wasSelected :: Bool
            wasSelected = fmap (.id) state.selectedDocument == Just targetId
        in  state
                { mutatingDocumentId     = Nothing
                , confirmPermanentDelete = Nothing
                , documentSummaries      = remaining
                , totalDocuments         = max 0 (state.totalDocuments - removedCount)
                , selectedDocument       = if wasSelected then Nothing else state.selectedDocument
                , draft                  = if wasSelected then emptyEditorDraft else state.draft
                , storedDrafts           = Map.delete targetId state.storedDrafts
                , draftSyncState         = if wasSelected then DraftSyncIdle else state.draftSyncState
                , restoredDraftDocId     = if wasSelected then Nothing else state.restoredDraftDocId
                , staleDraftPrompt       = if wasSelected then Nothing else state.staleDraftPrompt
                , editorStats            = if wasSelected then emptyEditorStats else state.editorStats
                , canUndo                = not wasSelected && state.canUndo
                , canRedo                = not wasSelected && state.canRedo
                , mobilePane             = if wasSelected then PaneList else state.mobilePane
                , lastError              = Nothing
                , statusNotice           = Just msgs.noticePermanentlyDeleted
                }
    Left err ->
        state
            { mutatingDocumentId     = Nothing
            , confirmPermanentDelete = Nothing
            , lastError              = Just (formatApiClientError msgs err)
            }

-- | CodeMirror 6 から受信したイベントを状態へ純粋に反映する (保存ショートカット実行は MVU 側で処理)。
applyEditorEvent :: EditorEvent -> ClientState -> ClientState
applyEditorEvent evt state = case evt of
    EvtBodyChanged nextBodyText stats ->
        case state.selectedDocument of
            Just doc | not (isTrashed doc) ->
                state
                    { draft        = setDraftBody (DocumentBody nextBodyText) state.draft
                    , editorStats  = stats
                    , statusNotice = Nothing
                    }
            _ ->
                state { editorStats = stats }
    EvtCursorMoved stats ->
        state { editorStats = stats }
    EvtSaveShortcut ->
        state
    EvtOpenSearch showRep ->
        state
            { searchPanel = state.searchPanel
                { isOpen      = True
                , showReplace = showRep || state.searchPanel.showReplace
                }
            }
    EvtSearchStatus status ->
        state
            { searchPanel = setSearchStatus status state.searchPanel
            }
    EvtHistoryState undoable redoable ->
        state
            { canUndo = undoable
            , canRedo = redoable
            }

-- | 表示中のエラーメッセージをクリアする。
dismissError :: ClientState -> ClientState
dismissError state = state { lastError = Nothing }

-- | 表示中の通知メッセージをクリアする。
dismissNotice :: ClientState -> ClientState
dismissNotice state = state { statusNotice = Nothing }

-- | Miso の MVU 更新関数。純粋な状態更新・API 呼び出し・CodeMirror 6 コマンド送出を調停する。
updateClient :: ClientEnv -> ClientAction -> Effect () () ClientState ClientAction
updateClient env action = case action of
    Initialize -> do
        triggerRefreshAll
        io (DraftsLoaded <$> env.draftStorage.listStoredDrafts)
    RefreshAll -> triggerRefreshAll
    HealthChecked eHealth ->
        modify (applyHealthResult env.messages eHealth)
    DocumentsLoaded reqSeq eDocs ->
        modify (applyDocumentsResult env.messages reqSeq eDocs)
    TagsLoaded eTags ->
        modify (applyTagsResult eTags)
    FoldersLoaded eFolders ->
        modify (applyFoldersResult eFolders)
    RequestStatusFilter nextStatus -> do
        st <- get
        if st.filterState.status == nextStatus
            then pure ()
            else if isCurrentDocumentDirty st
                then modify (\ s -> s { pendingNavigation = Just (NavSwitchStatus nextStatus) })
                else executeNavigation (NavSwitchStatus nextStatus)
    UpdateListSearchInput txt ->
        modify (\ st -> st { filterState = st.filterState { searchInput = txt } })
    SubmitListSearch ->
        reloadDocumentsForCurrentFilter
    SelectFilterTag mTag -> do
        modify (\ st -> st { filterState = st.filterState { selectedTag = mTag } })
        reloadDocumentsForCurrentFilter
    SelectFilterFolder mFolder -> do
        modify (\ st -> st { filterState = st.filterState { selectedFolder = mFolder } })
        reloadDocumentsForCurrentFilter
    ToggleIncludeSubfolders inc -> do
        modify (\ st -> st { filterState = setFilterIncludeSubfolders inc st.filterState })
        reloadDocumentsForCurrentFilter
    ClearFilters -> do
        modify $ \ st -> st
            { filterState = setFilterStatus st.filterState.status defaultFilterState
            , lastError   = Nothing
            }
        reloadDocumentsForCurrentFilter
    RequestOpenDocument docId -> do
        st <- get
        if fmap (.id) st.selectedDocument == Just docId
            then modify $ \ s -> s
                { openingDocumentId = Nothing
                , mobilePane        = PaneEditor
                }
            else if isCurrentDocumentDirty st
                then modify (\ s -> s { pendingNavigation = Just (NavOpenDocument docId) })
                else executeNavigation (NavOpenDocument docId)
    DocumentLoaded docId eDoc -> do
        stBefore <- get
        let shouldCleanRedundant :: Bool
            shouldCleanRedundant = case eDoc of
                Right doc
                    | stBefore.openingDocumentId == Just docId ->
                        reconcileDocumentWithDraft doc (Map.lookup doc.id stBefore.storedDrafts) == RedundantLocalDraft
                _ -> False
        modify (applyDocumentLoaded env.messages docId eDoc)
        when shouldCleanRedundant (io_ (env.draftStorage.deleteStoredDraft docId))
        syncEditorFromCurrentState False
    RequestCreateDocument -> do
        st <- get
        if st.isCreatingDocument || st.isImportingDocument
            then pure ()
            else if isCurrentDocumentDirty st
                then modify (\ s -> s { pendingNavigation = Just NavCreateDocument })
                else executeNavigation NavCreateDocument
    DocumentCreated eDoc -> do
        stBefore <- get
        let shouldReloadList :: Bool
            shouldReloadList = stBefore.pendingListReloadAfterCreate
        modify $ \ s ->
            let base :: ClientState
                base = (applyDocumentCreated env.messages eDoc s)
                    { pendingListReloadAfterCreate = False
                    }
            in  case eDoc of
                    Right _ | shouldReloadList -> base { filterState = defaultFilterState }
                    _                          -> base
        syncEditorFromCurrentState False
        case eDoc of
            Right _ -> do
                when shouldReloadList reloadDocumentsForCurrentFilter
                io (TagsLoaded <$> listTags env.apiClient)
                io (FoldersLoaded <$> listFolders env.apiClient)
                io_ (env.sendEditorCmd CmdFocus)
            Left _ -> pure ()
    ReturnToList -> do
        st <- get
        if isCurrentDocumentDirty st
            then modify (\ s -> s { pendingNavigation = Just NavReturnToList })
            else modify (\ s -> s { mobilePane = PaneList })
    ConfirmDiscardNavigation -> do
        st <- get
        let mDiscardDocId :: Maybe DocumentId
            mDiscardDocId = fmap (.id) st.selectedDocument

            shouldRevertDraft :: Bool
            shouldRevertDraft = case st.pendingNavigation of
                Just NavImportDocument       -> True
                Just NavRecoverOrphanedDraft -> True
                _                            -> False
        modify $ \ s ->
            let base :: ClientState
                base = s
                    { pendingNavigation  = Nothing
                    , storedDrafts       = maybe s.storedDrafts (`Map.delete` s.storedDrafts) mDiscardDocId
                    , draftSyncState     = DraftSyncIdle
                    , restoredDraftDocId = Nothing
                    , staleDraftPrompt   = Nothing
                    }
            in  if shouldRevertDraft
                    then
                        let resetDraft :: EditorDraft
                            resetDraft = maybe emptyEditorDraft draftFromDocument s.selectedDocument
                        in  base
                                { draft             = resetDraft
                                , editorSyncVersion = s.editorSyncVersion + 1
                                , editorStats       = computeTextStats (toText resetDraft.body)
                                , canUndo           = False
                                , canRedo           = False
                                }
                    else base
        for_ mDiscardDocId (io_ . env.draftStorage.deleteStoredDraft)
        when shouldRevertDraft (syncEditorFromCurrentState True)
        for_ st.pendingNavigation executeNavigation
    CancelPendingNavigation ->
        modify (\ s -> s { pendingNavigation = Nothing })
    UpdateDraftTitle txt -> do
        modify $ \ st -> st
            { draft        = setDraftTitle txt st.draft
            , statusNotice = Nothing
            }
        syncLocalDraftForCurrentState
    UpdateDraftFormat nextFmt -> do
        modify $ \ st -> st
            { draft          = setDraftFormat nextFmt st.draft
            , editorViewMode = normalizeViewMode nextFmt st.editorViewMode
            , statusNotice   = Nothing
            }
        syncLocalDraftForCurrentState
        syncEditorFromCurrentState False
    UpdateDraftFolder txt -> do
        modify $ \ st -> st
            { draft        = setDraftFolder txt st.draft
            , statusNotice = Nothing
            }
        syncLocalDraftForCurrentState
    UpdateDraftTags txt -> do
        modify $ \ st -> st
            { draft        = setDraftTags txt st.draft
            , statusNotice = Nothing
            }
        syncLocalDraftForCurrentState
    ToggleMetadataPanel ->
        modify (\ st -> st { metadataPanelOpen = not st.metadataPanelOpen })
    SetEditorViewMode mode ->
        modify $ \ st -> st
            { editorViewMode = normalizeViewMode st.draft.format mode
            }
    SaveCurrentDocument ->
        triggerSaveCurrentDocument
    DocumentSaved docId eDoc -> do
        modify (applyDocumentSaved env.messages docId eDoc)
        case eDoc of
            Right doc -> do
                stAfter <- get
                if fmap (.id) stAfter.selectedDocument == Just doc.id && isDraftDirty doc stAfter.draft
                    then syncLocalDraftForCurrentState
                    else io_ (env.draftStorage.deleteStoredDraft doc.id)
                io (TagsLoaded <$> listTags env.apiClient)
                io (FoldersLoaded <$> listFolders env.apiClient)
            Left _ -> pure ()
    EditorUndoClicked ->
        io_ (env.sendEditorCmd CmdUndo)
    EditorRedoClicked ->
        io_ (env.sendEditorCmd CmdRedo)
    EditorEventReceived evt -> case evt of
        EvtSaveShortcut -> triggerSaveCurrentDocument
        _ -> do
            modify (applyEditorEvent evt)
            case evt of
                EvtBodyChanged{} -> syncLocalDraftForCurrentState
                EvtOpenSearch{}  -> dispatchCurrentSearch
                _                -> pure ()
    ToggleSearchPanel openReplace -> do
        st <- get
        if st.searchPanel.isOpen && not openReplace
            then do
                modify (\ s -> s { searchPanel = s.searchPanel { isOpen = False } })
                io_ (env.sendEditorCmd CmdClearSearch)
            else do
                modify $ \ s -> s
                    { searchPanel = s.searchPanel
                        { isOpen      = True
                        , showReplace = openReplace || s.searchPanel.showReplace
                        }
                    }
                dispatchCurrentSearch
    ToggleReplaceRow ->
        modify $ \ st -> st
            { searchPanel = st.searchPanel { showReplace = not st.searchPanel.showReplace }
            }
    CloseSearchPanel -> do
        modify (\ st -> st { searchPanel = st.searchPanel { isOpen = False } })
        io_ (env.sendEditorCmd CmdClearSearch)
    UpdateSearchQuery txt -> do
        modify (\ st -> st { searchPanel = setSearchQueryText txt st.searchPanel })
        dispatchCurrentSearch
    UpdateReplaceText txt -> do
        modify (\ st -> st { searchPanel = setSearchReplaceText txt st.searchPanel })
        dispatchCurrentSearch
    ToggleSearchCaseSensitive -> do
        modify $ \ st -> st
            { searchPanel = toggleSearchCase st.searchPanel
            }
        dispatchCurrentSearch
    ToggleSearchRegex -> do
        modify $ \ st -> st
            { searchPanel = toggleSearchRegexMode st.searchPanel
            }
        dispatchCurrentSearch
    FindNextMatch ->
        io_ (env.sendEditorCmd CmdFindNext)
    FindPrevMatch ->
        io_ (env.sendEditorCmd CmdFindPrevious)
    ReplaceCurrentMatch ->
        io_ (env.sendEditorCmd CmdReplaceNext)
    ReplaceAllMatches ->
        io_ (env.sendEditorCmd CmdReplaceAll)
    SoftDeleteDocument docId -> do
        st <- get
        if isJust st.mutatingDocumentId || isJust st.savingDocument
            then pure ()
            else do
                modify $ \ s -> s
                    { mutatingDocumentId = Just docId
                    , lastError          = Nothing
                    }
                io (DocumentSoftDeleted docId <$> softDeleteDocument env.apiClient docId)
    DocumentSoftDeleted docId eDoc -> do
        stBefore <- get
        let wasSelected :: Bool
            wasSelected = fmap (.id) stBefore.selectedDocument == Just docId
        modify (applyDocumentSoftDeleted env.messages docId eDoc)
        case eDoc of
            Right _ -> do
                io_ (env.draftStorage.deleteStoredDraft docId)
                when wasSelected (io_ (env.sendEditorCmd CmdResetEditor))
                io (TagsLoaded <$> listTags env.apiClient)
                io (FoldersLoaded <$> listFolders env.apiClient)
            Left _ -> pure ()
    RestoreDocument docId -> do
        st <- get
        if isJust st.mutatingDocumentId
            then pure ()
            else do
                modify $ \ s -> s
                    { mutatingDocumentId = Just docId
                    , lastError          = Nothing
                    }
                io (DocumentRestored docId <$> restoreDocument env.apiClient docId)
    DocumentRestored docId eDoc -> do
        modify (applyDocumentRestored env.messages docId eDoc)
        syncEditorFromCurrentState False
        case eDoc of
            Right _ -> do
                io (TagsLoaded <$> listTags env.apiClient)
                io (FoldersLoaded <$> listFolders env.apiClient)
            Left _ -> pure ()
    RequestPermanentDelete docId ->
        modify (\ st -> st { confirmPermanentDelete = Just docId })
    CancelPermanentDelete ->
        modify (\ st -> st { confirmPermanentDelete = Nothing })
    ConfirmPermanentDelete docId -> do
        st <- get
        if isJust st.mutatingDocumentId
            then pure ()
            else do
                modify $ \ s -> s
                    { mutatingDocumentId = Just docId
                    , lastError          = Nothing
                    }
                io (DocumentPermanentlyDeleted docId <$> permanentDeleteDocument env.apiClient docId)
    DocumentPermanentlyDeleted docId eUnit -> do
        stBefore <- get
        let wasSelected :: Bool
            wasSelected = fmap (.id) stBefore.selectedDocument == Just docId
        modify (applyDocumentPermanentlyDeleted env.messages docId eUnit)
        case eUnit of
            Right () -> do
                io_ (env.draftStorage.deleteStoredDraft docId)
                when wasSelected (io_ (env.sendEditorCmd CmdResetEditor))
            Left _ -> pure ()
    ApplyExtension extId -> do
        st <- get
        let hasExt :: Bool
            hasExt = any (\ ext -> ext.metadata.extensionId == extId) env.extensions
        case st.selectedDocument of
            Just doc | not (isTrashed doc) && hasExt -> do
                modify $ \ s ->
                    (applyExtensionById env.extensions extId s)
                        { statusNotice = Just env.messages.noticeExtensionApplied
                        }
                syncLocalDraftForCurrentState
                syncEditorFromCurrentState False
            _ -> pure ()
    DraftsLoaded eDrafts -> do
        stBefore <- get
        modify (applyDraftsLoaded env.messages eDrafts)
        case eDrafts of
            Left _ -> pure ()
            Right drafts -> do
                let loadedMap :: Map DocumentId StoredDraft
                    loadedMap = Map.fromList [ (sd.docId, sd) | sd <- drafts ]
                case stBefore.selectedDocument of
                    Just doc | not (isDraftDirty doc stBefore.draft) ->
                        case reconcileDocumentWithDraft doc (Map.lookup doc.id (Map.union stBefore.storedDrafts loadedMap)) of
                            RedundantLocalDraft ->
                                io_ (env.draftStorage.deleteStoredDraft doc.id)
                            RestoreLocalDraft{} ->
                                syncEditorFromCurrentState False
                            _ -> pure ()
                    Nothing
                        | isNothing stBefore.openingDocumentId
                            && not stBefore.isCreatingDocument
                            && not stBefore.isImportingDocument ->
                            for_ (latestStoredDraft drafts) $ \ latest -> do
                                modify $ \ s -> s
                                    { openingDocumentId  = Just latest.docId
                                    , autoOpeningDraftId = Just latest.docId
                                    , mobilePane         = PaneEditor
                                    }
                                io (DocumentLoaded latest.docId <$> getDocument env.apiClient latest.docId)
                    _ -> pure ()
    DraftStorageEventReceived evt ->
        modify (applyDraftStorageEvent evt)
    ApplyStaleDraft -> do
        st <- get
        case (st.selectedDocument, st.staleDraftPrompt) of
            (Just doc, Just sd)
                | not (isTrashed doc)
                    && sd.docId == doc.id
                    && isNothing st.savingDocument -> do
                    let restoredDraft :: EditorDraft
                        restoredDraft = editorDraftFromStored sd
                    modify $ \ s -> s
                        { draft              = restoredDraft
                        , editorSyncVersion  = s.editorSyncVersion + 1
                        , editorStats        = computeTextStats (toText restoredDraft.body)
                        , editorViewMode     = normalizeViewMode restoredDraft.format s.editorViewMode
                        , canUndo            = False
                        , canRedo            = False
                        , restoredDraftDocId = Just doc.id
                        , staleDraftPrompt   = Nothing
                        , statusNotice       = Just env.messages.noticeDraftRestored
                        }
                    syncLocalDraftForCurrentState
                    syncEditorFromCurrentState True
            _ -> pure ()
    DiscardCurrentDraft -> do
        st <- get
        case st.selectedDocument of
            Nothing -> pure ()
            Just doc
                | isJust st.savingDocument -> pure ()
                | otherwise -> do
                    let isOnlyStalePrompt :: Bool
                        isOnlyStalePrompt = isJust st.staleDraftPrompt
                    if isOnlyStalePrompt
                        then modify $ \ s -> s
                            { storedDrafts     = Map.delete doc.id s.storedDrafts
                            , staleDraftPrompt = Nothing
                            , statusNotice     = Just env.messages.noticeDraftDiscarded
                            }
                        else do
                            let serverDraft :: EditorDraft
                                serverDraft = draftFromDocument doc
                            modify $ \ s -> s
                                { draft              = serverDraft
                                , storedDrafts       = Map.delete doc.id s.storedDrafts
                                , draftSyncState     = DraftSyncIdle
                                , restoredDraftDocId = Nothing
                                , staleDraftPrompt   = Nothing
                                , editorSyncVersion  = s.editorSyncVersion + 1
                                , editorStats        = computeTextStats (toText serverDraft.body)
                                , editorViewMode     = normalizeViewMode serverDraft.format s.editorViewMode
                                , canUndo            = False
                                , canRedo            = False
                                , statusNotice       = Just env.messages.noticeDraftDiscarded
                                }
                            syncEditorFromCurrentState True
                    io_ (env.draftStorage.deleteStoredDraft doc.id)
    RecoverOrphanedDraft -> do
        st <- get
        case st.orphanedDraft of
            Nothing -> pure ()
            Just _
                | st.isCreatingDocument || st.isImportingDocument -> pure ()
                | isCurrentDocumentDirty st ->
                    modify (\ s -> s { pendingNavigation = Just NavRecoverOrphanedDraft })
                | otherwise ->
                    executeNavigation NavRecoverOrphanedDraft
    OrphanDraftRecovered oldDocId eDoc -> do
        stBefore <- get
        let shouldReloadList :: Bool
            shouldReloadList = stBefore.pendingListReloadAfterCreate
        modify $ \ s ->
            let base :: ClientState
                base = (applyDocumentCreated env.messages eDoc s)
                    { pendingListReloadAfterCreate = False
                    }
            in  case eDoc of
                    Right _ -> base
                        { orphanedDraft = Nothing
                        , storedDrafts  = Map.delete oldDocId base.storedDrafts
                        , filterState   = if shouldReloadList then defaultFilterState else base.filterState
                        , statusNotice  = Just env.messages.noticeDraftRestored
                        }
                    Left _ -> base
        syncEditorFromCurrentState False
        case eDoc of
            Right _ -> do
                io_ (env.draftStorage.deleteStoredDraft oldDocId)
                when shouldReloadList reloadDocumentsForCurrentFilter
                io (TagsLoaded <$> listTags env.apiClient)
                io (FoldersLoaded <$> listFolders env.apiClient)
                io_ (env.sendEditorCmd CmdFocus)
            Left _ -> pure ()
    DiscardOrphanedDraft -> do
        st <- get
        for_ st.orphanedDraft $ \ sd -> do
            modify $ \ s -> s
                { orphanedDraft = Nothing
                , storedDrafts  = Map.delete sd.docId s.storedDrafts
                , statusNotice  = Just env.messages.noticeDraftDiscarded
                }
            io_ (env.draftStorage.deleteStoredDraft sd.docId)
    ExportCurrentDocument -> do
        st <- get
        case st.selectedDocument of
            Nothing -> pure ()
            Just _  -> do
                let spec :: ExportSpec
                    spec = buildExportSpec st.draft.titleInput st.draft.body st.draft.format
                io (ExportFinished spec.fileName <$> env.fileTransfer.exportFile spec)
    ExportFinished fileName eRes -> case eRes of
        Right () ->
            modify $ \ s -> s
                { lastError    = Nothing
                , statusNotice = Just (env.messages.noticeExportedPrefix <> fileName)
                }
        Left errMsg ->
            modify $ \ s -> s
                { lastError = Just (env.messages.errExportFailedPrefix <> " (" <> errMsg <> ")")
                }
    RequestImportDocument -> do
        st <- get
        if st.isImportingDocument || st.isCreatingDocument
            then pure ()
            else if isCurrentDocumentDirty st
                then modify (\ s -> s { pendingNavigation = Just NavImportDocument })
                else executeNavigation NavImportDocument
    ImportFilePicked pickRes -> case pickRes of
        FilePickCancelled -> pure ()
        FilePickError err ->
            modify (\ s -> s { lastError = Just (formatFileImportError env.messages err) })
        FilePickSelected imported -> do
            st <- get
            if st.isImportingDocument || st.isCreatingDocument
                then pure ()
                else do
                    let fromTrash :: Bool
                        fromTrash = st.filterState.status == StatusTrashed

                        targetFolder :: Maybe FolderPath
                        targetFolder = if fromTrash then Nothing else st.filterState.selectedFolder

                        targetTags :: TagSet
                        targetTags =
                            if fromTrash
                                then emptyTagSet
                                else case st.filterState.selectedTag of
                                    Nothing -> emptyTagSet
                                    Just t  -> fromRight emptyTagSet (mkTagSetFromTexts [toText t])
                    case prepareImportedDocumentRequest imported targetFolder targetTags of
                        Left err ->
                            modify (\ s -> s { lastError = Just (formatFileImportError env.messages err) })
                        Right createReq -> do
                            modify $ \ s -> s
                                { isImportingDocument          = True
                                , pendingListReloadAfterImport = fromTrash
                                , lastError                    = Nothing
                                , statusNotice                 = Nothing
                                }
                            io (DocumentImported imported.fileName <$> createDocument env.apiClient createReq)
    DocumentImported fileName eDoc -> do
        stBefore <- get
        let shouldReloadList :: Bool
            shouldReloadList = stBefore.pendingListReloadAfterImport
        modify $ \ s ->
            let base :: ClientState
                base = (applyDocumentCreated env.messages eDoc s)
                    { isImportingDocument          = False
                    , pendingListReloadAfterImport = False
                    }
            in  case eDoc of
                    Right _ -> base
                        { filterState  = if shouldReloadList then defaultFilterState else base.filterState
                        , statusNotice = Just (env.messages.noticeImportedPrefix <> fileName)
                        }
                    Left _ -> base
        syncEditorFromCurrentState False
        case eDoc of
            Right _ -> do
                when shouldReloadList reloadDocumentsForCurrentFilter
                io (TagsLoaded <$> listTags env.apiClient)
                io (FoldersLoaded <$> listFolders env.apiClient)
                io_ (env.sendEditorCmd CmdFocus)
            Left _ -> pure ()
    DismissError ->
        modify dismissError
    DismissNotice ->
        modify dismissNotice
  where
    triggerRefreshAll :: Effect () () ClientState ClientAction
    triggerRefreshAll = do
        st <- get
        case buildListQueryFromFilter st.filterState of
            Left vErr ->
                modify (\ s -> s { lastError = Just (formatValidationError env.messages vErr) })
            Right listQuery -> do
                let nextSeq :: Int
                    nextSeq = st.listRequestSeq + 1
                modify $ \ s -> s
                    { connectionStatus   = ConnChecking
                    , listRequestSeq     = nextSeq
                    , isLoadingDocuments = True
                    , lastError          = Nothing
                    }
                io (HealthChecked <$> fetchHealth env.apiClient)
                io (DocumentsLoaded nextSeq <$> listDocuments env.apiClient listQuery)
                io (TagsLoaded <$> listTags env.apiClient)
                io (FoldersLoaded <$> listFolders env.apiClient)

    reloadDocumentsForCurrentFilter :: Effect () () ClientState ClientAction
    reloadDocumentsForCurrentFilter = do
        st <- get
        case buildListQueryFromFilter st.filterState of
            Left vErr ->
                modify (\ s -> s { lastError = Just (formatValidationError env.messages vErr) })
            Right listQuery -> do
                let nextSeq :: Int
                    nextSeq = st.listRequestSeq + 1
                modify $ \ s -> s
                    { listRequestSeq     = nextSeq
                    , isLoadingDocuments = True
                    , lastError          = Nothing
                    }
                io (DocumentsLoaded nextSeq <$> listDocuments env.apiClient listQuery)

    executeNavigation :: PendingNavigation -> Effect () () ClientState ClientAction
    executeNavigation nav = case nav of
        NavOpenDocument docId -> do
            modify $ \ s -> s
                { openingDocumentId  = Just docId
                , autoOpeningDraftId = Nothing
                , mobilePane         = PaneEditor
                , lastError          = Nothing
                }
            io (DocumentLoaded docId <$> getDocument env.apiClient docId)
        NavCreateDocument -> do
            st <- get
            let fromTrash :: Bool
                fromTrash = st.filterState.status == StatusTrashed

                initialReq :: CreateDocumentRequest
                initialReq = buildCreateRequestFromFilter st.filterState
            modify $ \ s -> s
                { isCreatingDocument           = True
                , pendingListReloadAfterCreate = fromTrash
                , lastError                    = Nothing
                , statusNotice                 = Nothing
                }
            io (DocumentCreated <$> createDocument env.apiClient initialReq)
        NavSwitchStatus nextStatus -> do
            modify $ \ s -> s
                { filterState        = setFilterStatus nextStatus s.filterState
                , openingDocumentId  = Nothing
                , autoOpeningDraftId = Nothing
                , selectedDocument   = Nothing
                , draft              = emptyEditorDraft
                , draftSyncState     = DraftSyncIdle
                , restoredDraftDocId = Nothing
                , staleDraftPrompt   = Nothing
                , editorStats        = emptyEditorStats
                , canUndo            = False
                , canRedo            = False
                , mobilePane         = PaneList
                , statusNotice       = Nothing
                }
            io_ (env.sendEditorCmd CmdResetEditor)
            reloadDocumentsForCurrentFilter
        NavReturnToList -> do
            modify $ \ s ->
                let resetDraft :: EditorDraft
                    resetDraft = maybe emptyEditorDraft draftFromDocument s.selectedDocument
                in  s
                        { draft              = resetDraft
                        , draftSyncState     = DraftSyncIdle
                        , restoredDraftDocId = Nothing
                        , staleDraftPrompt   = Nothing
                        , editorSyncVersion  = s.editorSyncVersion + 1
                        , editorStats        = computeTextStats (toText resetDraft.body)
                        , canUndo            = False
                        , canRedo            = False
                        , mobilePane         = PaneList
                        , statusNotice       = Nothing
                        }
            syncEditorFromCurrentState True
        NavImportDocument ->
            io (ImportFilePicked <$> env.fileTransfer.pickImportFile)
        NavRecoverOrphanedDraft -> do
            st <- get
            case st.orphanedDraft of
                Nothing -> pure ()
                Just sd
                    | st.isCreatingDocument || st.isImportingDocument -> pure ()
                    | otherwise -> case buildRecoveryRequestFromStoredDraft sd of
                        Left vErr ->
                            modify (\ s -> s { lastError = Just (formatValidationError env.messages vErr) })
                        Right createReq -> do
                            let fromTrash :: Bool
                                fromTrash = st.filterState.status == StatusTrashed
                            modify $ \ s -> s
                                { isCreatingDocument           = True
                                , pendingListReloadAfterCreate = fromTrash
                                , lastError                    = Nothing
                                , statusNotice                 = Nothing
                                }
                            io (OrphanDraftRecovered sd.docId <$> createDocument env.apiClient createReq)

    syncLocalDraftForCurrentState :: Effect () () ClientState ClientAction
    syncLocalDraftForCurrentState = do
        st <- get
        case st.selectedDocument of
            Just doc | not (isTrashed doc) ->
                if isDraftDirty doc st.draft
                    then do
                        let provisional :: StoredDraft
                            provisional = toStoredDraft doc st.draft doc.updatedAt
                        modify $ \ s -> s
                            { storedDrafts     = Map.insert doc.id provisional s.storedDrafts
                            , draftSyncState   = DraftSyncPending
                            , staleDraftPrompt = Nothing
                            }
                        io_ $ do
                            now <- env.getCurrentUtcTime
                            env.draftStorage.scheduleSaveDraft (toStoredDraft doc st.draft now)
                    else do
                        modify $ \ s -> s
                            { storedDrafts       = Map.delete doc.id s.storedDrafts
                            , draftSyncState     = DraftSyncIdle
                            , restoredDraftDocId = Nothing
                            }
                        io_ (env.draftStorage.deleteStoredDraft doc.id)
            _ -> pure ()

    triggerSaveCurrentDocument :: Effect () () ClientState ClientAction
    triggerSaveCurrentDocument = do
        st <- get
        case st.selectedDocument of
            Nothing -> pure ()
            Just doc
                | isTrashed doc -> pure ()
                | isJust st.savingDocument -> pure ()
                | otherwise ->
                    case validateEditorDraft st.draft of
                        Left vErr ->
                            modify (\ s -> s { lastError = Just (formatValidationError env.messages vErr) })
                        Right updateReq -> do
                            modify $ \ s -> s
                                { savingDocument = Just (doc.id, st.draft)
                                , lastError      = Nothing
                                , statusNotice   = Nothing
                                }
                            io (DocumentSaved doc.id <$> updateDocument env.apiClient doc.id updateReq)

    syncEditorFromCurrentState :: Bool -> Effect () () ClientState ClientAction
    syncEditorFromCurrentState shouldResetHistory = do
        st <- get
        case buildEditorSyncPayload env.messages shouldResetHistory st of
            Nothing      -> pure ()
            Just payload -> io_ (env.sendEditorCmd (CmdSyncDocument payload))

    dispatchCurrentSearch :: Effect () () ClientState ClientAction
    dispatchCurrentSearch = do
        st <- get
        let sp :: SearchPanelState
            sp = st.searchPanel
        if sp.isOpen
            then io_ $ env.sendEditorCmd $ CmdApplySearch EditorSearchParams
                { query         = sp.query
                , replacement   = sp.replacement
                , caseSensitive = sp.caseSensitive
                , useRegex      = sp.useRegex
                }
            else io_ (env.sendEditorCmd CmdClearSearch)

buildCreateRequestFromFilter :: FilterState -> CreateDocumentRequest
buildCreateRequestFromFilter flt
    | flt.status == StatusTrashed = defaultCreateDocumentRequest
    | otherwise =
        let initialTags :: TagSet
            initialTags = case flt.selectedTag of
                Nothing -> emptyTagSet
                Just t  -> fromRight emptyTagSet (mkTagSetFromTexts [toText t])
        in  CreateDocumentRequest
                { title  = defaultCreateDocumentRequest.title
                , body   = defaultCreateDocumentRequest.body
                , format = defaultCreateDocumentRequest.format
                , folder = flt.selectedFolder
                , tags   = initialTags
                }

setDraftTitle :: Text -> EditorDraft -> EditorDraft
setDraftTitle nextTitle d = EditorDraft
    { titleInput  = nextTitle
    , body        = d.body
    , format      = d.format
    , folderInput = d.folderInput
    , tagsInput   = d.tagsInput
    }

setDraftBody :: DocumentBody -> EditorDraft -> EditorDraft
setDraftBody nextBody d = EditorDraft
    { titleInput  = d.titleInput
    , body        = nextBody
    , format      = d.format
    , folderInput = d.folderInput
    , tagsInput   = d.tagsInput
    }

setDraftFormat :: DocumentFormat -> EditorDraft -> EditorDraft
setDraftFormat nextFmt d = EditorDraft
    { titleInput  = d.titleInput
    , body        = d.body
    , format      = nextFmt
    , folderInput = d.folderInput
    , tagsInput   = d.tagsInput
    }

setDraftFolder :: Text -> EditorDraft -> EditorDraft
setDraftFolder nextFolder d = EditorDraft
    { titleInput  = d.titleInput
    , body        = d.body
    , format      = d.format
    , folderInput = nextFolder
    , tagsInput   = d.tagsInput
    }

setDraftTags :: Text -> EditorDraft -> EditorDraft
setDraftTags nextTags d = EditorDraft
    { titleInput  = d.titleInput
    , body        = d.body
    , format      = d.format
    , folderInput = d.folderInput
    , tagsInput   = nextTags
    }

setFilterStatus :: DocumentStatusFilter -> FilterState -> FilterState
setFilterStatus nextStatus flt = FilterState
    { status            = nextStatus
    , searchInput       = flt.searchInput
    , selectedTag       = flt.selectedTag
    , selectedFolder    = flt.selectedFolder
    , includeSubfolders = flt.includeSubfolders
    }

setFilterIncludeSubfolders :: Bool -> FilterState -> FilterState
setFilterIncludeSubfolders inc flt = FilterState
    { status            = flt.status
    , searchInput       = flt.searchInput
    , selectedTag       = flt.selectedTag
    , selectedFolder    = flt.selectedFolder
    , includeSubfolders = inc
    }

setSearchQueryText :: Text -> SearchPanelState -> SearchPanelState
setSearchQueryText nextQuery sp = SearchPanelState
    { isOpen        = sp.isOpen
    , showReplace   = sp.showReplace
    , query         = nextQuery
    , replacement   = sp.replacement
    , caseSensitive = sp.caseSensitive
    , useRegex      = sp.useRegex
    , matchCount    = sp.matchCount
    , activeMatch   = sp.activeMatch
    , regexError    = sp.regexError
    }

setSearchReplaceText :: Text -> SearchPanelState -> SearchPanelState
setSearchReplaceText nextReplace sp = SearchPanelState
    { isOpen        = sp.isOpen
    , showReplace   = sp.showReplace
    , query         = sp.query
    , replacement   = nextReplace
    , caseSensitive = sp.caseSensitive
    , useRegex      = sp.useRegex
    , matchCount    = sp.matchCount
    , activeMatch   = sp.activeMatch
    , regexError    = sp.regexError
    }

toggleSearchCase :: SearchPanelState -> SearchPanelState
toggleSearchCase sp = SearchPanelState
    { isOpen        = sp.isOpen
    , showReplace   = sp.showReplace
    , query         = sp.query
    , replacement   = sp.replacement
    , caseSensitive = not sp.caseSensitive
    , useRegex      = sp.useRegex
    , matchCount    = sp.matchCount
    , activeMatch   = sp.activeMatch
    , regexError    = sp.regexError
    }

toggleSearchRegexMode :: SearchPanelState -> SearchPanelState
toggleSearchRegexMode sp = SearchPanelState
    { isOpen        = sp.isOpen
    , showReplace   = sp.showReplace
    , query         = sp.query
    , replacement   = sp.replacement
    , caseSensitive = sp.caseSensitive
    , useRegex      = not sp.useRegex
    , matchCount    = sp.matchCount
    , activeMatch   = sp.activeMatch
    , regexError    = sp.regexError
    }

setSearchStatus :: EditorSearchStatus -> SearchPanelState -> SearchPanelState
setSearchStatus st sp = SearchPanelState
    { isOpen        = sp.isOpen
    , showReplace   = sp.showReplace
    , query         = sp.query
    , replacement   = sp.replacement
    , caseSensitive = sp.caseSensitive
    , useRegex      = sp.useRegex
    , matchCount    = st.matchCount
    , activeMatch   = st.activeMatch
    , regexError    = st.regexError
    }

normalizeViewMode :: DocumentFormat -> EditorViewMode -> EditorViewMode
normalizeViewMode FormatPlainText _    = ModeEdit
normalizeViewMode FormatMarkdown  mode = mode

-- | ビュー関数と依存環境を束ねてトップレベル Miso 'App' コンポーネントを構築する。
-- マウント直後に 'Initialize' を発火し、CodeMirror 6 ブリッジおよび IndexedDB ストレージブリッジのイベント購読を登録する。
mkClientComponent ::
       ClientEnv
    -> (UiMessages -> ClientState -> View () () ClientState ClientAction)
    -> App ClientState ClientAction
mkClientComponent env renderView =
    (component (initialClientState env.extensions) (updateClient env) (renderView env.messages))
        { mount = Just Initialize
        , subs  =
            [ editorEventsSub EditorEventReceived
            , draftStorageEventsSub DraftStorageEventReceived
            ]
        }
