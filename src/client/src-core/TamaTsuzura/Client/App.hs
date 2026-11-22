{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.App
    ( -- * ランタイム環境・認証・状態・アクション型
      ClientEnv(..)
    , defaultClientEnv
    , ConnectionStatus(..)
    , AuthFormMode(..)
    , AuthFormState(..)
    , emptyAuthFormState
    , AuthState(..)
    , currentAuthSession
    , currentUserId
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
    , RevisionDetailTab(..)
    , RevisionHistoryState(..)
    , emptyRevisionHistoryState
    , resetRevisionHistory
    , MergeSession(..)
    , ConflictMergeState(..)
    , ExtensionCatalogEntry(..)
    , defaultEnabledExtensions
    , ClientState(..)
    , ClientAction(..)
      -- * 純粋な変換・検証・判定関数
    , parseDisplayNameInput
    , validateLoginForm
    , validateRegisterForm
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
    , resetWorkspaceForUnauthenticated
    , applyAuthSessionChecked
    , applyAuthSubmitted
    , applySessionExpired
    , applyExtensionById
    , applyExtensionCommand
    , applyHealthResult
    , applyDocumentsResult
    , applyTagsResult
    , applyFoldersResult
    , applyDraftsLoaded
    , applyDraftStorageEvent
    , applyOfflineStoreLoaded
    , applyNetworkStatusChanged
    , applyDocumentLoaded
    , applyDocumentCreated
    , applyDocumentSaved
    , applyDocumentSoftDeleted
    , applyDocumentRestored
    , applyDocumentPermanentlyDeleted
    , applyRevisionListLoaded
    , applyRevisionDetailLoaded
    , applyRevisionCompareLoaded
    , applyRevisionRestored
    , applyEditorEvent
    , dismissError
    , dismissNotice
      -- * Miso MVU 更新ロジック
    , updateClient
    , mkClientComponent
    ) where

import           Control.Applicative             ((<|>))
import           Control.Monad                   (unless, when)
import           Control.Monad.State.Class       (get)
import           Data.Either                     (fromRight)
import           Data.Foldable                   (for_)
import           Data.List                       (find)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, isJust, isNothing)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Txt
import           Data.Time.Clock                 (UTCTime, getCurrentTime)
import           Miso                            (App, Component(..), Effect, View, component, io, io_, modify)
import           TamaTsuzura.Client.Api
    ( ApiClient
    , ApiClientError(..)
    , compareDocumentRevisions
    , createDocument
    , fetchHealth
    , getCurrentUser
    , getDocument
    , getDocumentRevision
    , isUnauthorizedError
    , listDocumentRevisions
    , listDocuments
    , listFolders
    , listTags
    , loginUser
    , logoutUser
    , permanentDeleteDocument
    , registerUser
    , restoreDocument
    , restoreDocumentRevision
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
    , formatExtensionError
    , formatFileImportError
    , formatLoginApiError
    , formatRegisterApiError
    , formatValidationError
    )
import           TamaTsuzura.Client.OfflineSync
    ( CachedDocument(..)
    , NetworkStatus(..)
    , OfflineStore(..)
    , PendingSyncSave(..)
    , SyncConflict(..)
    , browserOfflineStore
    , buildConflictRecoveryRequest
    , buildSyncUpdateRequest
    , enqueuePendingSave
    , filterCachedDocuments
    , isConflictError
    , isNetworkError
    , isNotFoundError
    , networkStatusSub
    , nextPendingSyncSave
    )
import           TamaTsuzura.Common.Api
    ( CreateDocumentRequest(..)
    , DocumentListQuery(..)
    , DocumentListResponse(..)
    , DocumentStatusFilter(..)
    , FolderListResponse(..)
    , HealthStatus
    , ListOffset
    , TagListResponse(..)
    , UpdateDocumentRequest(..)
    , defaultCreateDocumentRequest
    , defaultDocumentListQuery
    , mkListOffset
    , mkSearchQuery
    )
import           TamaTsuzura.Common.Auth
    ( AuthSessionResponse(..)
    , DisplayName
    , LoginRequest(..)
    , RegisterRequest(..)
    , UserId
    , UserProfile(..)
    , mkDisplayName
    , mkPassword
    , mkUsername
    , validateNewPassword
    , validateRegisterRequest
    )
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody(..)
    , DocumentFormat(..)
    , DocumentId
    , DocumentSummary(..)
    , DocumentTitle
    , FolderPath
    , RevisionNumber
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
import           TamaTsuzura.Common.Merge
    ( ConflictChoice(..)
    , MergeOutcome(..)
    , mergeThreeWay
    , resolveMergeOutcome
    )
import           TamaTsuzura.Common.Revision
    ( RestoreRevisionRequest(..)
    , RevisionCompareResponse(..)
    , RevisionDetail(..)
    , RevisionListResponse(..)
    , RevisionSummary(..)
    )
import           TamaTsuzura.Common.Text         (ToText(..))
import           TamaTsuzura.Common.Validation   (ValidationError)
import           TamaTsuzura.ExtensionApi
    ( CommandId
    , Extension(..)
    , ExtensionCommandInfo
    , ExtensionContext(..)
    , ExtensionError(..)
    , ExtensionId
    , ExtensionMetadata(..)
    , ExtensionResult(..)
    , runExtensionCommand
    , toExtensionCommandInfo
    )

-- | Miso コンポーネント実行時に外部から注入される不変の依存環境。
-- 関数を含む 'Extension' や 'HttpTransport'・エディタコマンド送出器・ローカルストレージ・
-- ファイル入出力クライアントを 'ClientState' (Model) から切り離すことで、
-- Model の純粋な同値比較 ('Eq') と単体テスト容易性を両立する。
data ClientEnv = ClientEnv
    { apiClient         :: !ApiClient
      -- ^ REST API 通信クライアント
    , extensions        :: ![Extension]
      -- ^ 結線層から注入された同梱拡張一覧
    , messages          :: !UiMessages
      -- ^ UI 表示文言カタログ
    , sendEditorCmd     :: !(EditorCommand -> IO ())
      -- ^ CodeMirror 6 ブリッジへのコマンド送出関数 (テスト時はモック注入可)
    , draftStorage      :: !DraftStorage
      -- ^ IndexedDB ドラフト自動退避クライアント (テスト時はインメモリ注入可)
    , offlineStore      :: !OfflineStore
      -- ^ IndexedDB オフライン文書キャッシュ・未同期保存キュー・競合退避ストア (テスト時はインメモリ注入可)
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
    , offlineStore      = browserOfflineStore
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

-- | 未認証画面で表示する認証フォームのモード。
data AuthFormMode
    = AuthModeLogin
      -- ^ 既存アカウントでのログインフォーム
    | AuthModeRegister
      -- ^ 新規アカウント作成フォーム
    deriving (Eq, Show)

-- | ログイン・新規登録フォームの入力状態。
-- 認証成功時に 'AuthAuthenticated' へ置き換えることで、生パスワード文字列が
-- 認証済みワークスペースのメモリ上に残留しない構造とする。
data AuthFormState = AuthFormState
    { mode                 :: !AuthFormMode
      -- ^ 現在の表示タブ (ログイン / 新規登録)
    , usernameInput        :: !Text
      -- ^ ユーザー名入力値
    , displayNameInput     :: !Text
      -- ^ 任意の表示名入力値 (新規登録時のみ使用)
    , passwordInput        :: !Text
      -- ^ パスワード入力値
    , passwordConfirmInput :: !Text
      -- ^ 確認用パスワード入力値 (新規登録時のみ使用)
    , isSubmitting         :: !Bool
      -- ^ ログインまたは登録リクエスト送信中フラグ
    , formError            :: !(Maybe Text)
      -- ^ フォーム検証または認証 API 失敗時の日本語エラーメッセージ
    } deriving (Eq, Show)

-- | 既定の空ログインフォーム状態。
emptyAuthFormState :: AuthFormState
emptyAuthFormState = AuthFormState
    { mode                 = AuthModeLogin
    , usernameInput        = ""
    , displayNameInput     = ""
    , passwordInput        = ""
    , passwordConfirmInput = ""
    , isSubmitting         = False
    , formError            = Nothing
    }

-- | クライアント全体の認証ライフサイクル状態。
-- 'AuthAuthenticated' にのみ 'AuthSessionResponse' ('UserProfile' + 'CsrfToken') を保持させることで、
-- 未認証状態で CSRF トークンやユーザー情報を誤参照できないことを型レベルで保証する。
data AuthState
    = AuthChecking
      -- ^ 起動時または手動再確認時の @GET /api/auth/me@ 実行中
    | AuthUnauthenticated !AuthFormState
      -- ^ 未認証状態 (ログイン・新規登録画面を表示)
    | AuthAuthenticated !AuthSessionResponse
      -- ^ 認証済み状態 (公開プロフィールとセッション紐づき CSRF トークンを保持)
    deriving (Eq, Show)

-- | 現在のクライアント状態から認証済みセッション情報を取り出す。
currentAuthSession :: ClientState -> Maybe AuthSessionResponse
currentAuthSession state = case state.authState of
    AuthAuthenticated session -> Just session
    _                         -> Nothing

-- | 現在のクライアント状態から認証済みユーザー ID を取り出す。
currentUserId :: ClientState -> Maybe UserId
currentUserId state = fmap (\ session -> session.user.id) (currentAuthSession state)

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

-- | 未保存変更がある状態で画面遷移またはログアウトを試みた際に確認待ちとする遷移先。
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
    | NavApplyConflictLocal !DocumentId
      -- ^ 競合退避されたローカル変更をエディタへ反映する
    | NavRecoverConflictAsNew !DocumentId
      -- ^ 競合退避されたローカル変更を別名の新規文書として保存する
    | NavLogout
      -- ^ 未保存変更を破棄してログアウトする
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

-- | 現在の認証済みユーザー ID・保存済み 'Document'・編集中 'EditorDraft' から
-- IndexedDB 退避用 'StoredDraft' を構築する。
-- 所有者 ID を常に記録することで、同一ブラウザで別ユーザーへ切り替えた際のドラフト誤露出を防ぐ。
toStoredDraft :: UserId -> Document -> EditorDraft -> UTCTime -> StoredDraft
toStoredDraft ownerUid doc currentDraft now = StoredDraft
    { docId         = doc.id
    , ownerId       = Just ownerUid
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

-- | リビジョン詳細ビューの表示タブ。
data RevisionDetailTab
    = RevTabBody
      -- ^ 再構成済み本文スナップショット表示
    | RevTabDiff
      -- ^ 親リビジョンからの unified diff 表示
    | RevTabCompare
      -- ^ 任意 2 リビジョン間の差分比較表示
    deriving (Eq, Show)

-- | 編集履歴パネル (過去リビジョン一覧・詳細閲覧・任意 2 版比較・復元) の状態。
-- 文書切替やログアウト時に 'resetRevisionHistory' で世代番号を進めつつ初期化することで、
-- 旧文書の遅延 API 応答による履歴混入を構造的に防ぐ。
data RevisionHistoryState = RevisionHistoryState
    { isHistoryOpen     :: !Bool
      -- ^ 履歴パネルを開いているか
    , documentId        :: !(Maybe DocumentId)
      -- ^ 履歴を保持している対象文書 ID
    , listRequestSeq    :: !Int
      -- ^ 履歴一覧取得の世代番号
    , isLoadingList     :: !Bool
      -- ^ 履歴一覧取得中フラグ
    , isLoadingMore     :: !Bool
      -- ^ 追加ページ取得中フラグ
    , revisions         :: ![RevisionSummary]
      -- ^ リビジョンサマリ一覧 (リビジョン番号降順)
    , totalRevisions    :: !Int
      -- ^ サーバ側の総リビジョン件数
    , selectedRevision  :: !(Maybe RevisionNumber)
      -- ^ 選択中のリビジョン番号
    , detailRequestSeq  :: !Int
      -- ^ リビジョン詳細取得の世代番号
    , isLoadingDetail   :: !Bool
      -- ^ リビジョン詳細取得中フラグ
    , revisionDetail    :: !(Maybe RevisionDetail)
      -- ^ 取得済みの選択リビジョン詳細
    , activeTab         :: !RevisionDetailTab
      -- ^ 詳細ビューの表示タブ (本文 / 差分 / 2 版比較)
    , compareFromRev    :: !(Maybe RevisionNumber)
      -- ^ 任意 2 版比較の比較元リビジョン番号
    , compareToRev      :: !(Maybe RevisionNumber)
      -- ^ 任意 2 版比較の比較先リビジョン番号
    , compareRequestSeq :: !Int
      -- ^ 任意 2 版比較取得の世代番号
    , isLoadingCompare  :: !Bool
      -- ^ 任意 2 版比較取得中フラグ
    , compareResult     :: !(Maybe RevisionCompareResponse)
      -- ^ 取得済みの任意 2 版比較レスポンス
    , confirmRestoreRev :: !(Maybe RevisionNumber)
      -- ^ 復元確認モーダル表示中の対象リビジョン番号
    } deriving (Eq, Show)

-- | 閉じた状態の初期リビジョン履歴状態。
emptyRevisionHistoryState :: RevisionHistoryState
emptyRevisionHistoryState = RevisionHistoryState
    { isHistoryOpen     = False
    , documentId        = Nothing
    , listRequestSeq    = 0
    , isLoadingList     = False
    , isLoadingMore     = False
    , revisions         = []
    , totalRevisions    = 0
    , selectedRevision  = Nothing
    , detailRequestSeq  = 0
    , isLoadingDetail   = False
    , revisionDetail    = Nothing
    , activeTab         = RevTabBody
    , compareFromRev    = Nothing
    , compareToRev      = Nothing
    , compareRequestSeq = 0
    , isLoadingCompare  = False
    , compareResult     = Nothing
    , confirmRestoreRev = Nothing
    }

-- | 履歴パネルを閉じ、進行中の非同期応答を無効化するよう世代番号を進めて初期化する。
resetRevisionHistory :: RevisionHistoryState -> RevisionHistoryState
resetRevisionHistory rh = emptyRevisionHistoryState
    { listRequestSeq    = rh.listRequestSeq + 1
    , detailRequestSeq  = rh.detailRequestSeq + 1
    , compareRequestSeq = rh.compareRequestSeq + 1
    }

-- | 3-way マージ手動解決セッションの確定状態。
data MergeSession = MergeSession
    { docId          :: !DocumentId
      -- ^ マージ対象文書 ID
    , baseRevision   :: !RevisionNumber
      -- ^ 共通祖先リビジョン番号
    , serverDocument :: !Document
      -- ^ サーバ側の最新文書スナップショット (@base_revision@ 基準)
    , localSave      :: !PendingSyncSave
      -- ^ 競合退避されたローカル変更
    , baseBody       :: !DocumentBody
      -- ^ 共通祖先リビジョンの再構成済み本文
    , mergeOutcome   :: !MergeOutcome
      -- ^ 純粋 3-way マージ ('mergeThreeWay') の構造化結果
    , choices        :: !(Map Int ConflictChoice)
      -- ^ 競合ブロックインデックスごとの選択済み解決方針
    , isSavingMerge  :: !Bool
      -- ^ マージ結果保存 (@PUT@) 実行中フラグ
    } deriving (Eq, Show)

-- | 競合 3-way マージ解決モーダルのライフサイクル状態。
data ConflictMergeState
    = MergeLoading !Int !DocumentId
      -- ^ 共通祖先リビジョンおよびサーバ最新文書を取得中 (リクエスト世代番号, 文書 ID)
    | MergeReady !MergeSession
      -- ^ 3-way マージ結果を表示・手動解決中
    deriving (Eq, Show)

-- | Miso Model 内で保持する拡張機能の純粋カタログエントリ (メタデータ＋コマンドメタデータ)。
-- 実行関数を含めないことで 'ClientState' の 'Eq' / 'Show' 導出を維持する。
data ExtensionCatalogEntry = ExtensionCatalogEntry
    { metadata :: !ExtensionMetadata
      -- ^ 拡張機能の基本メタデータ
    , commands :: ![ExtensionCommandInfo]
      -- ^ 拡張機能が提供するコマンドメタデータ一覧
    } deriving (Eq, Show)

-- | カタログから既定有効 (@defaultEnabled == True@) な拡張 ID 集合を算出する。
defaultEnabledExtensions :: [ExtensionCatalogEntry] -> Set ExtensionId
defaultEnabledExtensions entries =
    Set.fromList
        [ entry.metadata.extensionId
        | entry <- entries
        , entry.metadata.defaultEnabled
        ]

-- | クライアントコアの状態モデル (Miso Model)。
-- 本モジュールは 'tamatsuzura-common' と 'tamatsuzura-extension-api' のみに依存し、
-- 同梱拡張の具体実装には依存しない。
data ClientState = ClientState
    { connectionStatus             :: !ConnectionStatus
      -- ^ API サーバの疎通状態
    , networkStatus                :: !NetworkStatus
      -- ^ ブラウザのネットワークオンライン / オフライン状態
    , authState                    :: !AuthState
      -- ^ 認証セッション状態 (確認中 / 未認証フォーム / 認証済み)
    , authSessionSeq               :: !Int
      -- ^ 認証セッション世代番号 (ログアウト・401 失効・再ログイン時の古い非同期応答を完全に無視する)
    , isLoggingOut                 :: !Bool
      -- ^ ログアウト API 実行中フラグ
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
    , restoringRevisionDoc         :: !(Maybe DocumentId)
      -- ^ 現在過去リビジョン復元中の文書 ID (パネル開閉・文書切替を跨いで二重送信と競合を防止)
    , selectedDocument             :: !(Maybe Document)
      -- ^ 現在開いている保存済み文書スナップショット
    , draft                        :: !EditorDraft
      -- ^ 現在編集中のドラフト内容
    , storedDrafts                 :: !(Map DocumentId StoredDraft)
      -- ^ IndexedDB に退避されている現在ログイン中ユーザーの未保存ドラフト (文書 ID キー)
    , draftSyncState               :: !DraftSyncState
      -- ^ 現在編集中文書のローカルドラフト退避ステータス
    , restoredDraftDocId           :: !(Maybe DocumentId)
      -- ^ 未保存ドラフト復元バナーを表示中の対象文書 ID
    , staleDraftPrompt             :: !(Maybe StoredDraft)
      -- ^ サーバ側更新済みまたはゴミ箱内文書に残存する未適用ドラフト
    , orphanedDraft                :: !(Maybe StoredDraft)
      -- ^ サーバ側で 404 だった自動復元対象ドラフト (新規文書として救出可能)
    , cachedDocuments              :: !(Map DocumentId CachedDocument)
      -- ^ IndexedDB にキャッシュされている現在ログイン中ユーザーの文書 (文書 ID キー)
    , pendingSaves                 :: !(Map DocumentId PendingSyncSave)
      -- ^ オフライン中に保存された現在ログイン中ユーザーの未同期保存キュー (文書 ID キー)
    , syncConflicts                :: !(Map DocumentId SyncConflict)
      -- ^ 409 競合等でサーバを上書きせず安全退避されたローカル変更 (文書 ID キー)
    , syncingDocId                 :: !(Maybe DocumentId)
      -- ^ 現在バックグラウンド同期 PUT 実行中の文書 ID
    , isServingOfflineCache        :: !Bool
      -- ^ 現在の文書一覧がオフラインキャッシュから表示されているか
    , conflictMerge                :: !(Maybe ConflictMergeState)
      -- ^ 競合解決用 3-way マージモーダルの現在状態
    , mergeRequestSeq              :: !Int
      -- ^ 3-way マージ読み込みリクエストの世代番号
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
    , revisionHistory              :: !RevisionHistoryState
      -- ^ 過去リビジョン履歴閲覧・復元パネルの状態
    , extensionsPanelOpen          :: !Bool
      -- ^ 拡張機能管理・コマンド実行パネルを開いているか
    , enabledExtensions            :: !(Set ExtensionId)
      -- ^ 現在有効化されている拡張機能 ID の集合
    , pendingNavigation            :: !(Maybe PendingNavigation)
      -- ^ 未保存変更破棄の確認ダイアログ待ち遷移
    , confirmPermanentDelete       :: !(Maybe DocumentId)
      -- ^ 完全削除の二段階確認対象文書 ID
    , lastError                    :: !(Maybe Text)
      -- ^ 直近発生したユーザー向けエラーメッセージ
    , statusNotice                 :: !(Maybe Text)
      -- ^ 保存・復元などの完了通知メッセージ
    , extensionCatalog             :: ![ExtensionCatalogEntry]
      -- ^ 結線層から登録された拡張機能のカタログ (メタデータ＋コマンド一覧)
    } deriving (Eq, Show)

-- | クライアント MVU ループで処理するアクション。
data ClientAction
    = Initialize
      -- ^ マウント時の初回ヘルスチェック・認証セッション確認 (@GET /api/auth/me@) トリガ
    | RefreshAll
      -- ^ 認証済み時は一覧・タグ・フォルダ再取得、未認証時は接続・セッション再確認
    | HealthChecked !(Either ApiClientError HealthStatus)
      -- ^ @GET /api/health@ の結果受信
    | AuthSessionChecked !Int !(Either ApiClientError AuthSessionResponse)
      -- ^ @GET /api/auth/me@ の結果受信 (認証セッション世代番号, 結果)
    | SwitchAuthMode !AuthFormMode
      -- ^ ログイン / 新規登録タブの切替
    | UpdateAuthUsername !Text
      -- ^ 認証フォームのユーザー名入力変更
    | UpdateAuthDisplayName !Text
      -- ^ 認証フォームの表示名入力変更
    | UpdateAuthPassword !Text
      -- ^ 認証フォームのパスワード入力変更
    | UpdateAuthPasswordConfirm !Text
      -- ^ 認証フォームの確認用パスワード入力変更
    | SubmitAuthForm
      -- ^ ログインまたは新規登録フォームの送信
    | AuthSubmitted !Int !AuthFormMode !(Either ApiClientError AuthSessionResponse)
      -- ^ @POST /api/auth/login@ または @POST /api/auth/register@ の結果受信
    | RequestLogout
      -- ^ ログアウト要求 (未保存変更があれば確認ダイアログ)
    | LoggedOut !Int !(Either ApiClientError ())
      -- ^ @POST /api/auth/logout@ の結果受信 (認証セッション世代番号, 結果)
    | DocumentsLoaded !Int !Int !(Either ApiClientError DocumentListResponse)
      -- ^ @GET /api/documents@ の結果受信 (認証セッション世代番号, リクエスト世代番号, 結果)
    | TagsLoaded !Int !(Either ApiClientError TagListResponse)
      -- ^ @GET /api/tags@ の結果受信 (認証セッション世代番号, 結果)
    | FoldersLoaded !Int !(Either ApiClientError FolderListResponse)
      -- ^ @GET /api/folders@ の結果受信 (認証セッション世代番号, 結果)
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
    | DocumentLoaded !Int !DocumentId !(Either ApiClientError Document)
      -- ^ @GET /api/documents/{id}@ の結果受信 (認証セッション世代番号, 文書 ID, 結果)
    | RequestCreateDocument
      -- ^ 新規文書作成要求 (未保存変更があれば確認ダイアログ)
    | DocumentCreated !Int !(Either ApiClientError Document)
      -- ^ @POST /api/documents@ の結果受信 (認証セッション世代番号, 結果)
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
    | DocumentSaved !Int !DocumentId !(Either ApiClientError Document)
      -- ^ @PUT /api/documents/{id}@ の結果受信 (認証セッション世代番号, 文書 ID, 結果)
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
    | DocumentSoftDeleted !Int !DocumentId !(Either ApiClientError Document)
      -- ^ 論理削除の結果受信 (認証セッション世代番号, 文書 ID, 結果)
    | RestoreDocument !DocumentId
      -- ^ ゴミ箱内の文書を復元 (@POST /api/documents/{id}/restore@)
    | DocumentRestored !Int !DocumentId !(Either ApiClientError Document)
      -- ^ 復元の結果受信 (認証セッション世代番号, 文書 ID, 結果)
    | RequestPermanentDelete !DocumentId
      -- ^ 完全削除の確認ダイアログを表示する
    | CancelPermanentDelete
      -- ^ 完全削除の確認ダイアログを閉じる
    | ConfirmPermanentDelete !DocumentId
      -- ^ 完全削除を確定実行する (@DELETE /api/documents/{id}/permanent@)
    | DocumentPermanentlyDeleted !Int !DocumentId !(Either ApiClientError ())
      -- ^ 完全削除の結果受信 (認証セッション世代番号, 文書 ID, 結果)
    | ApplyExtension !ExtensionId
      -- ^ 指定した拡張機能のプライマリ変換を現在の編集本文へ適用する (互換アクション)
    | ToggleExtensionsPanel
      -- ^ 拡張機能管理パネルの開閉
    | ToggleExtensionEnabled !ExtensionId
      -- ^ 指定した拡張機能の有効・無効を切り替える
    | RunExtensionCommand !ExtensionId !CommandId
      -- ^ 有効化済み拡張の指定コマンドを現在文書へ実行する
    | DraftsLoaded !Int !(Either Text [StoredDraft])
      -- ^ IndexedDB 未保存ドラフト一覧の読み込み結果 (認証セッション世代番号, 結果)
    | DraftStorageEventReceived !DraftStorageEvent
      -- ^ IndexedDB ドラフト自動退避の完了/失敗通知
    | ApplyStaleDraft
      -- ^ サーバ側更新済み文書に対して残存ローカルドラフトを手動適用する
    | DiscardCurrentDraft
      -- ^ 現在文書に紐づくローカル未保存ドラフトを破棄してサーバ版へ戻す
    | RecoverOrphanedDraft
      -- ^ サーバ削除済み文書の孤立ドラフトを新規文書として復元する
    | OrphanDraftRecovered !Int !DocumentId !(Either ApiClientError Document)
      -- ^ 孤立ドラフトからの新規文書作成応答 (認証セッション世代番号, 元文書 ID, 結果)
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
    | DocumentImported !Int !Text !(Either ApiClientError Document)
      -- ^ インポートに伴う @POST /api/documents@ の結果受信 (認証セッション世代番号, 元ファイル名, 結果)
    | ToggleRevisionHistory
      -- ^ リビジョン履歴パネルの開閉
    | RefreshRevisionHistory
      -- ^ 現在開いている文書のリビジョン履歴一覧を再取得する
    | LoadMoreRevisions
      -- ^ 現在開いている文書のリビジョン履歴の次ページを追加取得する
    | RevisionListLoaded !Int !Int !DocumentId !Bool !(Either ApiClientError RevisionListResponse)
      -- ^ @GET /api/documents/{id}/revisions@ の結果受信 (認証世代, 一覧世代, 文書 ID, 追加読み込みフラグ, 結果)
    | SelectRevision !RevisionNumber
      -- ^ 履歴一覧から特定リビジョンを選択し詳細を取得する
    | RevisionDetailLoaded !Int !Int !DocumentId !RevisionNumber !(Either ApiClientError RevisionDetail)
      -- ^ @GET /api/documents/{id}/revisions/{revision}@ の結果受信 (認証世代, 詳細世代, 文書 ID, リビジョン番号, 結果)
    | SetRevisionDetailTab !RevisionDetailTab
      -- ^ リビジョン詳細ビューの本文 / 差分 / 2 版比較タブ切替
    | SetCompareFromRevision !RevisionNumber
      -- ^ 任意 2 版比較の比較元リビジョン番号を選択する
    | SetCompareToRevision !RevisionNumber
      -- ^ 任意 2 版比較の比較先リビジョン番号を選択する
    | CompareRevisions
      -- ^ 現在選択中の比較元・比較先リビジョンで比較 API を実行する
    | RevisionsCompared !Int !Int !DocumentId !(Either ApiClientError RevisionCompareResponse)
      -- ^ @GET /api/documents/{id}/revisions/compare@ の結果受信 (認証世代, 比較世代, 文書 ID, 結果)
    | RequestRestoreRevision !RevisionNumber
      -- ^ 選択リビジョンへの復元確認ダイアログを表示する
    | CancelRestoreRevision
      -- ^ 過去リビジョン復元の確認ダイアログを閉じる
    | ConfirmRestoreRevision !RevisionNumber
      -- ^ 過去リビジョンへの復元を確定実行する (@POST /api/documents/{id}/revisions/{revision}/restore@)
    | RevisionRestored !Int !DocumentId !(Either ApiClientError Document)
      -- ^ 過去リビジョン復元の結果受信 (認証世代, 文書 ID, 結果)
    | NetworkStatusChanged !NetworkStatus
      -- ^ ブラウザのオンライン / オフライン状態変化通知
    | OfflineStoreLoaded !Int !(Either Text [CachedDocument]) !(Either Text [PendingSyncSave]) !(Either Text [SyncConflict])
      -- ^ IndexedDB オフラインキャッシュ・未同期保存キュー・競合退避レコードの読み込み結果 (認証世代, 各結果)
    | TriggerSyncQueue
      -- ^ 未同期保存キューの同期を手動または自動で開始する
    | SyncItemCompleted !Int !DocumentId !(Either ApiClientError Document)
      -- ^ 未同期保存キュー 1 件分の @PUT /api/documents/{id}@ 結果受信 (認証世代, 文書 ID, 結果)
    | ApplyConflictLocalCopy !DocumentId
      -- ^ 競合退避されたローカル変更をエディタへ反映して確認・編集する
    | ConflictLocalApplyLoaded !Int !DocumentId !(Either ApiClientError Document)
      -- ^ 競合退避反映前の最新サーバ文書取得応答 (認証世代, 文書 ID, 結果)
    | RecoverConflictAsNewDocument !DocumentId
      -- ^ 競合退避されたローカル変更を別名の新規文書としてサーバへ保存する
    | ConflictRecoveredAsNew !Int !DocumentId !(Either ApiClientError Document)
      -- ^ 競合退避からの別文書作成応答 (認証世代, 元文書 ID, 結果)
    | DismissSyncConflict !DocumentId
      -- ^ 競合退避レコードをローカルから破棄する
    | OpenConflictMerge !DocumentId
      -- ^ 競合退避レコードに対して 3-way マージ解決モーダルを開く
    | ConflictMergeLoaded !Int !Int !DocumentId !(Either ApiClientError Document) !(Either ApiClientError RevisionDetail)
      -- ^ 3-way マージ用のサーバ最新版・共通祖先リビジョン取得応答
    | SelectMergeChoice !Int !ConflictChoice
      -- ^ 3-way マージ内の特定競合ブロックの解決方針を選択する
    | SelectAllMergeChoices !ConflictChoice
      -- ^ 3-way マージ内の全競合ブロックへ一括で解決方針を設定する
    | SaveResolvedMerge
      -- ^ 3-way マージ結果をサーバへ保存 (@PUT /api/documents/{id}@ with @base_revision@) する
    | ConflictMergeSaved !Int !DocumentId !(Either ApiClientError Document)
      -- ^ 3-way マージ保存の応答受信 (認証世代, 文書 ID, 結果)
    | CloseConflictMerge
      -- ^ 3-way マージ解決モーダルを閉じる
    | DismissError
      -- ^ 表示中のエラーバナーを閉じる
    | DismissNotice
      -- ^ 表示中の通知バナーを閉じる
    deriving (Eq, Show)

-- | 拡張一覧のメタデータ・コマンド情報を取り込んで初期クライアント状態を構築する。
initialClientState :: [Extension] -> ClientState
initialClientState exts =
    let catalog :: [ExtensionCatalogEntry]
        catalog = map toCatalogEntry exts
    in  ClientState
            { connectionStatus             = ConnChecking
            , networkStatus                = NetworkOnline
            , authState                    = AuthChecking
            , authSessionSeq               = 0
            , isLoggingOut                 = False
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
            , restoringRevisionDoc         = Nothing
            , selectedDocument             = Nothing
            , draft                        = emptyEditorDraft
            , storedDrafts                 = Map.empty
            , draftSyncState               = DraftSyncIdle
            , restoredDraftDocId           = Nothing
            , staleDraftPrompt             = Nothing
            , orphanedDraft                = Nothing
            , cachedDocuments              = Map.empty
            , pendingSaves                 = Map.empty
            , syncConflicts                = Map.empty
            , syncingDocId                 = Nothing
            , isServingOfflineCache        = False
            , conflictMerge                = Nothing
            , mergeRequestSeq              = 0
            , editorSyncVersion            = 0
            , editorStats                  = emptyEditorStats
            , canUndo                      = False
            , canRedo                      = False
            , metadataPanelOpen            = False
            , searchPanel                  = emptySearchPanelState
            , revisionHistory              = emptyRevisionHistoryState
            , extensionsPanelOpen          = False
            , enabledExtensions            = defaultEnabledExtensions catalog
            , pendingNavigation            = Nothing
            , confirmPermanentDelete       = Nothing
            , lastError                    = Nothing
            , statusNotice                 = Nothing
            , extensionCatalog             = catalog
            }
  where
    toCatalogEntry :: Extension -> ExtensionCatalogEntry
    toCatalogEntry ext = ExtensionCatalogEntry
        { metadata = ext.metadata
        , commands = map toExtensionCommandInfo ext.commands
        }

-- | ワークスペース上の全ユーザーデータを破棄し、未認証状態へ戻すための純粋リセット関数。
-- 接続状態および拡張メタデータは保持しつつ、文書一覧・選択文書・編集ドラフト・フィルタ・
-- 進行中リクエストフラグをすべて初期化し、@listRequestSeq@ と @authSessionSeq@ を進めて
-- 旧セッションの遅延応答による状態汚染を構造的に遮断する。
resetWorkspaceForUnauthenticated ::
       AuthFormState
    -> Maybe Text
    -> Maybe Text
    -> ClientState
    -> ClientState
resetWorkspaceForUnauthenticated nextForm mErr mNotice state = state
    { authState                    = AuthUnauthenticated nextForm
    , authSessionSeq               = state.authSessionSeq + 1
    , isLoggingOut                 = False
    , mobilePane                   = PaneList
    , editorViewMode               = ModeEdit
    , filterState                  = defaultFilterState
    , availableTags                = []
    , availableFolders             = []
    , documentSummaries            = []
    , totalDocuments               = 0
    , listRequestSeq               = state.listRequestSeq + 1
    , isLoadingDocuments           = False
    , isCreatingDocument           = False
    , isImportingDocument          = False
    , pendingListReloadAfterCreate = False
    , pendingListReloadAfterImport = False
    , openingDocumentId            = Nothing
    , autoOpeningDraftId           = Nothing
    , savingDocument               = Nothing
    , mutatingDocumentId           = Nothing
    , restoringRevisionDoc         = Nothing
    , selectedDocument             = Nothing
    , draft                        = emptyEditorDraft
    , storedDrafts                 = Map.empty
    , draftSyncState               = DraftSyncIdle
    , restoredDraftDocId           = Nothing
    , staleDraftPrompt             = Nothing
    , orphanedDraft                = Nothing
    , cachedDocuments              = Map.empty
    , pendingSaves                 = Map.empty
    , syncConflicts                = Map.empty
    , syncingDocId                 = Nothing
    , isServingOfflineCache        = False
    , conflictMerge                = Nothing
    , mergeRequestSeq              = state.mergeRequestSeq + 1
    , editorSyncVersion            = state.editorSyncVersion + 1
    , editorStats                  = emptyEditorStats
    , canUndo                      = False
    , canRedo                      = False
    , metadataPanelOpen            = False
    , searchPanel                  = emptySearchPanelState
    , revisionHistory              = resetRevisionHistory state.revisionHistory
    , extensionsPanelOpen          = False
    , enabledExtensions            = defaultEnabledExtensions state.extensionCatalog
    , pendingNavigation            = Nothing
    , confirmPermanentDelete       = Nothing
    , lastError                    = mErr
    , statusNotice                 = mNotice
    }

-- | 任意の表示名入力欄の生文字列を検証する。空白のみの入力は未設定 ('Nothing') とみなす。
parseDisplayNameInput :: Text -> Either ValidationError (Maybe DisplayName)
parseDisplayNameInput raw =
    let trimmed :: Text
        trimmed = Txt.strip raw
    in  if Txt.null trimmed
            then Right Nothing
            else Just <$> mkDisplayName trimmed

-- | ログインフォームの入力値を検証して 'LoginRequest' を構築する。
validateLoginForm :: UiMessages -> AuthFormState -> Either Text LoginRequest
validateLoginForm msgs form = do
    validUser <- mapValErr (mkUsername form.usernameInput)
    validPass <- mapValErr (mkPassword form.passwordInput)
    Right LoginRequest
        { username = validUser
        , password = validPass
        }
  where
    mapValErr :: Either ValidationError a -> Either Text a
    mapValErr = either (Left . formatValidationError msgs) Right

-- | 新規ユーザー登録フォームの入力値を検証して 'RegisterRequest' を構築する。
-- パスワード確認欄の一致確認と 'validateRegisterRequest' の最小文字数検証を送信前に完了させる。
validateRegisterForm :: UiMessages -> AuthFormState -> Either Text RegisterRequest
validateRegisterForm msgs form = do
    validUser    <- mapValErr (mkUsername form.usernameInput)
    validDisplay <- mapValErr (parseDisplayNameInput form.displayNameInput)
    rawPass      <- mapValErr (mkPassword form.passwordInput)
    validPass    <- mapValErr (validateNewPassword rawPass)
    when (form.passwordInput /= form.passwordConfirmInput) $
        Left (msgs.errValidationPrefix <> " (" <> msgs.valPasswordMismatch <> ")")
    let req :: RegisterRequest
        req = RegisterRequest
            { username    = validUser
            , displayName = validDisplay
            , password    = validPass
            }
    mapValErr (validateRegisterRequest req)
  where
    mapValErr :: Either ValidationError a -> Either Text a
    mapValErr = either (Left . formatValidationError msgs) Right

-- | 起動時・再読込時の @GET /api/auth/me@ 応答を状態へ純粋に反映する。
-- 通常の未ログイン状態 (HTTP 401) ではエラーバナーを出さずにログイン画面へ遷移する。
applyAuthSessionChecked ::
       UiMessages
    -> Int
    -> Either ApiClientError AuthSessionResponse
    -> ClientState
    -> ClientState
applyAuthSessionChecked msgs seqNo result state
    | seqNo /= state.authSessionSeq = state
    | otherwise = case result of
        Right session ->
            (resetWorkspaceForAuthenticated session Nothing state)
                { connectionStatus = ConnOnline
                }
        Left err
            | isUnauthorizedError err ->
                resetWorkspaceForUnauthenticated emptyAuthFormState Nothing Nothing state
            | otherwise ->
                resetWorkspaceForUnauthenticated
                    emptyAuthFormState
                    (Just (formatApiClientError msgs err))
                    Nothing
                    state

-- | ログインまたは新規登録 API 応答を状態へ純粋に反映する。
applyAuthSubmitted ::
       UiMessages
    -> Int
    -> AuthFormMode
    -> Either ApiClientError AuthSessionResponse
    -> ClientState
    -> ClientState
applyAuthSubmitted msgs seqNo submittedMode result state
    | seqNo /= state.authSessionSeq = state
    | otherwise = case result of
        Right session ->
            let noticeText :: Text
                noticeText = case submittedMode of
                    AuthModeLogin    -> msgs.noticeLoggedIn
                    AuthModeRegister -> msgs.noticeRegistered
            in  (resetWorkspaceForAuthenticated session (Just noticeText) state)
                    { connectionStatus = ConnOnline
                    }
        Left err ->
            let errText :: Text
                errText = case submittedMode of
                    AuthModeLogin    -> formatLoginApiError msgs err
                    AuthModeRegister -> formatRegisterApiError msgs err

                nextForm :: AuthFormState
                nextForm = case state.authState of
                    AuthUnauthenticated form -> form
                        { isSubmitting = False
                        , formError    = Just errText
                        }
                    _ -> emptyAuthFormState
                        { mode      = submittedMode
                        , formError = Just errText
                        }
            in  state
                    { authState = AuthUnauthenticated nextForm
                    }

-- | 認証済みセッションの失効 (HTTP 401) を状態へ純粋に反映する。
-- すでに未認証状態へ遷移済みの場合はログインフォームの入力内容を保護するため何もしない。
applySessionExpired :: UiMessages -> ClientState -> ClientState
applySessionExpired msgs state = case state.authState of
    AuthAuthenticated{} ->
        resetWorkspaceForUnauthenticated
            emptyAuthFormState
            (Just msgs.errSessionExpired)
            Nothing
            state
    _ -> state

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
        { title        = validTitle
        , body         = validBody
        , format       = currentDraft.format
        , folder       = validFolder
        , tags         = validTags
        , baseRevision = Nothing
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

-- | 拡張レジストリ内の該当コマンドを検索し、フォーマット制約を検証した上で現在の本文へ純粋に適用する。
-- 有効化状態 ('enabledExtensions') および選択文書・ゴミ箱・通信中ガードの判定は
-- 'updateClient' の 'RunExtensionCommand' 側で実施する。
-- コマンド不在・フォーマット不一致・コマンド固有失敗は 'Left' で返し、
-- 正常時は更新後 'ClientState' と任意の実行サマリ文言を返す。
applyExtensionCommand ::
       [Extension]
    -> ExtensionId
    -> CommandId
    -> ClientState
    -> Either ExtensionError (ClientState, Maybe Text)
applyExtensionCommand exts targetExtId targetCmdId state =
    case find (\ ext -> ext.metadata.extensionId == targetExtId) exts of
        Nothing ->
            Left (ExtensionCommandNotFound targetExtId targetCmdId)
        Just ext ->
            let ctx :: ExtensionContext
                ctx = ExtensionContext
                    { body   = state.draft.body
                    , format = state.draft.format
                    }
            in  case runExtensionCommand ext targetCmdId ctx of
                    Left err -> Left err
                    Right res ->
                        let nextState :: ClientState
                            nextState = state
                                { draft             = setDraftBody res.nextBody state.draft
                                , editorSyncVersion = state.editorSyncVersion + 1
                                , editorStats       = computeTextStats (toText res.nextBody)
                                }
                        in  Right (nextState, res.summary)

-- | ヘルスチェック応答をクライアント状態へ純粋に反映する。
applyHealthResult ::
       UiMessages
    -> Either ApiClientError HealthStatus
    -> ClientState
    -> ClientState
applyHealthResult _ (Right _health) state =
    state
        { connectionStatus = ConnOnline
        , networkStatus    = NetworkOnline
        }
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
                    { documentSummaries     = res.documents
                    , totalDocuments        = res.total
                    , isLoadingDocuments    = False
                    , isServingOfflineCache = False
                    , lastError             = nextError
                    }
        Left err
            | isNetworkError err, Just uid <- currentUserId state, not (Map.null state.cachedDocuments) ->
                let cachedSummaries :: [DocumentSummary]
                    cachedSummaries = filterCachedDocuments
                        uid
                        state.filterState.status
                        state.filterState.searchInput
                        state.filterState.selectedTag
                        state.filterState.selectedFolder
                        state.filterState.includeSubfolders
                        (Map.elems state.cachedDocuments)

                    nextError :: Maybe Text
                    nextError = case state.connectionStatus of
                        ConnError{} -> state.lastError
                        _
                            | state.networkStatus == NetworkOffline -> Nothing
                            | otherwise                             -> Just (formatApiClientError msgs err)
                in  state
                        { documentSummaries     = cachedSummaries
                        , totalDocuments        = length cachedSummaries
                        , isLoadingDocuments    = False
                        , isServingOfflineCache = True
                        , lastError             = nextError
                        }
            | otherwise ->
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

-- | IndexedDB ドラフト一覧読み込み結果をクライアント状態へ純粋に反映する。
-- マルチユーザー分離を保証するため、現在認証済みユーザー ('currentUserId') と 'ownerId' が
-- 一致するドラフトのみを採用し、他ユーザーおよび所有者未記録のレガシードラフトは無視する。
applyDraftsLoaded :: UiMessages -> Either Text [StoredDraft] -> ClientState -> ClientState
applyDraftsLoaded _ (Left errMsg) state =
    state { draftSyncState = DraftSyncFailed errMsg }
applyDraftsLoaded msgs (Right drafts) state = case currentUserId state of
    Nothing -> state
    Just uid ->
        let ownedDrafts :: [StoredDraft]
            ownedDrafts = filterDraftsForOwner uid drafts

            loadedMap :: Map DocumentId StoredDraft
            loadedMap = Map.fromList [ (sd.docId, sd) | sd <- ownedDrafts ]

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
applyDraftStorageEvent evt state = case state.authState of
    AuthAuthenticated{} -> case evt of
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
    _ -> state

-- | IndexedDB のオフラインキャッシュ・未同期保存キュー・競合退避レコードの読み込み結果を状態へ純粋に反映する。
-- マルチユーザー分離を保証するため、現在認証済みユーザー ('currentUserId') と 'ownerId' が一致するレコードのみを採用する。
applyOfflineStoreLoaded ::
       Either Text [CachedDocument]
    -> Either Text [PendingSyncSave]
    -> Either Text [SyncConflict]
    -> ClientState
    -> ClientState
applyOfflineStoreLoaded eCached ePending eConflicts state = case currentUserId state of
    Nothing  -> state
    Just uid ->
        let loadedCached :: Map DocumentId CachedDocument
            loadedCached = case eCached of
                Left _   -> Map.empty
                Right xs -> Map.fromList [ (cd.document.id, cd) | cd <- xs, cd.ownerId == uid ]

            loadedPending :: Map DocumentId PendingSyncSave
            loadedPending = case ePending of
                Left _   -> Map.empty
                Right xs -> Map.fromList [ (ps.docId, ps) | ps <- xs, ps.ownerId == uid ]

            loadedConflicts :: Map DocumentId SyncConflict
            loadedConflicts = case eConflicts of
                Left _   -> Map.empty
                Right xs -> Map.fromList [ (sc.docId, sc) | sc <- xs, sc.ownerId == uid ]
        in  state
                { cachedDocuments = Map.union state.cachedDocuments loadedCached
                , pendingSaves    = Map.union state.pendingSaves loadedPending
                , syncConflicts   = Map.union state.syncConflicts loadedConflicts
                }

-- | ブラウザのオンライン / オフライン状態変化を状態へ純粋に反映する。
applyNetworkStatusChanged :: NetworkStatus -> ClientState -> ClientState
applyNetworkStatusChanged nextStatus state = state
    { networkStatus = nextStatus
    }

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
                        , canUndo             = False
                        , canRedo             = False
                        , revisionHistory     = resetRevisionHistory state.revisionHistory
                        , extensionsPanelOpen = False
                        , mobilePane          = PaneEditor
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
                        , canUndo             = False
                        , canRedo             = False
                        , revisionHistory     = resetRevisionHistory state.revisionHistory
                        , extensionsPanelOpen = False
                        , mobilePane          = PaneEditor
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
                        , canUndo             = False
                        , canRedo             = False
                        , revisionHistory     = resetRevisionHistory state.revisionHistory
                        , extensionsPanelOpen = False
                        , mobilePane          = PaneEditor
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
                        , canUndo             = False
                        , canRedo             = False
                        , revisionHistory     = resetRevisionHistory state.revisionHistory
                        , extensionsPanelOpen = False
                        , mobilePane          = PaneEditor
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
            , canUndo             = False
            , canRedo             = False
            , revisionHistory     = resetRevisionHistory state.revisionHistory
            , extensionsPanelOpen = False
            , mobilePane          = PaneEditor
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
                , canUndo             = not wasSelected && state.canUndo
                , canRedo             = not wasSelected && state.canRedo
                , revisionHistory     = if wasSelected then resetRevisionHistory state.revisionHistory else state.revisionHistory
                , extensionsPanelOpen = not wasSelected && state.extensionsPanelOpen
                , mobilePane          = if wasSelected then PaneList else state.mobilePane
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
                , revisionHistory        = if wasSelected then resetRevisionHistory state.revisionHistory else state.revisionHistory
                , extensionsPanelOpen    = not wasSelected && state.extensionsPanelOpen
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

-- | リビジョン履歴一覧 (@GET /api/documents/{id}/revisions@) の応答をクライアント状態へ反映する。
-- パネルが閉じられている場合・対象文書が変わっていた場合・一覧世代番号が一致しない陳腐応答は無視する。
applyRevisionListLoaded ::
       UiMessages
    -> Int
    -> DocumentId
    -> Bool
    -> Either ApiClientError RevisionListResponse
    -> ClientState
    -> ClientState
applyRevisionListLoaded msgs reqSeq docId isAppend result state
    | not state.revisionHistory.isHistoryOpen         = state
    | reqSeq /= state.revisionHistory.listRequestSeq  = state
    | state.revisionHistory.documentId /= Just docId  = state
    | fmap (.id) state.selectedDocument /= Just docId = state
    | otherwise = case result of
        Right res ->
            let rh :: RevisionHistoryState
                rh = state.revisionHistory

                mergedRevisions :: [RevisionSummary]
                mergedRevisions
                    | isAppend =
                        let existingNums :: [RevisionNumber]
                            existingNums = map (.revision) rh.revisions
                        in  rh.revisions <> filter (\ item -> item.revision `notElem` existingNums) res.revisions
                    | otherwise = res.revisions

                stillHasSelected :: Bool
                stillHasSelected = case rh.selectedRevision of
                    Nothing  -> False
                    Just rev -> any (\ item -> item.revision == rev) mergedRevisions

                nextSelected :: Maybe RevisionNumber
                nextSelected
                    | stillHasSelected = rh.selectedRevision
                    | otherwise        = case mergedRevisions of
                        firstRev : _ -> Just firstRev.revision
                        []           -> Nothing

                nextDetail :: Maybe RevisionDetail
                nextDetail = case (nextSelected, rh.revisionDetail) of
                    (Just sel, Just detail)
                        | detail.documentId == docId && detail.revision == sel -> Just detail
                    _ -> Nothing
            in  state
                    { revisionHistory = rh
                        { isLoadingList    = False
                        , isLoadingMore    = False
                        , revisions        = mergedRevisions
                        , totalRevisions   = res.total
                        , selectedRevision = nextSelected
                        , revisionDetail   = nextDetail
                        }
                    , lastError = Nothing
                    }
        Left err ->
            state
                { revisionHistory = state.revisionHistory
                    { isLoadingList = False
                    , isLoadingMore = False
                    }
                , lastError = Just (formatApiClientError msgs err)
                }

-- | 特定リビジョン詳細 (@GET /api/documents/{id}/revisions/{revision}@) の応答を状態へ反映する。
-- パネルが閉じられている場合・詳細世代番号・対象文書 ID・選択中リビジョン番号が一致しない陳腐応答は無視する。
applyRevisionDetailLoaded ::
       UiMessages
    -> Int
    -> DocumentId
    -> RevisionNumber
    -> Either ApiClientError RevisionDetail
    -> ClientState
    -> ClientState
applyRevisionDetailLoaded msgs reqSeq docId revNo result state
    | not state.revisionHistory.isHistoryOpen              = state
    | reqSeq /= state.revisionHistory.detailRequestSeq     = state
    | state.revisionHistory.documentId /= Just docId       = state
    | state.revisionHistory.selectedRevision /= Just revNo = state
    | otherwise = case result of
        Right detail ->
            state
                { revisionHistory = state.revisionHistory
                    { isLoadingDetail = False
                    , revisionDetail  = Just detail
                    }
                , lastError = Nothing
                }
        Left err ->
            state
                { revisionHistory = state.revisionHistory
                    { isLoadingDetail = False
                    , revisionDetail  = Nothing
                    }
                , lastError = Just (formatApiClientError msgs err)
                }

-- | 任意 2 リビジョン比較 (@GET /api/documents/{id}/revisions/compare@) の応答を状態へ純粋に反映する。
-- パネルが閉じられている場合・比較世代番号・対象文書 ID が一致しない陳腐応答は無視する。
applyRevisionCompareLoaded ::
       UiMessages
    -> Int
    -> DocumentId
    -> Either ApiClientError RevisionCompareResponse
    -> ClientState
    -> ClientState
applyRevisionCompareLoaded msgs reqSeq docId result state
    | not state.revisionHistory.isHistoryOpen           = state
    | reqSeq /= state.revisionHistory.compareRequestSeq = state
    | state.revisionHistory.documentId /= Just docId    = state
    | otherwise = case result of
        Right cmpRes ->
            state
                { revisionHistory = state.revisionHistory
                    { isLoadingCompare = False
                    , compareResult    = Just cmpRes
                    }
                , lastError = Nothing
                }
        Left err ->
            state
                { revisionHistory = state.revisionHistory
                    { isLoadingCompare = False
                    , compareResult    = Nothing
                    }
                , lastError = Just (formatApiClientError msgs err)
                }

-- | 過去リビジョン復元 (@POST /api/documents/{id}/revisions/{revision}/restore@) の応答を状態へ反映する。
-- 進行中の復元対象文書 ID ('restoringRevisionDoc') と一致しない陳腐応答は無視する。
-- 成功時は本文・タイトル・形式を復元済み最新文書へ同期し、未保存ローカルドラフトを破棄する。
applyRevisionRestored ::
       UiMessages
    -> DocumentId
    -> Either ApiClientError Document
    -> ClientState
    -> ClientState
applyRevisionRestored msgs targetId result state
    | state.restoringRevisionDoc /= Just targetId = state
    | otherwise = case result of
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
                isSameSelected = fmap (.id) state.selectedDocument == Just targetId

                restoredDraft :: EditorDraft
                restoredDraft = draftFromDocument doc

                nextRevisionHistory :: RevisionHistoryState
                nextRevisionHistory = state.revisionHistory
                    { confirmRestoreRev = Nothing
                    , selectedRevision  = if isSameSelected then Just doc.revision else state.revisionHistory.selectedRevision
                    , revisionDetail    = if isSameSelected then Nothing else state.revisionHistory.revisionDetail
                    }
            in  state
                    { restoringRevisionDoc = Nothing
                    , documentSummaries    = nextSummaries
                    , selectedDocument     = if isSameSelected then Just doc else state.selectedDocument
                    , draft                = if isSameSelected then restoredDraft else state.draft
                    , storedDrafts         = Map.delete targetId state.storedDrafts
                    , draftSyncState       = if isSameSelected then DraftSyncIdle else state.draftSyncState
                    , restoredDraftDocId   = if isSameSelected then Nothing else state.restoredDraftDocId
                    , staleDraftPrompt     = if isSameSelected then Nothing else state.staleDraftPrompt
                    , editorSyncVersion    = if isSameSelected then state.editorSyncVersion + 1 else state.editorSyncVersion
                    , editorStats          = if isSameSelected then computeTextStats (toText doc.body) else state.editorStats
                    , editorViewMode       = if isSameSelected then normalizeViewMode doc.format state.editorViewMode else state.editorViewMode
                    , canUndo              = not isSameSelected && state.canUndo
                    , canRedo              = not isSameSelected && state.canRedo
                    , revisionHistory      = nextRevisionHistory
                    , lastError            = Nothing
                    , statusNotice         = Just msgs.noticeRevisionRestored
                    }
        Left err ->
            state
                { restoringRevisionDoc = Nothing
                , revisionHistory      = state.revisionHistory { confirmRestoreRev = Nothing }
                , lastError            = Just (formatApiClientError msgs err)
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

-- | Miso の MVU 更新関数。純粋な状態更新・認証状態管理・API 呼び出し・CodeMirror 6 コマンド送出を調停する。
updateClient :: ClientEnv -> ClientAction -> Effect () () ClientState ClientAction
updateClient env action = case action of
    Initialize ->
        triggerSessionBootstrap
    RefreshAll -> do
        st <- get
        case st.authState of
            AuthAuthenticated{}
                | not st.isLoggingOut -> triggerRefreshWorkspace
                | otherwise           -> pure ()
            AuthChecking ->
                pure ()
            AuthUnauthenticated form
                | form.isSubmitting -> pure ()
                | otherwise         -> triggerSessionBootstrap
    HealthChecked eHealth -> do
        modify (applyHealthResult env.messages eHealth)
        case eHealth of
            Right _ -> do
                st <- get
                when (isJust (currentAuthSession st) && not (Map.null st.pendingSaves))
                    triggerSyncNext
            Left _ -> pure ()
    AuthSessionChecked seqNo eSession -> do
        stBefore <- get
        when (seqNo == stBefore.authSessionSeq) $ do
            modify (applyAuthSessionChecked env.messages seqNo eSession)
            case eSession of
                Right _ -> loadAuthenticatedWorkspace
                Left _  -> pure ()
    SwitchAuthMode nextMode ->
        modify $ \ st -> case st.authState of
            AuthUnauthenticated form
                | not form.isSubmitting ->
                    st
                        { authState = AuthUnauthenticated form
                            { mode                 = nextMode
                            , passwordConfirmInput = ""
                            , formError            = Nothing
                            }
                        }
            _ -> st
    UpdateAuthUsername txt ->
        updateAuthFormField (\ form -> form { usernameInput = txt, formError = Nothing })
    UpdateAuthDisplayName txt ->
        updateAuthFormField (\ form -> form { displayNameInput = txt, formError = Nothing })
    UpdateAuthPassword txt ->
        updateAuthFormField (\ form -> form { passwordInput = txt, formError = Nothing })
    UpdateAuthPasswordConfirm txt ->
        updateAuthFormField (\ form -> form { passwordConfirmInput = txt, formError = Nothing })
    SubmitAuthForm -> do
        st <- get
        case st.authState of
            AuthUnauthenticated form
                | not form.isSubmitting -> case form.mode of
                    AuthModeLogin ->
                        case validateLoginForm env.messages form of
                            Left errMsg ->
                                updateAuthFormField (\ f -> f { formError = Just errMsg })
                            Right loginReq -> do
                                let seqNo :: Int
                                    seqNo = st.authSessionSeq
                                modify $ \ s -> s
                                    { authState    = AuthUnauthenticated form
                                        { isSubmitting = True
                                        , formError    = Nothing
                                        }
                                    , lastError    = Nothing
                                    , statusNotice = Nothing
                                    }
                                io (AuthSubmitted seqNo AuthModeLogin <$> loginUser env.apiClient loginReq)
                    AuthModeRegister ->
                        case validateRegisterForm env.messages form of
                            Left errMsg ->
                                updateAuthFormField (\ f -> f { formError = Just errMsg })
                            Right regReq -> do
                                let seqNo :: Int
                                    seqNo = st.authSessionSeq
                                modify $ \ s -> s
                                    { authState    = AuthUnauthenticated form
                                        { isSubmitting = True
                                        , formError    = Nothing
                                        }
                                    , lastError    = Nothing
                                    , statusNotice = Nothing
                                    }
                                io (AuthSubmitted seqNo AuthModeRegister <$> registerUser env.apiClient regReq)
            _ -> pure ()
    AuthSubmitted seqNo submittedMode eSession -> do
        stBefore <- get
        when (seqNo == stBefore.authSessionSeq) $ do
            modify (applyAuthSubmitted env.messages seqNo submittedMode eSession)
            case eSession of
                Right _ -> loadAuthenticatedWorkspace
                Left _  -> pure ()
    RequestLogout -> do
        st <- get
        case st.authState of
            AuthAuthenticated{}
                | not st.isLoggingOut ->
                    if isCurrentDocumentDirty st
                        then modify (\ s -> s { pendingNavigation = Just NavLogout })
                        else executeLogout
            _ -> pure ()
    LoggedOut seqNo eUnit -> do
        st <- get
        when (seqNo == st.authSessionSeq) $ case eUnit of
            Right () -> do
                modify (resetWorkspaceForUnauthenticated emptyAuthFormState Nothing (Just env.messages.noticeLoggedOut))
                io_ (env.sendEditorCmd CmdResetEditor)
            Left err
                | isUnauthorizedError err -> do
                    modify (resetWorkspaceForUnauthenticated emptyAuthFormState Nothing (Just env.messages.noticeLoggedOut))
                    io_ (env.sendEditorCmd CmdResetEditor)
                | otherwise ->
                    modify $ \ s -> s
                        { isLoggingOut = False
                        , lastError    = Just (formatApiClientError env.messages err)
                        }
    DocumentsLoaded seqNo reqSeq eDocs ->
        guardAuthResponse seqNo eDocs $
            modify (applyDocumentsResult env.messages reqSeq eDocs)
    TagsLoaded seqNo eTags ->
        guardAuthResponse seqNo eTags $
            modify (applyTagsResult eTags)
    FoldersLoaded seqNo eFolders ->
        guardAuthResponse seqNo eFolders $
            modify (applyFoldersResult eFolders)
    RequestStatusFilter nextStatus -> do
        st <- get
        if isNothing (currentAuthSession st) || st.filterState.status == nextStatus
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
        if isNothing (currentAuthSession st)
            then pure ()
            else if fmap (.id) st.selectedDocument == Just docId
                then modify $ \ s -> s
                    { openingDocumentId = Nothing
                    , mobilePane        = PaneEditor
                    }
                else if isCurrentDocumentDirty st
                    then modify (\ s -> s { pendingNavigation = Just (NavOpenDocument docId) })
                    else executeNavigation (NavOpenDocument docId)
    DocumentLoaded seqNo docId eDoc ->
        guardAuthResponse seqNo eDoc $ do
            stBefore <- get
            case eDoc of
                Right doc -> do
                    let shouldCleanRedundant :: Bool
                        shouldCleanRedundant =
                            stBefore.openingDocumentId == Just docId
                                && reconcileDocumentWithDraft doc (Map.lookup doc.id stBefore.storedDrafts) == RedundantLocalDraft
                    modify (applyDocumentLoaded env.messages docId (Right doc))
                    cacheDocumentLocally doc
                    when shouldCleanRedundant (io_ (env.draftStorage.deleteStoredDraft docId))
                    syncEditorFromCurrentState False
                Left err
                    | isNetworkError err, Just cached <- Map.lookup docId stBefore.cachedDocuments -> do
                        modify $ \ s ->
                            (applyDocumentLoaded env.messages docId (Right cached.document) s)
                                { statusNotice = Just env.messages.noticeOpenedFromCache
                                }
                        syncEditorFromCurrentState False
                    | otherwise -> do
                        modify (applyDocumentLoaded env.messages docId (Left err))
                        syncEditorFromCurrentState False
    RequestCreateDocument -> do
        st <- get
        if isNothing (currentAuthSession st) || st.isCreatingDocument || st.isImportingDocument
            then pure ()
            else if isCurrentDocumentDirty st
                then modify (\ s -> s { pendingNavigation = Just NavCreateDocument })
                else executeNavigation NavCreateDocument
    DocumentCreated seqNo eDoc ->
        guardAuthResponse seqNo eDoc $ do
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
                Right doc -> do
                    cacheDocumentLocally doc
                    when shouldReloadList reloadDocumentsForCurrentFilter
                    io (TagsLoaded seqNo <$> listTags env.apiClient)
                    io (FoldersLoaded seqNo <$> listFolders env.apiClient)
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
                Just NavApplyConflictLocal{} -> True
                Just NavRecoverConflictAsNew{} -> True
                Just NavLogout               -> True
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
    DocumentSaved seqNo docId eDoc ->
        guardAuthResponse seqNo eDoc $ do
            stBefore <- get
            case eDoc of
                Right doc -> do
                    modify $ \ s ->
                        (applyDocumentSaved env.messages docId (Right doc) s)
                            { pendingSaves = Map.delete doc.id s.pendingSaves
                            }
                    cacheDocumentLocally doc
                    io_ (env.offlineStore.deletePendingSave doc.id)
                    stAfter <- get
                    if fmap (.id) stAfter.selectedDocument == Just doc.id && isDraftDirty doc stAfter.draft
                        then syncLocalDraftForCurrentState
                        else io_ (env.draftStorage.deleteStoredDraft doc.id)
                    when (stAfter.revisionHistory.isHistoryOpen && stAfter.revisionHistory.documentId == Just doc.id) $
                        loadRevisionListForDocument doc.id False
                    io (TagsLoaded seqNo <$> listTags env.apiClient)
                    io (FoldersLoaded seqNo <$> listFolders env.apiClient)
                Left err
                    | isNetworkError err, Just (inFlightId, sentDraft) <- stBefore.savingDocument, inFlightId == docId -> do
                        modify $ \ s -> s
                            { savingDocument = Nothing
                            , networkStatus  = NetworkOffline
                            }
                        queueOfflineDraftSave docId sentDraft
                    | isConflictError err, Just (inFlightId, sentDraft) <- stBefore.savingDocument, inFlightId == docId -> do
                        modify (\ s -> s { savingDocument = Nothing, lastError = Nothing })
                        stashSaveConflictFromDraft docId sentDraft env.messages.errSyncConflictReason
                    | otherwise ->
                        modify (applyDocumentSaved env.messages docId (Left err))
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
        case currentAuthSession st of
            Just session
                | isNothing st.mutatingDocumentId
                    && isNothing st.savingDocument
                    && isNothing st.restoringRevisionDoc
                    && st.syncingDocId /= Just docId -> do
                    modify $ \ s -> s
                        { mutatingDocumentId = Just docId
                        , lastError          = Nothing
                        }
                    io (DocumentSoftDeleted st.authSessionSeq docId <$> softDeleteDocument env.apiClient session.csrfToken docId)
            _ -> pure ()
    DocumentSoftDeleted seqNo docId eDoc ->
        guardAuthResponse seqNo eDoc $ do
            stBefore <- get
            let wasSelected :: Bool
                wasSelected = fmap (.id) stBefore.selectedDocument == Just docId
            modify (applyDocumentSoftDeleted env.messages docId eDoc)
            case eDoc of
                Right doc -> do
                    cacheDocumentLocally doc
                    modify (\ s -> s { pendingSaves = Map.delete docId s.pendingSaves })
                    io_ (env.offlineStore.deletePendingSave docId)
                    io_ (env.draftStorage.deleteStoredDraft docId)
                    when wasSelected (io_ (env.sendEditorCmd CmdResetEditor))
                    io (TagsLoaded seqNo <$> listTags env.apiClient)
                    io (FoldersLoaded seqNo <$> listFolders env.apiClient)
                Left _ -> pure ()
    RestoreDocument docId -> do
        st <- get
        case currentAuthSession st of
            Just session
                | isNothing st.mutatingDocumentId && isNothing st.restoringRevisionDoc -> do
                    modify $ \ s -> s
                        { mutatingDocumentId = Just docId
                        , lastError          = Nothing
                        }
                    io (DocumentRestored st.authSessionSeq docId <$> restoreDocument env.apiClient session.csrfToken docId)
            _ -> pure ()
    DocumentRestored seqNo docId eDoc ->
        guardAuthResponse seqNo eDoc $ do
            modify (applyDocumentRestored env.messages docId eDoc)
            syncEditorFromCurrentState False
            case eDoc of
                Right doc -> do
                    cacheDocumentLocally doc
                    io (TagsLoaded seqNo <$> listTags env.apiClient)
                    io (FoldersLoaded seqNo <$> listFolders env.apiClient)
                Left _ -> pure ()
    RequestPermanentDelete docId ->
        modify (\ st -> st { confirmPermanentDelete = Just docId })
    CancelPermanentDelete ->
        modify (\ st -> st { confirmPermanentDelete = Nothing })
    ConfirmPermanentDelete docId -> do
        st <- get
        case currentAuthSession st of
            Just session
                | isNothing st.mutatingDocumentId && isNothing st.restoringRevisionDoc -> do
                    modify $ \ s -> s
                        { mutatingDocumentId = Just docId
                        , lastError          = Nothing
                        }
                    io (DocumentPermanentlyDeleted st.authSessionSeq docId <$> permanentDeleteDocument env.apiClient session.csrfToken docId)
            _ -> pure ()
    DocumentPermanentlyDeleted seqNo docId eUnit ->
        guardAuthResponse seqNo eUnit $ do
            stBefore <- get
            let wasSelected :: Bool
                wasSelected = fmap (.id) stBefore.selectedDocument == Just docId
            modify (applyDocumentPermanentlyDeleted env.messages docId eUnit)
            case eUnit of
                Right () -> do
                    modify $ \ s -> s
                        { cachedDocuments = Map.delete docId s.cachedDocuments
                        , pendingSaves    = Map.delete docId s.pendingSaves
                        , syncConflicts   = Map.delete docId s.syncConflicts
                        }
                    io_ (env.offlineStore.deleteCachedDocument docId)
                    io_ (env.offlineStore.deletePendingSave docId)
                    io_ (env.offlineStore.deleteSyncConflict docId)
                    io_ (env.draftStorage.deleteStoredDraft docId)
                    when wasSelected (io_ (env.sendEditorCmd CmdResetEditor))
                Left _ -> pure ()
    ApplyExtension extId -> do
        st <- get
        let hasExt :: Bool
            hasExt = any (\ ext -> ext.metadata.extensionId == extId) env.extensions
        case st.selectedDocument of
            Just doc
                | not (isTrashed doc)
                    && hasExt
                    && Set.member extId st.enabledExtensions
                    && isNothing st.savingDocument
                    && isNothing st.mutatingDocumentId
                    && isNothing st.restoringRevisionDoc -> do
                modify $ \ s ->
                    (applyExtensionById env.extensions extId s)
                        { statusNotice = Just env.messages.noticeExtensionApplied
                        }
                syncLocalDraftForCurrentState
                syncEditorFromCurrentState False
            _ -> pure ()
    ToggleExtensionsPanel -> do
        st <- get
        case st.selectedDocument of
            Nothing -> pure ()
            Just _ ->
                if st.extensionsPanelOpen
                    then modify (\ s -> s { extensionsPanelOpen = False })
                    else modify $ \ s -> s
                        { extensionsPanelOpen = True
                        , revisionHistory     = resetRevisionHistory s.revisionHistory
                        }
    ToggleExtensionEnabled extId ->
        modify $ \ s ->
            let known :: Bool
                known = any (\ entry -> entry.metadata.extensionId == extId) s.extensionCatalog

                nextEnabled :: Set ExtensionId
                nextEnabled
                    | not known                            = s.enabledExtensions
                    | Set.member extId s.enabledExtensions = Set.delete extId s.enabledExtensions
                    | otherwise                            = Set.insert extId s.enabledExtensions
            in  s { enabledExtensions = nextEnabled }
    RunExtensionCommand extId cmdId -> do
        st <- get
        case st.selectedDocument of
            Just doc
                | not (isTrashed doc)
                    && isNothing st.savingDocument
                    && isNothing st.mutatingDocumentId
                    && isNothing st.restoringRevisionDoc ->
                    if not (Set.member extId st.enabledExtensions)
                        then modify $ \ s -> s
                            { lastError    = Just env.messages.errExtensionNotEnabled
                            , statusNotice = Nothing
                            }
                        else case applyExtensionCommand env.extensions extId cmdId st of
                            Left err ->
                                modify $ \ s -> s
                                    { lastError    = Just (formatExtensionError env.messages err)
                                    , statusNotice = Nothing
                                    }
                            Right (nextState, mSummary) -> do
                                let noticeMsg :: Text
                                    noticeMsg = fromMaybe env.messages.noticeExtensionApplied mSummary
                                modify $ const nextState
                                    { lastError    = Nothing
                                    , statusNotice = Just noticeMsg
                                    }
                                syncLocalDraftForCurrentState
                                syncEditorFromCurrentState False
            _ -> pure ()
    DraftsLoaded seqNo eDrafts -> do
        stBefore <- get
        case currentUserId stBefore of
            Just uid | seqNo == stBefore.authSessionSeq -> do
                modify (applyDraftsLoaded env.messages eDrafts)
                case eDrafts of
                    Left _ -> pure ()
                    Right drafts -> do
                        let ownedDrafts :: [StoredDraft]
                            ownedDrafts = filterDraftsForOwner uid drafts

                            loadedMap :: Map DocumentId StoredDraft
                            loadedMap = Map.fromList [ (sd.docId, sd) | sd <- ownedDrafts ]
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
                                    for_ (latestStoredDraft ownedDrafts) $ \ latest -> do
                                        modify $ \ s -> s
                                            { openingDocumentId  = Just latest.docId
                                            , autoOpeningDraftId = Just latest.docId
                                            , mobilePane         = PaneEditor
                                            }
                                        io (DocumentLoaded seqNo latest.docId <$> getDocument env.apiClient latest.docId)
                            _ -> pure ()
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
        case (currentAuthSession st, st.orphanedDraft) of
            (Just _, Just _)
                | st.isCreatingDocument || st.isImportingDocument -> pure ()
                | isCurrentDocumentDirty st ->
                    modify (\ s -> s { pendingNavigation = Just NavRecoverOrphanedDraft })
                | otherwise ->
                    executeNavigation NavRecoverOrphanedDraft
            _ -> pure ()
    OrphanDraftRecovered seqNo oldDocId eDoc ->
        guardAuthResponse seqNo eDoc $ do
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
                    io (TagsLoaded seqNo <$> listTags env.apiClient)
                    io (FoldersLoaded seqNo <$> listFolders env.apiClient)
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
        if isNothing (currentAuthSession st) || st.isImportingDocument || st.isCreatingDocument
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
            case currentAuthSession st of
                Just session
                    | not st.isImportingDocument && not st.isCreatingDocument -> do
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
                                io (DocumentImported st.authSessionSeq imported.fileName <$> createDocument env.apiClient session.csrfToken createReq)
                _ -> pure ()
    DocumentImported seqNo fileName eDoc ->
        guardAuthResponse seqNo eDoc $ do
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
                Right doc -> do
                    cacheDocumentLocally doc
                    when shouldReloadList reloadDocumentsForCurrentFilter
                    io (TagsLoaded seqNo <$> listTags env.apiClient)
                    io (FoldersLoaded seqNo <$> listFolders env.apiClient)
                    io_ (env.sendEditorCmd CmdFocus)
                Left _ -> pure ()
    ToggleRevisionHistory -> do
        st <- get
        case st.selectedDocument of
            Nothing -> pure ()
            Just doc ->
                if st.revisionHistory.isHistoryOpen
                    then modify $ \ s -> s
                        { revisionHistory = resetRevisionHistory s.revisionHistory
                        }
                    else do
                        modify $ \ s -> s
                            { extensionsPanelOpen = False
                            , revisionHistory     = (resetRevisionHistory s.revisionHistory)
                                { isHistoryOpen = True
                                , documentId    = Just doc.id
                                }
                            , lastError = Nothing
                            }
                        loadRevisionListForDocument doc.id False
    RefreshRevisionHistory -> do
        st <- get
        case st.selectedDocument of
            Just doc | st.revisionHistory.isHistoryOpen ->
                loadRevisionListForDocument doc.id False
            _ -> pure ()
    LoadMoreRevisions -> do
        st <- get
        case st.selectedDocument of
            Just doc
                | st.revisionHistory.isHistoryOpen
                    && not st.revisionHistory.isLoadingList
                    && not st.revisionHistory.isLoadingMore
                    && length st.revisionHistory.revisions < st.revisionHistory.totalRevisions ->
                    loadRevisionListForDocument doc.id True
            _ -> pure ()
    RevisionListLoaded seqNo reqSeq docId isAppend eList ->
        guardAuthResponse seqNo eList $ do
            stBefore <- get
            let prevDetail :: Maybe RevisionDetail
                prevDetail = stBefore.revisionHistory.revisionDetail
            modify (applyRevisionListLoaded env.messages reqSeq docId isAppend eList)
            stAfter <- get
            case eList of
                Right _
                    | stAfter.revisionHistory.isHistoryOpen
                        && stAfter.revisionHistory.documentId == Just docId ->
                        case stAfter.revisionHistory.selectedRevision of
                            Just revNo
                                | fmap (\ d -> (d.documentId, d.revision)) prevDetail /= Just (docId, revNo)
                                    || isNothing stAfter.revisionHistory.revisionDetail ->
                                    loadRevisionDetailForDocument docId revNo
                            _ -> pure ()
                _ -> pure ()
    SelectRevision revNo -> do
        st <- get
        case st.selectedDocument of
            Just doc | st.revisionHistory.isHistoryOpen -> do
                modify $ \ s -> s
                    { revisionHistory = s.revisionHistory
                        { selectedRevision  = Just revNo
                        , confirmRestoreRev = Nothing
                        }
                    }
                loadRevisionDetailForDocument doc.id revNo
            _ -> pure ()
    RevisionDetailLoaded seqNo reqSeq docId revNo eDetail ->
        guardAuthResponse seqNo eDetail $
            modify (applyRevisionDetailLoaded env.messages reqSeq docId revNo eDetail)
    SetRevisionDetailTab nextTab -> do
        st <- get
        case nextTab of
            RevTabCompare -> do
                let rh :: RevisionHistoryState
                    rh = st.revisionHistory

                    (defFrom, defTo) = defaultComparePair rh.selectedRevision rh.revisions

                    nextFrom :: Maybe RevisionNumber
                    nextFrom = rh.compareFromRev <|> defFrom

                    nextTo :: Maybe RevisionNumber
                    nextTo = rh.compareToRev <|> defTo
                modify $ \ s -> s
                    { revisionHistory = s.revisionHistory
                        { activeTab      = RevTabCompare
                        , compareFromRev = nextFrom
                        , compareToRev   = nextTo
                        }
                    }
                case (st.selectedDocument, nextFrom, nextTo) of
                    (Just doc, Just fromRev, Just toRev)
                        | isNothing rh.compareResult -> loadRevisionCompare doc.id fromRev toRev
                    _ -> pure ()
            _ ->
                modify $ \ s -> s
                    { revisionHistory = s.revisionHistory { activeTab = nextTab }
                    }
    SetCompareFromRevision revNo ->
        modify $ \ s -> s
            { revisionHistory = s.revisionHistory { compareFromRev = Just revNo }
            }
    SetCompareToRevision revNo ->
        modify $ \ s -> s
            { revisionHistory = s.revisionHistory { compareToRev = Just revNo }
            }
    CompareRevisions -> do
        st <- get
        case (st.selectedDocument, st.revisionHistory.compareFromRev, st.revisionHistory.compareToRev) of
            (Just doc, Just fromRev, Just toRev) | st.revisionHistory.isHistoryOpen ->
                loadRevisionCompare doc.id fromRev toRev
            _ -> pure ()
    RevisionsCompared seqNo reqSeq docId eCmp ->
        guardAuthResponse seqNo eCmp $
            modify (applyRevisionCompareLoaded env.messages reqSeq docId eCmp)
    RequestRestoreRevision revNo -> do
        st <- get
        case st.selectedDocument of
            Just doc
                | not (isTrashed doc)
                    && isNothing st.restoringRevisionDoc
                    && isNothing st.mutatingDocumentId
                    && isNothing st.savingDocument
                    && st.syncingDocId /= Just doc.id
                    && revNo /= doc.revision ->
                    modify $ \ s -> s
                        { revisionHistory = s.revisionHistory { confirmRestoreRev = Just revNo }
                        }
            _ -> pure ()
    CancelRestoreRevision ->
        modify $ \ s -> s
            { revisionHistory = s.revisionHistory { confirmRestoreRev = Nothing }
            }
    ConfirmRestoreRevision revNo -> do
        st <- get
        case (currentAuthSession st, st.selectedDocument) of
            (Just session, Just doc)
                | not (isTrashed doc)
                    && isNothing st.restoringRevisionDoc
                    && isNothing st.mutatingDocumentId
                    && isNothing st.savingDocument
                    && st.syncingDocId /= Just doc.id -> do
                    let req :: RestoreRevisionRequest
                        req = RestoreRevisionRequest
                            { baseRevision = Just doc.revision
                            }
                    modify $ \ s -> s
                        { restoringRevisionDoc = Just doc.id
                        , revisionHistory      = s.revisionHistory { confirmRestoreRev = Nothing }
                        , lastError            = Nothing
                        , statusNotice         = Nothing
                        }
                    io (RevisionRestored st.authSessionSeq doc.id <$> restoreDocumentRevision env.apiClient session.csrfToken doc.id revNo req)
            _ -> pure ()
    RevisionRestored seqNo docId eDoc ->
        guardAuthResponse seqNo eDoc $ do
            stBefore <- get
            when (stBefore.restoringRevisionDoc == Just docId) $ do
                modify (applyRevisionRestored env.messages docId eDoc)
                case eDoc of
                    Right doc -> do
                        cacheDocumentLocally doc
                        io_ (env.draftStorage.deleteStoredDraft doc.id)
                        stAfter <- get
                        when (fmap (.id) stAfter.selectedDocument == Just doc.id) $ do
                            syncEditorFromCurrentState True
                            when (stAfter.revisionHistory.isHistoryOpen && stAfter.revisionHistory.documentId == Just doc.id) $
                                loadRevisionListForDocument doc.id False
                    Left _ -> pure ()
    NetworkStatusChanged nextStatus -> do
        modify (applyNetworkStatusChanged nextStatus)
        case nextStatus of
            NetworkOffline -> pure ()
            NetworkOnline -> do
                io (HealthChecked <$> fetchHealth env.apiClient)
                st <- get
                when (isJust (currentAuthSession st) && not (Map.null st.pendingSaves))
                    triggerSyncNext
    OfflineStoreLoaded seqNo eCached ePending eConflicts -> do
        st <- get
        when (seqNo == st.authSessionSeq && isJust (currentAuthSession st)) $ do
            modify (applyOfflineStoreLoaded eCached ePending eConflicts)
            stAfter <- get
            when (stAfter.networkStatus == NetworkOnline && not (Map.null stAfter.pendingSaves))
                triggerSyncNext
    TriggerSyncQueue ->
        triggerSyncNext
    SyncItemCompleted seqNo docId eDoc ->
        guardAuthResponse seqNo eDoc $ do
            stBefore <- get
            when (stBefore.syncingDocId == Just docId) $ case eDoc of
                Right doc -> do
                    let isSameSelected :: Bool
                        isSameSelected = fmap (.id) stBefore.selectedDocument == Just doc.id

                        sentDraft :: EditorDraft
                        sentDraft = case Map.lookup doc.id stBefore.pendingSaves of
                            Just ps -> draftFromPendingSave ps
                            Nothing -> draftFromDocument doc

                        mergedDraft :: EditorDraft
                        mergedDraft =
                            if isSameSelected
                                then mergeDraftAfterSave sentDraft (draftFromDocument doc) stBefore.draft
                                else stBefore.draft

                        stillDirty :: Bool
                        stillDirty = isSameSelected && isDraftDirty doc mergedDraft
                    modify $ \ s ->
                        let nextPending :: Map DocumentId PendingSyncSave
                            nextPending = Map.delete doc.id s.pendingSaves

                            summary :: DocumentSummary
                            summary = toDocumentSummary doc

                            nextSummaries :: [DocumentSummary]
                            nextSummaries = case s.filterState.status of
                                StatusTrashed -> s.documentSummaries
                                _             ->
                                    if any (\ item -> item.id == doc.id) s.documentSummaries
                                        then summary : filter (\ item -> item.id /= doc.id) s.documentSummaries
                                        else s.documentSummaries
                        in  s
                                { syncingDocId       = Nothing
                                , pendingSaves       = nextPending
                                , documentSummaries  = nextSummaries
                                , selectedDocument   = if isSameSelected then Just doc else s.selectedDocument
                                , draft              = mergedDraft
                                , editorSyncVersion  = if isSameSelected && not stillDirty then s.editorSyncVersion + 1 else s.editorSyncVersion
                                , editorStats        = if isSameSelected && not stillDirty then computeTextStats (toText doc.body) else s.editorStats
                                , connectionStatus   = ConnOnline
                                , networkStatus      = NetworkOnline
                                , lastError          = Nothing
                                , statusNotice       = if Map.null nextPending then Just env.messages.noticeSyncCompleted else s.statusNotice
                                }
                    cacheDocumentLocally doc
                    io_ (env.offlineStore.deletePendingSave doc.id)
                    when isSameSelected $
                        if stillDirty
                            then syncLocalDraftForCurrentState
                            else syncEditorFromCurrentState False
                    stAfter <- get
                    if Map.null stAfter.pendingSaves
                        then do
                            reloadDocumentsForCurrentFilter
                            io (TagsLoaded seqNo <$> listTags env.apiClient)
                            io (FoldersLoaded seqNo <$> listFolders env.apiClient)
                        else triggerSyncNext
                Left err
                    | isNetworkError err ->
                        modify $ \ s -> s
                            { syncingDocId  = Nothing
                            , networkStatus = NetworkOffline
                            }
                    | isConflictError err || isNotFoundError err -> do
                        let reasonText :: Text
                            reasonText =
                                if isNotFoundError err
                                    then env.messages.errSyncNotFoundReason
                                    else env.messages.errSyncConflictReason
                        case Map.lookup docId stBefore.pendingSaves of
                            Nothing ->
                                modify (\ s -> s { syncingDocId = Nothing })
                            Just pendingItem -> do
                                let provisionalConflict :: SyncConflict
                                    provisionalConflict = SyncConflict
                                        { docId      = docId
                                        , ownerId    = pendingItem.ownerId
                                        , localSave  = pendingItem
                                        , detectedAt = pendingItem.queuedAt
                                        , reason     = reasonText
                                        }
                                modify $ \ s -> s
                                    { syncingDocId  = Nothing
                                    , pendingSaves  = Map.delete docId s.pendingSaves
                                    , syncConflicts = Map.insert docId provisionalConflict s.syncConflicts
                                    , lastError     = Nothing
                                    }
                                io_ $ do
                                    now <- env.getCurrentUtcTime
                                    env.offlineStore.deletePendingSave docId
                                    env.offlineStore.putSyncConflict provisionalConflict { detectedAt = now }
                        stAfter <- get
                        if Map.null stAfter.pendingSaves
                            then reloadDocumentsForCurrentFilter
                            else triggerSyncNext
                    | otherwise ->
                        modify $ \ s -> s
                            { syncingDocId = Nothing
                            , lastError    = Just (formatApiClientError env.messages err)
                            }
    ApplyConflictLocalCopy docId -> do
        st <- get
        case (currentAuthSession st, Map.lookup docId st.syncConflicts) of
            (Just _, Just _) ->
                if isCurrentDocumentDirty st
                    then modify (\ s -> s { pendingNavigation = Just (NavApplyConflictLocal docId) })
                    else executeNavigation (NavApplyConflictLocal docId)
            _ -> pure ()
    ConflictLocalApplyLoaded seqNo docId eDoc ->
        guardAuthResponse seqNo eDoc $ do
            st <- get
            case Map.lookup docId st.syncConflicts of
                Nothing -> pure ()
                Just conflict -> case eDoc of
                    Right serverDoc -> do
                        cacheDocumentLocally serverDoc
                        applyConflictToEditor serverDoc conflict
                    Left _ ->
                        case fmap (.document) (Map.lookup docId st.cachedDocuments) of
                            Just cachedDoc -> applyConflictToEditor cachedDoc conflict
                            Nothing        -> pure ()
    RecoverConflictAsNewDocument docId -> do
        st <- get
        case (currentAuthSession st, Map.lookup docId st.syncConflicts) of
            (Just _, Just _)
                | not st.isCreatingDocument && not st.isImportingDocument ->
                    if isCurrentDocumentDirty st
                        then modify (\ s -> s { pendingNavigation = Just (NavRecoverConflictAsNew docId) })
                        else executeNavigation (NavRecoverConflictAsNew docId)
            _ -> pure ()
    ConflictRecoveredAsNew seqNo oldDocId eDoc ->
        guardAuthResponse seqNo eDoc $ do
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
                            { syncConflicts = Map.delete oldDocId base.syncConflicts
                            , filterState   = if shouldReloadList then defaultFilterState else base.filterState
                            , statusNotice  = Just env.messages.noticeConflictRecovered
                            }
                        Left _ -> base
            syncEditorFromCurrentState False
            case eDoc of
                Right doc -> do
                    cacheDocumentLocally doc
                    io_ (env.offlineStore.deleteSyncConflict oldDocId)
                    when shouldReloadList reloadDocumentsForCurrentFilter
                    io (TagsLoaded seqNo <$> listTags env.apiClient)
                    io (FoldersLoaded seqNo <$> listFolders env.apiClient)
                    io_ (env.sendEditorCmd CmdFocus)
                Left _ -> pure ()
    DismissSyncConflict docId -> do
        modify $ \ s -> s
            { syncConflicts = Map.delete docId s.syncConflicts
            , conflictMerge = case s.conflictMerge of
                Just (MergeLoading _ mid) | mid == docId      -> Nothing
                Just (MergeReady ms)      | ms.docId == docId -> Nothing
                other                                         -> other
            , statusNotice  = Just env.messages.noticeConflictDismissed
            }
        io_ (env.offlineStore.deleteSyncConflict docId)
    OpenConflictMerge docId -> do
        st <- get
        case (currentAuthSession st, Map.lookup docId st.syncConflicts) of
            (Just _, Just conflict) | st.networkStatus == NetworkOnline && isNothing st.conflictMerge -> do
                let nextSeq :: Int
                    nextSeq = st.mergeRequestSeq + 1

                    authSeq :: Int
                    authSeq = st.authSessionSeq
                modify $ \ s -> s
                    { mergeRequestSeq = nextSeq
                    , conflictMerge   = Just (MergeLoading nextSeq docId)
                    , lastError       = Nothing
                    }
                io $ do
                    eServerDoc <- getDocument env.apiClient docId
                    eBaseRev   <- getDocumentRevision env.apiClient docId conflict.localSave.baseRevision
                    pure (ConflictMergeLoaded authSeq nextSeq docId eServerDoc eBaseRev)
            _ -> pure ()
    ConflictMergeLoaded seqNo reqSeq docId eServerDoc eBaseRev -> do
        let firstAuthErr :: Either ApiClientError ()
            firstAuthErr = case (eServerDoc, eBaseRev) of
                (Left err, _) | isUnauthorizedError err -> Left err
                (_, Left err) | isUnauthorizedError err -> Left err
                _                                       -> Right ()
        guardAuthResponse seqNo firstAuthErr $ do
            st <- get
            case (st.conflictMerge, Map.lookup docId st.syncConflicts) of
                (Just (MergeLoading activeSeq activeDocId), Just conflict)
                    | activeSeq == reqSeq && activeDocId == docId ->
                        case (eServerDoc, eBaseRev) of
                            (Right serverDoc, Right baseDetail) -> do
                                let outcome :: MergeOutcome
                                    outcome = mergeThreeWay
                                        (toText baseDetail.body)
                                        (toText conflict.localSave.body)
                                        (toText serverDoc.body)

                                    session :: MergeSession
                                    session = MergeSession
                                        { docId          = docId
                                        , baseRevision   = conflict.localSave.baseRevision
                                        , serverDocument = serverDoc
                                        , localSave      = conflict.localSave
                                        , baseBody       = baseDetail.body
                                        , mergeOutcome   = outcome
                                        , choices        = Map.empty
                                        , isSavingMerge  = False
                                        }
                                modify $ \ s -> s
                                    { conflictMerge = Just (MergeReady session)
                                    , lastError     = Nothing
                                    }
                            (Left err, _) ->
                                modify $ \ s -> s
                                    { conflictMerge = Nothing
                                    , lastError     = Just (formatApiClientError env.messages err)
                                    }
                            (_, Left err) ->
                                modify $ \ s -> s
                                    { conflictMerge = Nothing
                                    , lastError     = Just (formatApiClientError env.messages err)
                                    }
                _ -> pure ()
    SelectMergeChoice idx choice ->
        modify $ \ s -> case s.conflictMerge of
            Just (MergeReady ms) | not ms.isSavingMerge ->
                s
                    { conflictMerge = Just (MergeReady ms { choices = Map.insert idx choice ms.choices })
                    , lastError     = Nothing
                    }
            _ -> s
    SelectAllMergeChoices choice ->
        modify $ \ s -> case s.conflictMerge of
            Just (MergeReady ms) | not ms.isSavingMerge ->
                let allChoices :: Map Int ConflictChoice
                    allChoices = Map.fromList [ (i, choice) | i <- [0 .. ms.mergeOutcome.conflictCount - 1] ]
                in  s
                        { conflictMerge = Just (MergeReady ms { choices = allChoices })
                        , lastError     = Nothing
                        }
            _ -> s
    SaveResolvedMerge -> do
        st <- get
        case (currentAuthSession st, st.conflictMerge) of
            (Just session, Just (MergeReady ms)) | not ms.isSavingMerge ->
                case resolveMergeOutcome ms.choices ms.mergeOutcome of
                    Left _ ->
                        modify (\ s -> s { lastError = Just env.messages.errMergeUnresolved })
                    Right mergedText ->
                        case validateDocumentBody (DocumentBody mergedText) of
                            Left vErr ->
                                modify (\ s -> s { lastError = Just (formatValidationError env.messages vErr) })
                            Right validBody -> do
                                let updateReq :: UpdateDocumentRequest
                                    updateReq = UpdateDocumentRequest
                                        { title        = ms.localSave.title
                                        , body         = validBody
                                        , format       = ms.localSave.format
                                        , folder       = ms.localSave.folder
                                        , tags         = ms.localSave.tags
                                        , baseRevision = Just ms.serverDocument.revision
                                        }
                                modify $ \ s -> s
                                    { conflictMerge = Just (MergeReady ms { isSavingMerge = True })
                                    , lastError     = Nothing
                                    }
                                io (ConflictMergeSaved st.authSessionSeq ms.docId <$> updateDocument env.apiClient session.csrfToken ms.docId updateReq)
            _ -> pure ()
    ConflictMergeSaved seqNo docId eDoc ->
        guardAuthResponse seqNo eDoc $ do
            stBefore <- get
            case eDoc of
                Right doc -> do
                    let shouldOpenOrSync :: Bool
                        shouldOpenOrSync =
                            isNothing stBefore.selectedDocument
                                || fmap (.id) stBefore.selectedDocument == Just doc.id
                    modify $ \ s ->
                        let summary :: DocumentSummary
                            summary = toDocumentSummary doc

                            nextSummaries :: [DocumentSummary]
                            nextSummaries = case s.filterState.status of
                                StatusTrashed -> s.documentSummaries
                                _             ->
                                    if any (\ item -> item.id == doc.id) s.documentSummaries
                                        then summary : filter (\ item -> item.id /= doc.id) s.documentSummaries
                                        else s.documentSummaries
                        in  s
                                { conflictMerge      = Nothing
                                , syncConflicts      = Map.delete doc.id s.syncConflicts
                                , pendingSaves       = Map.delete doc.id s.pendingSaves
                                , storedDrafts       = Map.delete doc.id s.storedDrafts
                                , documentSummaries  = nextSummaries
                                , selectedDocument   = if shouldOpenOrSync then Just doc else s.selectedDocument
                                , draft              = if shouldOpenOrSync then draftFromDocument doc else s.draft
                                , draftSyncState     = if shouldOpenOrSync then DraftSyncIdle else s.draftSyncState
                                , restoredDraftDocId = if shouldOpenOrSync then Nothing else s.restoredDraftDocId
                                , staleDraftPrompt   = if shouldOpenOrSync then Nothing else s.staleDraftPrompt
                                , editorSyncVersion  = if shouldOpenOrSync then s.editorSyncVersion + 1 else s.editorSyncVersion
                                , editorStats        = if shouldOpenOrSync then computeTextStats (toText doc.body) else s.editorStats
                                , editorViewMode     = if shouldOpenOrSync then normalizeViewMode doc.format s.editorViewMode else s.editorViewMode
                                , canUndo            = not shouldOpenOrSync && s.canUndo
                                , canRedo            = not shouldOpenOrSync && s.canRedo
                                , lastError          = Nothing
                                , statusNotice       = Just env.messages.noticeMergeSaved
                                }
                    cacheDocumentLocally doc
                    io_ $ do
                        env.offlineStore.deleteSyncConflict doc.id
                        env.offlineStore.deletePendingSave doc.id
                        env.draftStorage.deleteStoredDraft doc.id
                    when shouldOpenOrSync (syncEditorFromCurrentState True)
                    stAfter <- get
                    when (stAfter.revisionHistory.isHistoryOpen && stAfter.revisionHistory.documentId == Just doc.id) $
                        loadRevisionListForDocument doc.id False
                    reloadDocumentsForCurrentFilter
                    io (TagsLoaded seqNo <$> listTags env.apiClient)
                    io (FoldersLoaded seqNo <$> listFolders env.apiClient)
                Left err ->
                    modify $ \ s -> s
                        { conflictMerge = case s.conflictMerge of
                            Just (MergeReady ms) | ms.docId == docId -> Just (MergeReady ms { isSavingMerge = False })
                            other                                    -> other
                        , lastError     = Just (formatApiClientError env.messages err)
                        }
    CloseConflictMerge ->
        modify $ \ s -> case s.conflictMerge of
            Just (MergeReady ms) | ms.isSavingMerge -> s
            _ -> s
                { conflictMerge   = Nothing
                , mergeRequestSeq = s.mergeRequestSeq + 1
                }
    DismissError ->
        modify dismissError
    DismissNotice ->
        modify dismissNotice
  where
    triggerSessionBootstrap :: Effect () () ClientState ClientAction
    triggerSessionBootstrap = do
        st <- get
        let nextAuthSeq :: Int
            nextAuthSeq = st.authSessionSeq + 1
        modify $ \ s -> s
            { connectionStatus = ConnChecking
            , authState        = AuthChecking
            , authSessionSeq   = nextAuthSeq
            , lastError        = Nothing
            }
        io $ do
            online <- env.offlineStore.isBrowserOnline
            pure (NetworkStatusChanged (if online then NetworkOnline else NetworkOffline))
        io (HealthChecked <$> fetchHealth env.apiClient)
        io (AuthSessionChecked nextAuthSeq <$> getCurrentUser env.apiClient)

    loadAuthenticatedWorkspace :: Effect () () ClientState ClientAction
    loadAuthenticatedWorkspace = do
        st <- get
        case currentAuthSession st of
            Nothing -> pure ()
            Just _ -> do
                let authSeq :: Int
                    authSeq = st.authSessionSeq

                    nextListSeq :: Int
                    nextListSeq = st.listRequestSeq + 1
                modify $ \ s -> s
                    { listRequestSeq     = nextListSeq
                    , isLoadingDocuments = True
                    }
                io (DocumentsLoaded authSeq nextListSeq <$> listDocuments env.apiClient defaultDocumentListQuery)
                io (TagsLoaded authSeq <$> listTags env.apiClient)
                io (FoldersLoaded authSeq <$> listFolders env.apiClient)
                io (DraftsLoaded authSeq <$> env.draftStorage.listStoredDrafts)
                io $ do
                    eCached  <- env.offlineStore.listCachedDocuments
                    ePending <- env.offlineStore.listPendingSaves
                    OfflineStoreLoaded authSeq eCached ePending <$> env.offlineStore.listSyncConflicts

    triggerRefreshWorkspace :: Effect () () ClientState ClientAction
    triggerRefreshWorkspace = do
        st <- get
        case currentAuthSession st of
            Nothing -> pure ()
            Just _ -> case buildListQueryFromFilter st.filterState of
                Left vErr ->
                    modify (\ s -> s { lastError = Just (formatValidationError env.messages vErr) })
                Right listQuery -> do
                    let authSeq :: Int
                        authSeq = st.authSessionSeq

                        nextSeq :: Int
                        nextSeq = st.listRequestSeq + 1
                    modify $ \ s -> s
                        { connectionStatus   = ConnChecking
                        , listRequestSeq     = nextSeq
                        , isLoadingDocuments = True
                        , lastError          = Nothing
                        }
                    io (HealthChecked <$> fetchHealth env.apiClient)
                    io (DocumentsLoaded authSeq nextSeq <$> listDocuments env.apiClient listQuery)
                    io (TagsLoaded authSeq <$> listTags env.apiClient)
                    io (FoldersLoaded authSeq <$> listFolders env.apiClient)

    reloadDocumentsForCurrentFilter :: Effect () () ClientState ClientAction
    reloadDocumentsForCurrentFilter = do
        st <- get
        case currentAuthSession st of
            Nothing -> pure ()
            Just _ -> case buildListQueryFromFilter st.filterState of
                Left vErr ->
                    modify (\ s -> s { lastError = Just (formatValidationError env.messages vErr) })
                Right listQuery -> do
                    let authSeq :: Int
                        authSeq = st.authSessionSeq

                        nextSeq :: Int
                        nextSeq = st.listRequestSeq + 1
                    modify $ \ s -> s
                        { listRequestSeq     = nextSeq
                        , isLoadingDocuments = True
                        , lastError          = Nothing
                        }
                    io (DocumentsLoaded authSeq nextSeq <$> listDocuments env.apiClient listQuery)

    guardAuthResponse ::
           Int
        -> Either ApiClientError a
        -> Effect () () ClientState ClientAction
        -> Effect () () ClientState ClientAction
    guardAuthResponse seqNo eRes onAuthorized = do
        st <- get
        case st.authState of
            AuthAuthenticated{}
                | seqNo == st.authSessionSeq -> case eRes of
                    Left err | isUnauthorizedError err -> do
                        modify (applySessionExpired env.messages)
                        io_ (env.sendEditorCmd CmdResetEditor)
                    _ -> onAuthorized
            _ -> pure ()

    updateAuthFormField :: (AuthFormState -> AuthFormState) -> Effect () () ClientState ClientAction
    updateAuthFormField fn =
        modify $ \ st -> case st.authState of
            AuthUnauthenticated form -> st { authState = AuthUnauthenticated (fn form) }
            _                        -> st

    executeLogout :: Effect () () ClientState ClientAction
    executeLogout = do
        st <- get
        case currentAuthSession st of
            Just session
                | not st.isLoggingOut -> do
                    modify $ \ s -> s
                        { isLoggingOut = True
                        , lastError    = Nothing
                        , statusNotice = Nothing
                        }
                    io (LoggedOut st.authSessionSeq <$> logoutUser env.apiClient session.csrfToken)
            _ -> pure ()

    executeNavigation :: PendingNavigation -> Effect () () ClientState ClientAction
    executeNavigation nav = case nav of
        NavOpenDocument docId -> do
            st <- get
            when (isJust (currentAuthSession st)) $
                case (st.networkStatus, Map.lookup docId st.cachedDocuments) of
                    (NetworkOffline, Just cached) -> do
                        modify $ \ s ->
                            let opened :: ClientState
                                opened = applyDocumentLoaded
                                    env.messages
                                    docId
                                    (Right cached.document)
                                    (s { openingDocumentId = Just docId, autoOpeningDraftId = Nothing })
                            in  opened { statusNotice = Just env.messages.noticeOpenedFromCache }
                        syncEditorFromCurrentState False
                    _ -> do
                        modify $ \ s -> s
                            { openingDocumentId  = Just docId
                            , autoOpeningDraftId = Nothing
                            , mobilePane         = PaneEditor
                            , lastError          = Nothing
                            }
                        io (DocumentLoaded st.authSessionSeq docId <$> getDocument env.apiClient docId)
        NavCreateDocument -> do
            st <- get
            case currentAuthSession st of
                Nothing -> pure ()
                Just session -> do
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
                    io (DocumentCreated st.authSessionSeq <$> createDocument env.apiClient session.csrfToken initialReq)
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
                , canUndo             = False
                , canRedo             = False
                , revisionHistory     = resetRevisionHistory s.revisionHistory
                , extensionsPanelOpen = False
                , mobilePane          = PaneList
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
            case (currentAuthSession st, st.orphanedDraft) of
                (Just session, Just sd)
                    | not st.isCreatingDocument && not st.isImportingDocument ->
                        case buildRecoveryRequestFromStoredDraft sd of
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
                                io (OrphanDraftRecovered st.authSessionSeq sd.docId <$> createDocument env.apiClient session.csrfToken createReq)
                _ -> pure ()
        NavApplyConflictLocal docId -> do
            st <- get
            case (currentAuthSession st, Map.lookup docId st.syncConflicts) of
                (Just _, Just conflict) ->
                    if st.networkStatus == NetworkOnline
                        then io (ConflictLocalApplyLoaded st.authSessionSeq docId <$> getDocument env.apiClient docId)
                        else case fmap (.document) (Map.lookup docId st.cachedDocuments) of
                            Just cachedDoc -> applyConflictToEditor cachedDoc conflict
                            Nothing ->
                                case st.selectedDocument of
                                    Just selDoc | selDoc.id == docId -> applyConflictToEditor selDoc conflict
                                    _                                -> pure ()
                _ -> pure ()
        NavRecoverConflictAsNew docId -> do
            st <- get
            case (currentAuthSession st, Map.lookup docId st.syncConflicts) of
                (Just session, Just conflict)
                    | not st.isCreatingDocument && not st.isImportingDocument ->
                        case buildConflictRecoveryRequest env.messages.conflictTitleSuffix conflict of
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
                                io (ConflictRecoveredAsNew st.authSessionSeq docId <$> createDocument env.apiClient session.csrfToken createReq)
                _ -> pure ()
        NavLogout ->
            executeLogout

    syncLocalDraftForCurrentState :: Effect () () ClientState ClientAction
    syncLocalDraftForCurrentState = do
        st <- get
        case (currentUserId st, st.selectedDocument) of
            (Just uid, Just doc) | not (isTrashed doc) ->
                if isDraftDirty doc st.draft
                    then do
                        let provisional :: StoredDraft
                            provisional = toStoredDraft uid doc st.draft doc.updatedAt
                        modify $ \ s -> s
                            { storedDrafts     = Map.insert doc.id provisional s.storedDrafts
                            , draftSyncState   = DraftSyncPending
                            , staleDraftPrompt = Nothing
                            }
                        io_ $ do
                            now <- env.getCurrentUtcTime
                            env.draftStorage.scheduleSaveDraft (toStoredDraft uid doc st.draft now)
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
        case (currentAuthSession st, st.selectedDocument) of
            (Just session, Just doc)
                | not (isTrashed doc)
                    && isNothing st.savingDocument
                    && isNothing st.restoringRevisionDoc
                    && isNothing st.conflictMerge
                    && st.syncingDocId /= Just doc.id ->
                    case validateEditorDraft st.draft of
                        Left vErr ->
                            modify (\ s -> s { lastError = Just (formatValidationError env.messages vErr) })
                        Right updateReq ->
                            if st.networkStatus == NetworkOffline
                                then queueOfflineDraftSave doc.id st.draft
                                else do
                                    let expectedRev :: RevisionNumber
                                        expectedRev = case Map.lookup doc.id st.pendingSaves of
                                            Just prev -> prev.baseRevision
                                            Nothing   -> doc.revision

                                        reqWithRev :: UpdateDocumentRequest
                                        reqWithRev = UpdateDocumentRequest
                                            { title        = updateReq.title
                                            , body         = updateReq.body
                                            , format       = updateReq.format
                                            , folder       = updateReq.folder
                                            , tags         = updateReq.tags
                                            , baseRevision = Just expectedRev
                                            }
                                    modify $ \ s -> s
                                        { savingDocument = Just (doc.id, st.draft)
                                        , lastError      = Nothing
                                        , statusNotice   = Nothing
                                        }
                                    io (DocumentSaved st.authSessionSeq doc.id <$> updateDocument env.apiClient session.csrfToken doc.id reqWithRev)
            _ -> pure ()

    cacheDocumentLocally :: Document -> Effect () () ClientState ClientAction
    cacheDocumentLocally doc = do
        st <- get
        for_ (currentUserId st) $ \ uid -> do
            let provisional :: CachedDocument
                provisional = CachedDocument
                    { ownerId  = uid
                    , document = doc
                    , cachedAt = doc.updatedAt
                    }
            modify (\ s -> s { cachedDocuments = Map.insert doc.id provisional s.cachedDocuments })
            io_ $ do
                now <- env.getCurrentUtcTime
                env.offlineStore.putCachedDocument provisional { cachedAt = now }

    applyConflictToEditor :: Document -> SyncConflict -> Effect () () ClientState ClientAction
    applyConflictToEditor baseDoc conflict = do
        let localDraft :: EditorDraft
            localDraft = draftFromPendingSave conflict.localSave
        modify $ \ s -> s
            { openingDocumentId   = Nothing
            , autoOpeningDraftId  = Nothing
            , selectedDocument    = Just baseDoc
            , draft               = localDraft
            , restoredDraftDocId  = Nothing
            , staleDraftPrompt    = Nothing
            , editorSyncVersion   = s.editorSyncVersion + 1
            , editorStats         = computeTextStats (toText localDraft.body)
            , editorViewMode      = normalizeViewMode localDraft.format s.editorViewMode
            , canUndo             = False
            , canRedo             = False
            , revisionHistory     = resetRevisionHistory s.revisionHistory
            , extensionsPanelOpen = False
            , mobilePane          = PaneEditor
            , lastError           = Nothing
            }
        syncEditorFromCurrentState True
        syncLocalDraftForCurrentState

    resolveDocumentForOfflineOp :: DocumentId -> ClientState -> Maybe Document
    resolveDocumentForOfflineOp docId st =
        case st.selectedDocument of
            Just selDoc | selDoc.id == docId -> Just selDoc
            _                                -> fmap (.document) (Map.lookup docId st.cachedDocuments)

    queueOfflineDraftSave :: DocumentId -> EditorDraft -> Effect () () ClientState ClientAction
    queueOfflineDraftSave docId targetDraft = do
        st <- get
        case (currentUserId st, resolveDocumentForOfflineOp docId st) of
            (Just uid, Just doc) ->
                case validateEditorDraft targetDraft of
                    Left vErr ->
                        modify (\ s -> s { lastError = Just (formatValidationError env.messages vErr) })
                    Right validReq -> do
                        let baseRev :: RevisionNumber
                            baseRev = case Map.lookup doc.id st.pendingSaves of
                                Just prev -> prev.baseRevision
                                Nothing   -> doc.revision

                            pendingItem :: PendingSyncSave
                            pendingItem = PendingSyncSave
                                { docId        = doc.id
                                , ownerId      = uid
                                , title        = validReq.title
                                , body         = validReq.body
                                , format       = validReq.format
                                , folder       = validReq.folder
                                , tags         = validReq.tags
                                , baseRevision = baseRev
                                , queuedAt     = doc.updatedAt
                                }

                            -- ローカル表示とオフラインキャッシュを更新しつつ、サーバ確定リビジョン番号は維持する
                            localDoc :: Document
                            localDoc = Document
                                { id        = doc.id
                                , title     = validReq.title
                                , body      = validReq.body
                                , format    = validReq.format
                                , folder    = validReq.folder
                                , tags      = validReq.tags
                                , revision  = doc.revision
                                , createdAt = doc.createdAt
                                , updatedAt = doc.updatedAt
                                , deletedAt = doc.deletedAt
                                }

                            isSameSelected :: Bool
                            isSameSelected = fmap (.id) st.selectedDocument == Just doc.id

                            mergedDraft :: EditorDraft
                            mergedDraft =
                                if isSameSelected
                                    then mergeDraftAfterSave targetDraft (draftFromDocument localDoc) st.draft
                                    else st.draft

                            stillDirty :: Bool
                            stillDirty = isSameSelected && isDraftDirty localDoc mergedDraft

                            summary :: DocumentSummary
                            summary = toDocumentSummary localDoc

                            nextSummaries :: [DocumentSummary]
                            nextSummaries = case st.filterState.status of
                                StatusTrashed -> st.documentSummaries
                                _             ->
                                    if any (\ item -> item.id == doc.id) st.documentSummaries
                                        then summary : filter (\ item -> item.id /= doc.id) st.documentSummaries
                                        else st.documentSummaries

                            nextCached :: CachedDocument
                            nextCached = CachedDocument
                                { ownerId  = uid
                                , document = localDoc
                                , cachedAt = doc.updatedAt
                                }

                            nextQueue :: Map DocumentId PendingSyncSave
                            nextQueue = enqueuePendingSave pendingItem st.pendingSaves
                        modify $ \ s -> s
                            { selectedDocument   = if isSameSelected then Just localDoc else s.selectedDocument
                            , draft              = mergedDraft
                            , documentSummaries  = nextSummaries
                            , cachedDocuments    = Map.insert doc.id nextCached s.cachedDocuments
                            , pendingSaves       = nextQueue
                            , storedDrafts       = if stillDirty then s.storedDrafts else Map.delete doc.id s.storedDrafts
                            , draftSyncState     = if not isSameSelected then s.draftSyncState else if stillDirty then DraftSyncPending else DraftSyncIdle
                            , restoredDraftDocId = if not isSameSelected || stillDirty then s.restoredDraftDocId else Nothing
                            , staleDraftPrompt   = if isSameSelected then Nothing else s.staleDraftPrompt
                            , lastError          = Nothing
                            , statusNotice       = Just env.messages.noticeSavedOffline
                            }
                        io_ $ do
                            now <- env.getCurrentUtcTime
                            let storedPending :: PendingSyncSave
                                storedPending = pendingItem { queuedAt = now }
                            env.offlineStore.putPendingSave storedPending
                            env.offlineStore.putCachedDocument nextCached { cachedAt = now }
                            unless stillDirty (env.draftStorage.deleteStoredDraft doc.id)
                        when stillDirty syncLocalDraftForCurrentState
            _ -> pure ()

    stashSaveConflictFromDraft :: DocumentId -> EditorDraft -> Text -> Effect () () ClientState ClientAction
    stashSaveConflictFromDraft docId targetDraft reasonText = do
        st <- get
        case (currentUserId st, resolveDocumentForOfflineOp docId st) of
            (Just uid, Just doc) ->
                case validateEditorDraft targetDraft of
                    Left _ -> pure ()
                    Right validReq -> do
                        let baseRev :: RevisionNumber
                            baseRev = case Map.lookup doc.id st.pendingSaves of
                                Just prev -> prev.baseRevision
                                Nothing   -> doc.revision

                            localItem :: PendingSyncSave
                            localItem = PendingSyncSave
                                { docId        = doc.id
                                , ownerId      = uid
                                , title        = validReq.title
                                , body         = validReq.body
                                , format       = validReq.format
                                , folder       = validReq.folder
                                , tags         = validReq.tags
                                , baseRevision = baseRev
                                , queuedAt     = doc.updatedAt
                                }

                            conflict :: SyncConflict
                            conflict = SyncConflict
                                { docId      = doc.id
                                , ownerId    = uid
                                , localSave  = localItem
                                , detectedAt = doc.updatedAt
                                , reason     = reasonText
                                }
                        modify $ \ s -> s
                            { pendingSaves  = Map.delete doc.id s.pendingSaves
                            , syncConflicts = Map.insert doc.id conflict s.syncConflicts
                            , lastError     = Nothing
                            }
                        io_ $ do
                            now <- env.getCurrentUtcTime
                            env.offlineStore.deletePendingSave doc.id
                            env.offlineStore.putSyncConflict conflict { detectedAt = now }
            _ -> pure ()

    draftFromPendingSave :: PendingSyncSave -> EditorDraft
    draftFromPendingSave ps = EditorDraft
        { titleInput  = toText ps.title
        , body        = ps.body
        , format      = ps.format
        , folderInput = maybe "" toText ps.folder
        , tagsInput   = formatTagsInput ps.tags
        }

    triggerSyncNext :: Effect () () ClientState ClientAction
    triggerSyncNext = do
        st <- get
        case currentAuthSession st of
            Just session
                | st.networkStatus == NetworkOnline
                    && isNothing st.syncingDocId
                    && isNothing st.savingDocument ->
                    case nextPendingSyncSave st.pendingSaves of
                        Nothing -> pure ()
                        Just item -> do
                            modify (\ s -> s { syncingDocId = Just item.docId })
                            io (SyncItemCompleted st.authSessionSeq item.docId <$> updateDocument env.apiClient session.csrfToken item.docId (buildSyncUpdateRequest item))
            _ -> pure ()

    syncEditorFromCurrentState :: Bool -> Effect () () ClientState ClientAction
    syncEditorFromCurrentState shouldResetHistory = do
        st <- get
        case buildEditorSyncPayload env.messages shouldResetHistory st of
            Nothing      -> pure ()
            Just payload -> io_ (env.sendEditorCmd (CmdSyncDocument payload))

    loadRevisionListForDocument :: DocumentId -> Bool -> Effect () () ClientState ClientAction
    loadRevisionListForDocument docId isAppend = do
        st <- get
        case currentAuthSession st of
            Nothing -> pure ()
            Just _ -> do
                let authSeq :: Int
                    authSeq = st.authSessionSeq

                    nextListSeq :: Int
                    nextListSeq = st.revisionHistory.listRequestSeq + 1

                    mOffset :: Maybe ListOffset
                    mOffset =
                        if isAppend
                            then either (const Nothing) Just (mkListOffset (length st.revisionHistory.revisions))
                            else Nothing
                modify $ \ s -> s
                    { revisionHistory = s.revisionHistory
                        { documentId     = Just docId
                        , listRequestSeq = nextListSeq
                        , isLoadingList  = not isAppend
                        , isLoadingMore  = isAppend
                        }
                    }
                io (RevisionListLoaded authSeq nextListSeq docId isAppend <$> listDocumentRevisions env.apiClient docId Nothing mOffset)

    loadRevisionDetailForDocument :: DocumentId -> RevisionNumber -> Effect () () ClientState ClientAction
    loadRevisionDetailForDocument docId revNo = do
        st <- get
        case currentAuthSession st of
            Nothing -> pure ()
            Just _ -> do
                let authSeq :: Int
                    authSeq = st.authSessionSeq

                    nextDetailSeq :: Int
                    nextDetailSeq = st.revisionHistory.detailRequestSeq + 1
                modify $ \ s -> s
                    { revisionHistory = s.revisionHistory
                        { documentId       = Just docId
                        , selectedRevision = Just revNo
                        , detailRequestSeq = nextDetailSeq
                        , isLoadingDetail  = True
                        }
                    }
                io (RevisionDetailLoaded authSeq nextDetailSeq docId revNo <$> getDocumentRevision env.apiClient docId revNo)

    loadRevisionCompare :: DocumentId -> RevisionNumber -> RevisionNumber -> Effect () () ClientState ClientAction
    loadRevisionCompare docId fromRev toRev = do
        st <- get
        case currentAuthSession st of
            Nothing -> pure ()
            Just _ -> do
                let authSeq :: Int
                    authSeq = st.authSessionSeq

                    nextCmpSeq :: Int
                    nextCmpSeq = st.revisionHistory.compareRequestSeq + 1
                modify $ \ s -> s
                    { revisionHistory = s.revisionHistory
                        { documentId        = Just docId
                        , compareFromRev    = Just fromRev
                        , compareToRev      = Just toRev
                        , compareRequestSeq = nextCmpSeq
                        , isLoadingCompare  = True
                        }
                    , lastError = Nothing
                    }
                io (RevisionsCompared authSeq nextCmpSeq docId <$> compareDocumentRevisions env.apiClient docId fromRev toRev)

    defaultComparePair ::
           Maybe RevisionNumber
        -> [RevisionSummary]
        -> (Maybe RevisionNumber, Maybe RevisionNumber)
    defaultComparePair mSelected revs = case revs of
        []      -> (Nothing, Nothing)
        [only]  -> (Just only.revision, Just only.revision)
        newest : second : _ ->
            let fromCandidate :: RevisionNumber
                fromCandidate = case mSelected of
                    Just sel | sel /= newest.revision -> sel
                    _                                 -> second.revision
            in  (Just fromCandidate, Just newest.revision)

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

resetWorkspaceForAuthenticated ::
       AuthSessionResponse
    -> Maybe Text
    -> ClientState
    -> ClientState
resetWorkspaceForAuthenticated session mNotice state = state
    { authState                    = AuthAuthenticated session
    , authSessionSeq               = state.authSessionSeq + 1
    , isLoggingOut                 = False
    , mobilePane                   = PaneList
    , editorViewMode               = ModeEdit
    , filterState                  = defaultFilterState
    , availableTags                = []
    , availableFolders             = []
    , documentSummaries            = []
    , totalDocuments               = 0
    , listRequestSeq               = state.listRequestSeq + 1
    , isLoadingDocuments           = False
    , isCreatingDocument           = False
    , isImportingDocument          = False
    , pendingListReloadAfterCreate = False
    , pendingListReloadAfterImport = False
    , openingDocumentId            = Nothing
    , autoOpeningDraftId           = Nothing
    , savingDocument               = Nothing
    , mutatingDocumentId           = Nothing
    , restoringRevisionDoc         = Nothing
    , selectedDocument             = Nothing
    , draft                        = emptyEditorDraft
    , storedDrafts                 = Map.empty
    , draftSyncState               = DraftSyncIdle
    , restoredDraftDocId           = Nothing
    , staleDraftPrompt             = Nothing
    , orphanedDraft                = Nothing
    , cachedDocuments              = Map.empty
    , pendingSaves                 = Map.empty
    , syncConflicts                = Map.empty
    , syncingDocId                 = Nothing
    , isServingOfflineCache        = False
    , conflictMerge                = Nothing
    , mergeRequestSeq              = state.mergeRequestSeq + 1
    , editorSyncVersion            = state.editorSyncVersion + 1
    , editorStats                  = emptyEditorStats
    , canUndo                      = False
    , canRedo                      = False
    , metadataPanelOpen            = False
    , searchPanel                  = emptySearchPanelState
    , revisionHistory              = resetRevisionHistory state.revisionHistory
    , extensionsPanelOpen          = False
    , enabledExtensions            = defaultEnabledExtensions state.extensionCatalog
    , pendingNavigation            = Nothing
    , confirmPermanentDelete       = Nothing
    , lastError                    = Nothing
    , statusNotice                 = mNotice
    }

filterDraftsForOwner :: UserId -> [StoredDraft] -> [StoredDraft]
filterDraftsForOwner uid = filter (\ sd -> sd.ownerId == Just uid)

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
            , networkStatusSub NetworkStatusChanged
            ]
        }
