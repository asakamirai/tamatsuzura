{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.I18n
    ( UiMessages(..)
    , jaMessages
    , formatDocumentFormat
    , formatApiClientError
    , formatValidationError
    , formatFileImportError
    ) where

import           Data.Text                       (Text)
import qualified Data.Text                       as Txt
import           TamaTsuzura.Client.Api          (ApiClientError(..))
import           TamaTsuzura.Client.FileTransfer (FileImportError(..))
import           TamaTsuzura.Common.Api          (ApiError(..), ApiErrorCode(..))
import           TamaTsuzura.Common.Document     (DocumentFormat(..))
import           TamaTsuzura.Common.Validation   (ValidationError(..))

-- | クライアント UI の全表示文言を集約するメッセージカタログ。
-- 将来の多言語化 (英語リソース追加や UI 言語切替) に備え、
-- ビューおよび状態管理ロジックへ日本語文字列を直書きせず本型経由で参照する。
data UiMessages = UiMessages
    { appTitle                   :: !Text
      -- ^ アプリケーション名
    , appSubtitle                :: !Text
      -- ^ ヘッダ副題
    , statusChecking             :: !Text
      -- ^ サーバ接続確認中のバッジ文言
    , statusOnline               :: !Text
      -- ^ サーバ接続正常時のバッジ文言
    , statusError                :: !Text
      -- ^ サーバ接続失敗時のバッジ文言
    , actionReload               :: !Text
      -- ^ 再読込ボタンラベル
    , actionNewDocument          :: !Text
      -- ^ 新規作成ボタンラベル
    , actionCreating             :: !Text
      -- ^ 新規作成実行中のボタンラベル
    , actionDismissError         :: !Text
      -- ^ エラー・通知バナーを閉じるボタンラベル
    , actionBackToList           :: !Text
      -- ^ モバイルで一覧画面へ戻るボタンラベル
    , actionSave                 :: !Text
      -- ^ 明示的保存ボタンラベル
    , actionSaving               :: !Text
      -- ^ 保存中ボタンラベル
    , actionUndo                 :: !Text
      -- ^ 元に戻すボタンラベル
    , actionRedo                 :: !Text
      -- ^ やり直すボタンラベル
    , actionSearchToggle         :: !Text
      -- ^ エディタ内検索/置換パネル切替ボタンラベル
    , actionMetadataToggle       :: !Text
      -- ^ 整理設定 (フォルダ・タグ・形式) 折りたたみ切替ラベル
    , actionMoveToTrash          :: !Text
      -- ^ ゴミ箱へ移動ボタンラベル
    , actionRestore              :: !Text
      -- ^ ゴミ箱から復元ボタンラベル
    , actionPermanentDelete      :: !Text
      -- ^ 完全削除ボタンラベル
    , actionCancel               :: !Text
      -- ^ キャンセルボタンラベル
    , actionClearFilters         :: !Text
      -- ^ フィルタ解除ボタンラベル
    , actionSearchRun            :: !Text
      -- ^ 文書一覧検索実行ボタンラベル
    , actionDiscardAndContinue   :: !Text
      -- ^ 未保存変更を破棄して続行するボタンラベル
    , actionConfirmPermanentDel  :: !Text
      -- ^ 完全削除を確定するボタンラベル
    , actionImport               :: !Text
      -- ^ ファイルインポートボタンラベル
    , actionImporting            :: !Text
      -- ^ ファイルインポート中ボタンラベル
    , actionExport               :: !Text
      -- ^ ファイルエクスポートボタンラベル
    , actionDiscardDraft         :: !Text
      -- ^ ローカル未保存ドラフト破棄ボタンラベル
    , actionApplyDraft           :: !Text
      -- ^ 更新前ローカルドラフト適用ボタンラベル
    , actionRecoverOrphanDraft   :: !Text
      -- ^ 孤立ドラフトを新規文書として復元するボタンラベル
    , badgeHasDraft              :: !Text
      -- ^ 文書一覧に表示する未保存ドラフト保有バッジ文言
    , draftStatePending          :: !Text
      -- ^ ローカルドラフト退避中インジケータ
    , draftStateSaved            :: !Text
      -- ^ ローカルドラフト退避済みインジケータ
    , draftStateFailed           :: !Text
      -- ^ ローカルドラフト退避失敗インジケータ
    , draftRestoredBanner        :: !Text
      -- ^ ローカル未保存ドラフト復元完了バナー文言
    , draftStaleBanner           :: !Text
      -- ^ サーバ側更新済み文書に対する未保存ドラフト存在警告バナー文言
    , draftTrashedBanner         :: !Text
      -- ^ ゴミ箱内文書に対する残存ドラフト案内バナー文言
    , draftOrphanedBanner        :: !Text
      -- ^ サーバ削除済み文書に対する孤立ドラフト救出バナー文言
    , tabActiveDocuments         :: !Text
      -- ^ 稼働中文書タブ文言
    , tabTrashDocuments          :: !Text
      -- ^ ゴミ箱タブ文言
    , modeEditLabel              :: !Text
      -- ^ 編集モードタブ文言
    , modeSplitLabel             :: !Text
      -- ^ 分割モードタブ文言
    , modePreviewLabel           :: !Text
      -- ^ プレビューモードタブ文言
    , documentsHeading           :: !Text
      -- ^ 文書一覧セクションの見出し
    , documentsCountUnit         :: !Text
      -- ^ 文書件数の単位 (例: @"件"@)
    , loadingDocuments           :: !Text
      -- ^ 文書一覧読み込み中メッセージ
    , loadingDocumentBody        :: !Text
      -- ^ 単一文書読み込み中メッセージ
    , emptyDocuments             :: !Text
      -- ^ 稼働中文書が 0 件のときの案内文言
    , emptyTrashDocuments        :: !Text
      -- ^ ゴミ箱が空のときの案内文言
    , emptyFilteredDocuments     :: !Text
      -- ^ フィルタ・検索条件に一致する文書がないときの案内文言
    , emptyEditorPrompt          :: !Text
      -- ^ 文書未選択時のエディタペイン案内文言
    , untitledDocument           :: !Text
      -- ^ タイトル未入力文書の代替表示名
    , formatMarkdownLabel        :: !Text
      -- ^ Markdown フォーマット表示名
    , formatPlainTextLabel       :: !Text
      -- ^ プレーンテキストフォーマット表示名
    , listSearchPlaceholder      :: !Text
      -- ^ 文書一覧検索欄のプレースホルダー
    , filterAllFolders           :: !Text
      -- ^ フォルダフィルタ「すべて」選択肢
    , filterAllTags              :: !Text
      -- ^ タグフィルタ「すべて」選択肢
    , filterIncludeSubfolders    :: !Text
      -- ^ サブフォルダを含めるチェックボックスラベル
    , filterFolderLabel          :: !Text
      -- ^ フォルダフィルタのラベル
    , filterTagLabel             :: !Text
      -- ^ タグフィルタのラベル
    , fieldTitleLabel            :: !Text
      -- ^ 文書タイトル入力ラベル
    , fieldTitlePlaceholder      :: !Text
      -- ^ 文書タイトル入力プレースホルダー
    , fieldFormatLabel           :: !Text
      -- ^ 形式選択ラベル
    , fieldFolderLabel           :: !Text
      -- ^ フォルダ入力ラベル
    , fieldFolderPlaceholder     :: !Text
      -- ^ フォルダ入力プレースホルダー
    , fieldTagsLabel             :: !Text
      -- ^ タグ入力ラベル
    , fieldTagsPlaceholder       :: !Text
      -- ^ タグ入力プレースホルダー
    , editorBodyPlaceholder      :: !Text
      -- ^ CodeMirror 6 本文プレースホルダー
    , stateSavedLabel            :: !Text
      -- ^ 保存済みインジケータ文言
    , stateUnsavedLabel          :: !Text
      -- ^ 未保存変更インジケータ文言
    , stateReadOnlyLabel         :: !Text
      -- ^ 読み取り専用インジケータ文言
    , trashReadOnlyBanner        :: !Text
      -- ^ ゴミ箱内文書閲覧時の案内バナー
    , previewEmptyHint           :: !Text
      -- ^ 本文が空のときのプレビュー表示案内
    , searchInputPlaceholder     :: !Text
      -- ^ エディタ内検索語プレースホルダー
    , replaceInputPlaceholder    :: !Text
      -- ^ エディタ内置換語プレースホルダー
    , searchCaseSensitiveLabel   :: !Text
      -- ^ 大文字・小文字を区別するボタンラベル
    , searchRegexLabel           :: !Text
      -- ^ 正規表現ボタンラベル
    , searchPrevLabel            :: !Text
      -- ^ 前の一致へ移動ラベル
    , searchNextLabel            :: !Text
      -- ^ 次の一致へ移動ラベル
    , searchReplaceLabel         :: !Text
      -- ^ 置換実行ボタンラベル
    , searchReplaceAllLabel      :: !Text
      -- ^ すべて置換ボタンラベル
    , searchToggleReplaceLabel   :: !Text
      -- ^ 置換入力欄の開閉ボタンラベル
    , searchNoMatches            :: !Text
      -- ^ 一致なしのときの表示文言
    , searchInvalidRegex         :: !Text
      -- ^ 正規表現エラー時の表示文言
    , statsLinesUnit             :: !Text
      -- ^ ステータスバー行数単位
    , statsCharsUnit             :: !Text
      -- ^ ステータスバー文字数単位
    , statsCursorPrefix          :: !Text
      -- ^ ステータスバーカーソル位置接頭辞
    , extensionsHeading          :: !Text
      -- ^ 登録済み拡張機能ラベル
    , noticeSaved                :: !Text
      -- ^ 保存完了通知
    , noticeMovedToTrash         :: !Text
      -- ^ ゴミ箱移動完了通知
    , noticeRestored             :: !Text
      -- ^ 復元完了通知
    , noticePermanentlyDeleted   :: !Text
      -- ^ 完全削除完了通知
    , noticeExtensionApplied     :: !Text
      -- ^ 拡張適用完了通知
    , noticeDraftRestored        :: !Text
      -- ^ 未保存ドラフト復元完了通知
    , noticeDraftDiscarded       :: !Text
      -- ^ 未保存ドラフト破棄完了通知
    , noticeExportedPrefix       :: !Text
      -- ^ エクスポート完了通知の接頭辞
    , noticeImportedPrefix       :: !Text
      -- ^ インポート完了通知の接頭辞
    , confirmUnsavedTitle        :: !Text
      -- ^ 未保存変更破棄確認モーダル見出し
    , confirmUnsavedBody         :: !Text
      -- ^ 未保存変更破棄確認モーダル本文
    , confirmPermanentDelTitle   :: !Text
      -- ^ 完全削除確認モーダル見出し
    , confirmPermanentDelBody    :: !Text
      -- ^ 完全削除確認モーダル本文
    , errNetworkPrefix           :: !Text
      -- ^ ネットワークエラーの日本語説明接頭辞
    , errValidationPrefix        :: !Text
      -- ^ 入力検証エラーの日本語説明接頭辞
    , errNotFoundMessage         :: !Text
      -- ^ 404 エラーの日本語説明
    , errConflictMessage         :: !Text
      -- ^ 409 エラーの日本語説明
    , errPayloadTooLarge         :: !Text
      -- ^ 413 エラーの日本語説明
    , errServerPrefix            :: !Text
      -- ^ サーバ・HTTP エラーの日本語説明接頭辞
    , errDecodeMessage           :: !Text
      -- ^ レスポンス解析失敗時の日本語説明
    , errDraftStoragePrefix      :: !Text
      -- ^ IndexedDB ドラフト操作失敗時の接頭辞
    , errExportFailedPrefix      :: !Text
      -- ^ ファイルエクスポート失敗時の接頭辞
    , errImportInvalidUtf8       :: !Text
      -- ^ UTF-8 以外のファイルインポート拒否時の説明
    , errImportFailedPrefix      :: !Text
      -- ^ ファイルインポート失敗時の接頭辞
    , valTitleTooLong            :: !Text
      -- ^ タイトル文字数上限超過の説明
    , valTitleControlChar        :: !Text
      -- ^ タイトル制御文字エラーの説明
    , valEmptyTag                :: !Text
      -- ^ 空タグエラーの説明
    , valTagTooLong              :: !Text
      -- ^ タグ文字数上限超過の説明
    , valTagControlChar          :: !Text
      -- ^ タグ制御文字エラーの説明
    , valTooManyTags             :: !Text
      -- ^ タグ個数上限超過の説明
    , valEmptyFolder             :: !Text
      -- ^ 空フォルダパスエラーの説明
    , valFolderTooLong           :: !Text
      -- ^ フォルダパス文字数上限超過の説明
    , valFolderTooDeep           :: !Text
      -- ^ フォルダ階層深さ上限超過の説明
    , valFolderInvalidSegment    :: !Text
      -- ^ フォルダパス不正セグメントエラーの説明
    , valSearchTooLong           :: !Text
      -- ^ 検索語文字数上限超過の説明
    , valSearchControlChar       :: !Text
      -- ^ 検索語制御文字エラーの説明
    } deriving (Eq, Show)

-- | 既定の日本語 UI メッセージカタログ。
jaMessages :: UiMessages
jaMessages = UiMessages
    { appTitle                  = "TamaTsuzura"
    , appSubtitle               = "モバイルファースト・テキストエディタ"
    , statusChecking            = "接続確認中..."
    , statusOnline              = "サーバ接続済み"
    , statusError               = "接続エラー"
    , actionReload              = "再読込"
    , actionNewDocument         = "新規作成"
    , actionCreating            = "作成中..."
    , actionDismissError        = "閉じる"
    , actionBackToList          = "一覧へ"
    , actionSave                = "保存"
    , actionSaving              = "保存中..."
    , actionUndo                = "戻す"
    , actionRedo                = "やり直す"
    , actionSearchToggle        = "検索・置換"
    , actionMetadataToggle      = "整理"
    , actionMoveToTrash         = "ゴミ箱へ"
    , actionRestore             = "復元"
    , actionPermanentDelete     = "完全削除"
    , actionCancel              = "キャンセル"
    , actionClearFilters        = "解除"
    , actionSearchRun           = "検索"
    , actionDiscardAndContinue  = "変更を破棄して移動"
    , actionConfirmPermanentDel = "完全に削除する"
    , actionImport              = "インポート"
    , actionImporting           = "インポート中..."
    , actionExport              = "エクスポート"
    , actionDiscardDraft        = "ドラフトを破棄"
    , actionApplyDraft          = "ドラフトを適用"
    , actionRecoverOrphanDraft  = "新規文書として復元"
    , badgeHasDraft             = "ドラフトあり"
    , draftStatePending         = "ドラフト退避中..."
    , draftStateSaved           = "ドラフト退避済み"
    , draftStateFailed          = "ドラフト退避失敗"
    , draftRestoredBanner       = "ローカルに退避されていた未保存ドラフトを復元しました。"
    , draftStaleBanner          = "この文書はサーバ側で更新されていますが、更新前のローカル未保存ドラフトが残っています。"
    , draftTrashedBanner        = "この文書にはゴミ箱移動前のローカル未保存ドラフトが残っています。"
    , draftOrphanedBanner       = "元の文書がサーバ上に見つかりませんでした。ローカルの未保存ドラフトを新規文書として復元するか、破棄できます。"
    , tabActiveDocuments        = "文書"
    , tabTrashDocuments         = "ゴミ箱"
    , modeEditLabel             = "編集"
    , modeSplitLabel            = "分割"
    , modePreviewLabel          = "プレビュー"
    , documentsHeading          = "文書一覧"
    , documentsCountUnit        = "件"
    , loadingDocuments          = "文書一覧を読み込んでいます..."
    , loadingDocumentBody       = "文書を読み込んでいます..."
    , emptyDocuments            = "文書はまだありません。「新規作成」から最初のメモを作成できます。"
    , emptyTrashDocuments       = "ゴミ箱は空です。"
    , emptyFilteredDocuments    = "条件に一致する文書が見つかりませんでした。"
    , emptyEditorPrompt         = "左の一覧から文書を選ぶか、「新規作成」から執筆を始めてください。"
    , untitledDocument          = "無題"
    , formatMarkdownLabel       = "Markdown"
    , formatPlainTextLabel      = "テキスト"
    , listSearchPlaceholder     = "タイトル・本文を検索"
    , filterAllFolders          = "すべてのフォルダ"
    , filterAllTags             = "すべてのタグ"
    , filterIncludeSubfolders   = "配下フォルダも含む"
    , filterFolderLabel         = "フォルダ"
    , filterTagLabel            = "タグ"
    , fieldTitleLabel           = "タイトル"
    , fieldTitlePlaceholder     = "タイトルを入力（省略可）"
    , fieldFormatLabel          = "形式"
    , fieldFolderLabel          = "フォルダ"
    , fieldFolderPlaceholder    = "例: work/notes（空欄でルート）"
    , fieldTagsLabel            = "タグ"
    , fieldTagsPlaceholder      = "例: メモ, アイデア（カンマ区切り）"
    , editorBodyPlaceholder     = "ここに本文を入力してください（Ctrl+S で保存 / Ctrl+F で検索）"
    , stateSavedLabel           = "保存済み"
    , stateUnsavedLabel         = "未保存"
    , stateReadOnlyLabel        = "閲覧専用"
    , trashReadOnlyBanner       = "ゴミ箱内の文書は閲覧専用です。編集するには「復元」を押してください。"
    , previewEmptyHint          = "本文を入力すると、ここに Markdown プレビューが表示されます。"
    , searchInputPlaceholder    = "検索語を入力"
    , replaceInputPlaceholder   = "置換後の文字列"
    , searchCaseSensitiveLabel  = "Aa"
    , searchRegexLabel          = ".*"
    , searchPrevLabel           = "前へ"
    , searchNextLabel           = "次へ"
    , searchReplaceLabel        = "置換"
    , searchReplaceAllLabel     = "すべて置換"
    , searchToggleReplaceLabel  = "置換欄"
    , searchNoMatches           = "一致なし"
    , searchInvalidRegex        = "正規表現の構文が正しくありません"
    , statsLinesUnit            = "行"
    , statsCharsUnit            = "文字"
    , statsCursorPrefix         = "カーソル"
    , extensionsHeading         = "拡張機能"
    , noticeSaved               = "文書を保存しました。"
    , noticeMovedToTrash        = "文書をゴミ箱へ移動しました。"
    , noticeRestored            = "文書をゴミ箱から復元しました。"
    , noticePermanentlyDeleted  = "文書を完全に削除しました。"
    , noticeExtensionApplied    = "拡張機能を適用しました。"
    , noticeDraftRestored       = "未保存ドラフトを復元しました。"
    , noticeDraftDiscarded      = "ローカルの未保存ドラフトを破棄しました。"
    , noticeExportedPrefix      = "ファイルをエクスポートしました: "
    , noticeImportedPrefix      = "ファイルをインポートしました: "
    , confirmUnsavedTitle       = "未保存の変更があります"
    , confirmUnsavedBody        = "現在の文書に未保存の変更があります。保存せずに移動すると変更は失われます。"
    , confirmPermanentDelTitle  = "文書を完全に削除しますか？"
    , confirmPermanentDelBody   = "この操作は取り消せません。対象文書はサーバから完全に削除されます。"
    , errNetworkPrefix          = "サーバへ接続できませんでした"
    , errValidationPrefix       = "入力内容に誤りがあります"
    , errNotFoundMessage        = "対象の文書または API が見つかりませんでした。"
    , errConflictMessage        = "文書の現在状態と競合しました。一覧を再読込してください。"
    , errPayloadTooLarge        = "文書サイズが上限 (5 MiB) を超えています。"
    , errServerPrefix           = "サーバエラーが発生しました"
    , errDecodeMessage          = "サーバ応答の読み取りに失敗しました。"
    , errDraftStoragePrefix     = "ローカルドラフトの保存に失敗しました"
    , errExportFailedPrefix     = "ファイルのエクスポートに失敗しました"
    , errImportInvalidUtf8      = "UTF-8 テキストとして読み込めないファイルのためインポートできません。"
    , errImportFailedPrefix     = "ファイルのインポートに失敗しました"
    , valTitleTooLong           = "タイトルが文字数上限を超えています"
    , valTitleControlChar       = "タイトルに改行や制御文字は使用できません"
    , valEmptyTag               = "空のタグは指定できません"
    , valTagTooLong             = "タグが文字数上限を超えています"
    , valTagControlChar         = "タグに制御文字は使用できません"
    , valTooManyTags            = "タグの最大個数を超えています"
    , valEmptyFolder            = "フォルダパスが不正です"
    , valFolderTooLong          = "フォルダパスが文字数上限を超えています"
    , valFolderTooDeep          = "フォルダ階層の最大深さを超えています"
    , valFolderInvalidSegment   = "フォルダパスに空セグメント・'.'・'..'・バックスラッシュは使用できません"
    , valSearchTooLong          = "検索語が文字数上限を超えています"
    , valSearchControlChar      = "検索語に制御文字は使用できません"
    }

-- | 文書フォーマットをユーザー向け表示文字列へ変換する。
formatDocumentFormat :: UiMessages -> DocumentFormat -> Text
formatDocumentFormat msgs FormatMarkdown  = msgs.formatMarkdownLabel
formatDocumentFormat msgs FormatPlainText = msgs.formatPlainTextLabel

-- | ドメイン検証エラーをユーザー向けの表示文言へ整形する。
formatValidationError :: UiMessages -> ValidationError -> Text
formatValidationError msgs err = case err of
    TitleTooLong actual limit ->
        withLimit msgs.valTitleTooLong actual limit
    TitleContainsControlChar ->
        withDetail msgs.valTitleControlChar
    BodyTooLarge{} ->
        msgs.errPayloadTooLarge
    EmptyTag ->
        withDetail msgs.valEmptyTag
    TagTooLong actual limit ->
        withLimit msgs.valTagTooLong actual limit
    TagContainsControlChar{} ->
        withDetail msgs.valTagControlChar
    TooManyTags actual limit ->
        withLimit msgs.valTooManyTags actual limit
    EmptyFolderPath ->
        withDetail msgs.valEmptyFolder
    FolderPathTooLong actual limit ->
        withLimit msgs.valFolderTooLong actual limit
    FolderPathTooDeep actual limit ->
        withLimit msgs.valFolderTooDeep actual limit
    InvalidFolderPathSegment{} ->
        withDetail msgs.valFolderInvalidSegment
    SearchQueryTooLong actual limit ->
        withLimit msgs.valSearchTooLong actual limit
    SearchQueryContainsControlChar ->
        withDetail msgs.valSearchControlChar
    _ ->
        withDetail (Txt.pack (show err))
  where
    withDetail :: Text -> Text
    withDetail detail = msgs.errValidationPrefix <> " (" <> detail <> ")"

    withLimit :: Text -> Int -> Int -> Text
    withLimit label actual limit =
        withDetail (label <> ": " <> Txt.pack (show actual) <> "/" <> Txt.pack (show limit))

-- | 'ApiClientError' をユーザー向けの分かりやすい表示文言へ整形する。
formatApiClientError :: UiMessages -> ApiClientError -> Text
formatApiClientError msgs (ClientValidationError vErr) =
    formatValidationError msgs vErr
formatApiClientError msgs (ClientNetworkError detail) =
    msgs.errNetworkPrefix <> " (" <> detail <> ")"
formatApiClientError msgs (ClientApiError _status apiErr) = case apiErr.code of
    ErrNotFound         -> msgs.errNotFoundMessage
    ErrConflict         -> msgs.errConflictMessage
    ErrPayloadTooLarge  -> msgs.errPayloadTooLarge
    ErrValidation       -> msgs.errValidationPrefix <> " (" <> apiErr.message <> ")"
    ErrMethodNotAllowed -> msgs.errServerPrefix <> " (" <> apiErr.message <> ")"
    ErrInternalServer   -> msgs.errServerPrefix <> " (" <> apiErr.message <> ")"
formatApiClientError msgs (ClientHttpError status detail)
    | Txt.null detail = msgs.errServerPrefix <> " (HTTP " <> Txt.pack (show status) <> ")"
    | otherwise       = msgs.errServerPrefix <> " (HTTP " <> Txt.pack (show status) <> ": " <> detail <> ")"
formatApiClientError msgs ClientDecodeError{} =
    msgs.errDecodeMessage

-- | 'FileImportError' をユーザー向けの表示文言へ整形する。
formatFileImportError :: UiMessages -> FileImportError -> Text
formatFileImportError msgs err = case err of
    ImportFileTooLarge{} ->
        msgs.errPayloadTooLarge
    ImportInvalidUtf8{} ->
        msgs.errImportInvalidUtf8
    ImportValidationFailed vErr ->
        formatValidationError msgs vErr
    ImportReadFailed detail ->
        msgs.errImportFailedPrefix <> " (" <> detail <> ")"
