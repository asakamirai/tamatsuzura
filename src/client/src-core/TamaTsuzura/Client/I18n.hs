{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Client.I18n
    ( UiMessages(..)
    , jaMessages
    , formatDocumentFormat
    , formatApiClientError
    ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as Txt
import           TamaTsuzura.Client.Api     (ApiClientError(..))
import           TamaTsuzura.Common.Api     (ApiError(..), ApiErrorCode(..))
import           TamaTsuzura.Common.Document (DocumentFormat(..))
import           TamaTsuzura.Common.Text    (ToText(..))

-- | クライアント UI の全表示文言を集約するメッセージカタログ。
-- 将来の多言語化 (英語リソース追加や UI 言語切替) に備え、
-- ビューおよび状態管理ロジックへ日本語文字列を直書きせず本型経由で参照する。
data UiMessages = UiMessages
    { appTitle                 :: !Text
      -- ^ アプリケーション名
    , appSubtitle              :: !Text
      -- ^ ヘッダ副題
    , statusChecking           :: !Text
      -- ^ サーバ接続確認中のバッジ文言
    , statusOnline             :: !Text
      -- ^ サーバ接続正常時のバッジ文言
    , statusError              :: !Text
      -- ^ サーバ接続失敗時のバッジ文言
    , actionReload             :: !Text
      -- ^ 再読込ボタンラベル
    , actionNewDocument        :: !Text
      -- ^ 新規メモ作成ボタンラベル
    , actionCreating           :: !Text
      -- ^ 新規作成実行中のボタンラベル
    , actionDismissError       :: !Text
      -- ^ エラーバナーを閉じるボタンラベル
    , documentsHeading         :: !Text
      -- ^ 文書一覧セクションの見出し
    , documentsCountUnit       :: !Text
      -- ^ 文書件数の単位 (例: @"件"@)
    , loadingDocuments         :: !Text
      -- ^ 文書一覧読み込み中メッセージ
    , emptyDocuments           :: !Text
      -- ^ 文書が 0 件のときの案内文言
    , untitledDocument         :: !Text
      -- ^ タイトル未入力文書の代替表示名
    , formatMarkdownLabel      :: !Text
      -- ^ Markdown フォーマット表示名
    , formatPlainTextLabel     :: !Text
      -- ^ プレーンテキストフォーマット表示名
    , lastCreatedHeading       :: !Text
      -- ^ 直近作成文書確認セクションの見出し
    , extensionsHeading        :: !Text
      -- ^ 登録済み拡張一覧セクションの見出し
    , errNetworkPrefix         :: !Text
      -- ^ ネットワークエラーの日本語説明接頭辞
    , errValidationPrefix      :: !Text
      -- ^ 入力検証エラーの日本語説明接頭辞
    , errNotFoundMessage       :: !Text
      -- ^ 404 エラーの日本語説明
    , errConflictMessage       :: !Text
      -- ^ 409 エラーの日本語説明
    , errPayloadTooLarge       :: !Text
      -- ^ 413 エラーの日本語説明
    , errServerPrefix          :: !Text
      -- ^ サーバ・HTTP エラーの日本語説明接頭辞
    , errDecodeMessage         :: !Text
      -- ^ レスポンス解析失敗時の日本語説明
    } deriving (Eq, Show)

-- | 既定の日本語 UI メッセージカタログ。
jaMessages :: UiMessages
jaMessages = UiMessages
    { appTitle             = "TamaTsuzura"
    , appSubtitle          = "モバイルファースト・テキストエディタ"
    , statusChecking       = "接続確認中..."
    , statusOnline         = "サーバ接続済み"
    , statusError          = "接続エラー"
    , actionReload         = "再読込"
    , actionNewDocument    = "新規メモ作成"
    , actionCreating       = "作成中..."
    , actionDismissError   = "閉じる"
    , documentsHeading     = "文書一覧"
    , documentsCountUnit   = "件"
    , loadingDocuments     = "文書一覧を読み込んでいます..."
    , emptyDocuments       = "文書はまだありません。「新規メモ作成」から最初のメモを作成できます。"
    , untitledDocument     = "無題"
    , formatMarkdownLabel  = "Markdown"
    , formatPlainTextLabel = "テキスト"
    , lastCreatedHeading   = "最後に作成した文書"
    , extensionsHeading    = "読み込み済み拡張機能"
    , errNetworkPrefix     = "サーバへ接続できませんでした"
    , errValidationPrefix  = "入力内容に誤りがあります"
    , errNotFoundMessage   = "対象の文書または API が見つかりませんでした。"
    , errConflictMessage   = "文書の現在状態と競合しました。一覧を再読込してください。"
    , errPayloadTooLarge   = "文書サイズが上限 (5 MiB) を超えています。"
    , errServerPrefix      = "サーバエラーが発生しました"
    , errDecodeMessage     = "サーバ応答の読み取りに失敗しました。"
    }

-- | 文書フォーマットをユーザー向け表示文字列へ変換する。
formatDocumentFormat :: UiMessages -> DocumentFormat -> Text
formatDocumentFormat msgs FormatMarkdown  = msgs.formatMarkdownLabel
formatDocumentFormat msgs FormatPlainText = msgs.formatPlainTextLabel

-- | 'ApiClientError' をユーザー向けの分かりやすい表示文言へ整形する。
formatApiClientError :: UiMessages -> ApiClientError -> Text
formatApiClientError msgs (ClientValidationError vErr) =
    msgs.errValidationPrefix <> " (" <> toText vErr <> ")"
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
formatApiClientError msgs (ClientDecodeError _detail) =
    msgs.errDecodeMessage
