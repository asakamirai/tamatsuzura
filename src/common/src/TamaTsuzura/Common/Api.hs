{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module TamaTsuzura.Common.Api
    ( -- * ヘルスチェック
      HealthStatus(..)
    , healthOk
      -- * 一覧クエリ・フィルタ型
    , DocumentStatusFilter(..)
    , defaultDocumentStatusFilter
    , parseDocumentStatusFilter
    , matchesDocumentStatus
    , SearchQuery
    , mkSearchQuery
    , ListLimit
    , mkListLimit
    , defaultListLimitValue
    , listLimitValue
    , ListOffset
    , mkListOffset
    , defaultListOffsetValue
    , listOffsetValue
    , DocumentListQuery(..)
    , defaultDocumentListQuery
      -- * 文書 CRUD リクエスト / レスポンス DTO
    , CreateDocumentRequest(..)
    , defaultCreateDocumentRequest
    , validateCreateDocumentRequest
    , UpdateDocumentRequest(..)
    , validateUpdateDocumentRequest
    , DocumentListResponse(..)
    , TagListResponse(..)
    , FolderListResponse(..)
      -- * 共通エラーレスポンス
    , ApiErrorCode(..)
    , parseApiErrorCode
    , ApiError(..)
    , validationErrorToApiError
    ) where

import           Data.Aeson                    (FromJSON(..), ToJSON(..))
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types              (Pair)
import qualified Data.Char                     as Char
import           Data.Maybe                    (isJust, isNothing)
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import           Data.Time.Clock               (UTCTime)
import           TamaTsuzura.Common.Document
    ( DocumentBody
    , DocumentFormat
    , DocumentSummary
    , DocumentTitle
    , FolderPath
    , Tag
    , TagSet
    , defaultDocumentFormat
    , emptyDocumentBody
    , emptyDocumentTitle
    , emptyTagSet
    , validateDocumentBody
    )
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation
    ( ValidationError(..)
    , defaultListLimit
    , maxListLimit
    , maxSearchQueryLength
    , minListLimit
    , validationErrorField
    )

-- | ヘルスチェック API (@GET /api/health@) のレスポンス表現。
-- OpenAPI 仕様 (@docs/api/openapi.yaml@) と一致する JSON 形式を保証する。
newtype HealthStatus = HealthStatus
    { status :: Text
      -- ^ サーバ稼働状態を示すステータス文字列 (正常時は @"ok"@)
    } deriving (Eq)

instance ToText HealthStatus where
    toText health = health.status

instance Show HealthStatus where
    show = Txt.unpack . toText

instance ToJSON HealthStatus where
    toJSON health = Aeson.object
        [ "status" Aeson..= health.status
        ]

instance FromJSON HealthStatus where
    parseJSON = Aeson.withObject "HealthStatus" $ \ obj ->
        HealthStatus <$> obj Aeson..: "status"

-- | 正常稼働時の標準ヘルスチェック値。
healthOk :: HealthStatus
healthOk = HealthStatus
    { status = "ok"
    }

-- | 文書一覧 API (@GET /api/documents@) の論理削除ステータスフィルタ。
data DocumentStatusFilter
    = StatusActive
      -- ^ 稼働中の文書のみ (@deleted_at IS NULL@、既定値)
    | StatusTrashed
      -- ^ ゴミ箱内の文書のみ (@deleted_at IS NOT NULL@)
    | StatusAll
      -- ^ 稼働中・ゴミ箱内を問わずすべての文書
    deriving (Eq, Ord, Enum, Bounded)

-- | 一覧取得時の既定ステータスフィルタ ('StatusActive')。
defaultDocumentStatusFilter :: DocumentStatusFilter
defaultDocumentStatusFilter = StatusActive

instance ToText DocumentStatusFilter where
    toText StatusActive  = "active"
    toText StatusTrashed = "trashed"
    toText StatusAll     = "all"

instance Show DocumentStatusFilter where
    show = Txt.unpack . toText

instance ToJSON DocumentStatusFilter where
    toJSON = toJSON . toText

instance FromJSON DocumentStatusFilter where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (parseDocumentStatusFilter raw)

-- | クエリ文字列から 'DocumentStatusFilter' を解釈する。
parseDocumentStatusFilter :: Text -> Either ValidationError DocumentStatusFilter
parseDocumentStatusFilter "active"  = Right StatusActive
parseDocumentStatusFilter "trashed" = Right StatusTrashed
parseDocumentStatusFilter "all"     = Right StatusAll
parseDocumentStatusFilter raw       = Left (InvalidDocumentStatusFilter raw)

-- | 文書の @deletedAt@ が指定されたステータスフィルタに一致するか判定する。
matchesDocumentStatus :: DocumentStatusFilter -> Maybe UTCTime -> Bool
matchesDocumentStatus StatusActive  mDeletedAt = isNothing mDeletedAt
matchesDocumentStatus StatusTrashed mDeletedAt = isJust mDeletedAt
matchesDocumentStatus StatusAll     _          = True

-- | 文書一覧検索クエリ (@q@ パラメータ)。
-- 空白除去後に空でなく、制御文字を含まない 1〜'maxSearchQueryLength' 文字であることを保証する。
newtype SearchQuery = SearchQuery Text
    deriving (Eq, Ord, ToText)

instance Show SearchQuery where
    show = Txt.unpack . toText

instance ToJSON SearchQuery where
    toJSON = toJSON . toText

-- | 検索クエリ文字列を検証・正規化する。
-- 空白のみの入力は「検索フィルタなし」として @Right Nothing@ を返す。
mkSearchQuery :: Text -> Either ValidationError (Maybe SearchQuery)
mkSearchQuery raw
    | Txt.any Char.isControl raw = Left SearchQueryContainsControlChar
    | Txt.null trimmed           = Right Nothing
    | len > maxSearchQueryLength = Left (SearchQueryTooLong len maxSearchQueryLength)
    | otherwise                  = Right (Just (SearchQuery trimmed))
  where
    trimmed :: Text
    trimmed = Txt.strip raw

    len :: Int
    len = Txt.length trimmed

-- | 一覧取得 API の件数上限 (@limit@: 'minListLimit' 〜 'maxListLimit')。
-- 許容範囲外の値を型レベルで排除するためコンストラクタを非公開とする。
newtype ListLimit = ListLimit
    { unWrap :: Int
    } deriving (Eq, Ord)

instance ToText ListLimit where
    toText limit = Txt.pack (show limit.unWrap)

instance Show ListLimit where
    show = Txt.unpack . toText

instance ToJSON ListLimit where
    toJSON limit = toJSON limit.unWrap

instance FromJSON ListLimit where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkListLimit raw)

-- | 許容範囲内であることを検証して 'ListLimit' を構築する。
mkListLimit :: Int -> Either ValidationError ListLimit
mkListLimit n
    | n < minListLimit || n > maxListLimit = Left (ListLimitOutOfRange n minListLimit maxListLimit)
    | otherwise                            = Right (ListLimit n)

-- | 一覧取得件数上限の既定値 ('defaultListLimit')。
defaultListLimitValue :: ListLimit
defaultListLimitValue = ListLimit defaultListLimit

-- | 検証済み 'ListLimit' から生の 'Int' 値を取り出す。
listLimitValue :: ListLimit -> Int
listLimitValue limit = limit.unWrap

-- | 一覧取得 API の開始位置オフセット (@offset@: 0 以上)。
-- 負数の混入を型レベルで排除するためコンストラクタを非公開とする。
newtype ListOffset = ListOffset
    { unWrap :: Int
    } deriving (Eq, Ord)

instance ToText ListOffset where
    toText offset = Txt.pack (show offset.unWrap)

instance Show ListOffset where
    show = Txt.unpack . toText

instance ToJSON ListOffset where
    toJSON offset = toJSON offset.unWrap

instance FromJSON ListOffset where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack . toText) pure (mkListOffset raw)

-- | 0 以上であることを検証して 'ListOffset' を構築する。
mkListOffset :: Int -> Either ValidationError ListOffset
mkListOffset n
    | n < 0     = Left (NegativeListOffset n)
    | otherwise = Right (ListOffset n)

-- | 一覧取得オフセットの既定値 (0)。
defaultListOffsetValue :: ListOffset
defaultListOffsetValue = ListOffset 0

-- | 検証済み 'ListOffset' から生の 'Int' 値を取り出す。
listOffsetValue :: ListOffset -> Int
listOffsetValue offset = offset.unWrap

-- | 文書一覧取得 (@GET /api/documents@) の検証済みクエリ条件。
data DocumentListQuery = DocumentListQuery
    { status            :: !DocumentStatusFilter
      -- ^ 論理削除ステータスフィルタ (既定: 'StatusActive')
    , tag               :: !(Maybe Tag)
      -- ^ 指定タグを含む文書のみへ絞り込むフィルタ
    , folder            :: !(Maybe FolderPath)
      -- ^ 指定フォルダに属する文書のみへ絞り込むフィルタ
    , includeSubfolders :: !Bool
      -- ^ @folder@ 指定時に配下サブフォルダの文書も含めるかどうか (既定: 'False')
    , search            :: !(Maybe SearchQuery)
      -- ^ タイトル・本文に対する部分一致検索クエリ
    , limit             :: !ListLimit
      -- ^ 1 回の取得件数上限
    , offset            :: !ListOffset
      -- ^ 取得開始位置のオフセット
    } deriving (Eq, Show)

instance ToText DocumentListQuery where
    toText query =
        "status=" <> toText query.status
            <> ", limit=" <> toText query.limit
            <> ", offset=" <> toText query.offset

-- | パラメータ未指定時の既定一覧クエリ条件。
defaultDocumentListQuery :: DocumentListQuery
defaultDocumentListQuery = DocumentListQuery
    { status            = defaultDocumentStatusFilter
    , tag               = Nothing
    , folder            = Nothing
    , includeSubfolders = False
    , search            = Nothing
    , limit             = defaultListLimitValue
    , offset            = defaultListOffsetValue
    }

-- | 文書新規作成 API (@POST /api/documents@) のリクエスト DTO。
-- モバイルからの即時メモ作成を容易にするため、JSON 受信時は全フィールド省略可能とし安全な既定値を補う。
data CreateDocumentRequest = CreateDocumentRequest
    { title  :: !DocumentTitle
      -- ^ 文書タイトル (省略時は空タイトル)
    , body   :: !DocumentBody
      -- ^ 文書本文 (省略時は空本文)
    , format :: !DocumentFormat
      -- ^ 文書フォーマット (省略時は 'FormatMarkdown')
    , folder :: !(Maybe FolderPath)
      -- ^ 所属フォルダパス (省略時または @null@ はルート)
    , tags   :: !TagSet
      -- ^ 付与タグ集合 (省略時は空集合)
    } deriving (Eq, Show)

instance ToText CreateDocumentRequest where
    toText req = "CreateDocument(" <> toText req.title <> ", " <> toText req.format <> ")"

-- | 空の Markdown 文書をルートに作成するための既定リクエスト。
defaultCreateDocumentRequest :: CreateDocumentRequest
defaultCreateDocumentRequest = CreateDocumentRequest
    { title  = emptyDocumentTitle
    , body   = emptyDocumentBody
    , format = defaultDocumentFormat
    , folder = Nothing
    , tags   = emptyTagSet
    }

instance ToJSON CreateDocumentRequest where
    toJSON req = Aeson.object
        [ "title"  Aeson..= req.title
        , "body"   Aeson..= req.body
        , "format" Aeson..= req.format
        , "folder" Aeson..= req.folder
        , "tags"   Aeson..= req.tags
        ]

instance FromJSON CreateDocumentRequest where
    parseJSON = Aeson.withObject "CreateDocumentRequest" $ \ obj ->
        CreateDocumentRequest
            <$> obj Aeson..:! "title"  Aeson..!= emptyDocumentTitle
            <*> obj Aeson..:! "body"   Aeson..!= emptyDocumentBody
            <*> obj Aeson..:! "format" Aeson..!= defaultDocumentFormat
            <*> obj Aeson..:? "folder"
            <*> obj Aeson..:! "tags"   Aeson..!= emptyTagSet

-- | 新規作成リクエストの追加ドメイン制約 (本文バイト長上限) を検証する。
validateCreateDocumentRequest :: CreateDocumentRequest -> Either ValidationError CreateDocumentRequest
validateCreateDocumentRequest req = do
    _ <- validateDocumentBody req.body
    Right req

-- | 文書更新 API (@PUT /api/documents/{id}@) のリクエスト DTO。
-- PUT による全体置換の意図せぬフィールド消失を防ぐため、@title@・@body@・@format@・@tags@ を必須とする。
data UpdateDocumentRequest = UpdateDocumentRequest
    { title  :: !DocumentTitle
      -- ^ 更新後タイトル (空文字可)
    , body   :: !DocumentBody
      -- ^ 更新後本文 (UTF-8)
    , format :: !DocumentFormat
      -- ^ 更新後フォーマット種別
    , folder :: !(Maybe FolderPath)
      -- ^ 更新後フォルダパス (@null@ または省略時はルートへ移動)
    , tags   :: !TagSet
      -- ^ 更新後タグ集合
    } deriving (Eq, Show)

instance ToText UpdateDocumentRequest where
    toText req = "UpdateDocument(" <> toText req.title <> ", " <> toText req.format <> ")"

instance ToJSON UpdateDocumentRequest where
    toJSON req = Aeson.object
        [ "title"  Aeson..= req.title
        , "body"   Aeson..= req.body
        , "format" Aeson..= req.format
        , "folder" Aeson..= req.folder
        , "tags"   Aeson..= req.tags
        ]

instance FromJSON UpdateDocumentRequest where
    parseJSON = Aeson.withObject "UpdateDocumentRequest" $ \ obj ->
        UpdateDocumentRequest
            <$> obj Aeson..:  "title"
            <*> obj Aeson..:  "body"
            <*> obj Aeson..:  "format"
            <*> obj Aeson..:? "folder"
            <*> obj Aeson..:  "tags"

-- | 更新リクエストの追加ドメイン制約 (本文バイト長上限) を検証する。
validateUpdateDocumentRequest :: UpdateDocumentRequest -> Either ValidationError UpdateDocumentRequest
validateUpdateDocumentRequest req = do
    _ <- validateDocumentBody req.body
    Right req

-- | 文書一覧 API (@GET /api/documents@) のレスポンス DTO。
-- 将来のページネーション拡張に備え、裸の配列ではなくメタデータを含むオブジェクト形式で返す。
data DocumentListResponse = DocumentListResponse
    { documents :: ![DocumentSummary]
      -- ^ 条件に一致した文書サマリ一覧 (更新日時降順)
    , total     :: !Int
      -- ^ @limit@ / @offset@ 適用前の総一致件数 (0 以上)
    } deriving (Eq, Show)

instance ToText DocumentListResponse where
    toText res =
        "DocumentListResponse(count="
            <> Txt.pack (show (length res.documents))
            <> ", total="
            <> Txt.pack (show res.total)
            <> ")"

instance ToJSON DocumentListResponse where
    toJSON res = Aeson.object
        [ "documents" Aeson..= res.documents
        , "total"     Aeson..= res.total
        ]

instance FromJSON DocumentListResponse where
    parseJSON = Aeson.withObject "DocumentListResponse" $ \ obj -> do
        docs <- obj Aeson..: "documents"
        tot  <- obj Aeson..: "total"
        if tot < 0
            then fail "total must be non-negative"
            else pure DocumentListResponse
                { documents = docs
                , total     = tot
                }

-- | タグ一覧 API (@GET /api/tags@) のレスポンス DTO。
newtype TagListResponse = TagListResponse
    { tags :: [Tag]
      -- ^ 稼働中文書で使用されている重複なし・昇順ソート済みタグ一覧
    } deriving (Eq)

instance ToText TagListResponse where
    toText res = Txt.intercalate ", " (map toText res.tags)

instance Show TagListResponse where
    show = Txt.unpack . toText

instance ToJSON TagListResponse where
    toJSON res = Aeson.object
        [ "tags" Aeson..= res.tags
        ]

instance FromJSON TagListResponse where
    parseJSON = Aeson.withObject "TagListResponse" $ \ obj ->
        TagListResponse <$> obj Aeson..: "tags"

-- | フォルダ一覧 API (@GET /api/folders@) のレスポンス DTO。
newtype FolderListResponse = FolderListResponse
    { folders :: [FolderPath]
      -- ^ 稼働中文書で使用されている重複なし・昇順ソート済みフォルダパス一覧
    } deriving (Eq)

instance ToText FolderListResponse where
    toText res = Txt.intercalate ", " (map toText res.folders)

instance Show FolderListResponse where
    show = Txt.unpack . toText

instance ToJSON FolderListResponse where
    toJSON res = Aeson.object
        [ "folders" Aeson..= res.folders
        ]

instance FromJSON FolderListResponse where
    parseJSON = Aeson.withObject "FolderListResponse" $ \ obj ->
        FolderListResponse <$> obj Aeson..: "folders"

-- | REST API 全体で統一使用する機械判読可能なエラーコード。
data ApiErrorCode
    = ErrValidation
      -- ^ リクエスト形式またはドメイン制約の検証エラー (HTTP 400)
    | ErrNotFound
      -- ^ 対象リソースまたはエンドポイントが存在しない (HTTP 404)
    | ErrConflict
      -- ^ リソース状態との競合 (ゴミ箱内文書の編集、稼働中文書の完全削除など、HTTP 409)
    | ErrPayloadTooLarge
      -- ^ 本文またはリクエストサイズが上限を超過 (HTTP 413)
    | ErrMethodNotAllowed
      -- ^ 未許可の HTTP メソッド (HTTP 405)
    | ErrInternalServer
      -- ^ 予期しないサーバ内部エラー (HTTP 500)
    deriving (Eq, Ord, Enum, Bounded)

instance ToText ApiErrorCode where
    toText ErrValidation       = "validation_error"
    toText ErrNotFound         = "not_found"
    toText ErrConflict         = "conflict"
    toText ErrPayloadTooLarge  = "payload_too_large"
    toText ErrMethodNotAllowed = "method_not_allowed"
    toText ErrInternalServer   = "internal_error"

instance Show ApiErrorCode where
    show = Txt.unpack . toText

instance ToJSON ApiErrorCode where
    toJSON = toJSON . toText

instance FromJSON ApiErrorCode where
    parseJSON v = do
        raw <- parseJSON v
        either (fail . Txt.unpack) pure (parseApiErrorCode raw)

-- | 文字列表現から 'ApiErrorCode' を復元する。
parseApiErrorCode :: Text -> Either Text ApiErrorCode
parseApiErrorCode "validation_error"   = Right ErrValidation
parseApiErrorCode "not_found"          = Right ErrNotFound
parseApiErrorCode "conflict"           = Right ErrConflict
parseApiErrorCode "payload_too_large"  = Right ErrPayloadTooLarge
parseApiErrorCode "method_not_allowed" = Right ErrMethodNotAllowed
parseApiErrorCode "internal_error"     = Right ErrInternalServer
parseApiErrorCode raw                  = Left ("Unknown API error code: " <> raw)

-- | 全エラーレスポンスで共通利用する構造化エラー DTO。
-- JSON 表現のキー名は OpenAPI 仕様に従い @"error"@ としつつ、
-- Haskell 側では 'Prelude.error' との衝突を避けるためフィールド名を @code@ とする。
data ApiError = ApiError
    { code    :: !ApiErrorCode
      -- ^ 機械判読可能なエラー種別コード (JSON キー: @"error"@)
    , message :: !Text
      -- ^ 人間・開発者向けの診断メッセージ
    , field   :: !(Maybe Text)
      -- ^ 検証エラー時に対象となったフィールド名・クエリパラメータ名
    } deriving (Eq)

instance ToText ApiError where
    toText err = case err.field of
        Nothing -> toText err.code <> ": " <> err.message
        Just f  -> toText err.code <> " (" <> f <> "): " <> err.message

instance Show ApiError where
    show = Txt.unpack . toText

instance ToJSON ApiError where
    toJSON err = Aeson.object $
        [ "error"   Aeson..= err.code
        , "message" Aeson..= err.message
        ] <> fieldPair
      where
        fieldPair :: [Pair]
        fieldPair = case err.field of
            Nothing -> []
            Just f  -> ["field" Aeson..= f]

instance FromJSON ApiError where
    parseJSON = Aeson.withObject "ApiError" $ \ obj ->
        ApiError
            <$> obj Aeson..:  "error"
            <*> obj Aeson..:  "message"
            <*> obj Aeson..:? "field"

-- | ドメインの 'ValidationError' をクライアント向け 'ApiError' へ変換する。
-- 本文サイズ超過のみ HTTP 413 相当の 'ErrPayloadTooLarge' へ振り分ける。
-- @field@ には 'validationErrorField' の結果を既定設定するため、単数形クエリパラメータ
-- (@tag@ 等) のエラーでは必要に応じて呼び出し側で @field@ を上書きする。
validationErrorToApiError :: ValidationError -> ApiError
validationErrorToApiError err@BodyTooLarge{} = ApiError
    { code    = ErrPayloadTooLarge
    , message = toText err
    , field   = Just (validationErrorField err)
    }
validationErrorToApiError err = ApiError
    { code    = ErrValidation
    , message = toText err
    , field   = Just (validationErrorField err)
    }
