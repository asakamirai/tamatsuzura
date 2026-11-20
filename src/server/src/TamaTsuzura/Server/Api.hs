{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module TamaTsuzura.Server.Api
    ( -- * Servant API 型定義
      TamaTsuzuraApi
    , DocumentsApi
    , DocumentByIdApi
    , tamaTsuzuraApiProxy
      -- * リクエストパラメータ検証
    , RawDocumentListQuery(..)
    , emptyRawDocumentListQuery
    , parseDocumentListQuery
    , parseDocumentIdParam
      -- * エラーレスポンス変換・フォーマッタ
    , jsonContentType
    , apiErrorHttpStatus
    , apiErrorToServerError
    , apiErrorFormatters
    ) where

import           Data.Aeson                    (encode)
import           Data.Bifunctor                (first)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as BS8
import qualified Data.Char                     as Char
import           Data.Proxy                    (Proxy(..))
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import qualified Network.HTTP.Types            as Http
import           Servant
    ( Capture
    , Delete
    , ErrorFormatters(..)
    , Get
    , JSON
    , NoContent
    , Post
    , PostCreated
    , Put
    , QueryParam
    , ReqBody
    , ServerError(..)
    , StdMethod(DELETE)
    , Verb
    , defaultErrorFormatters
    , type (:<|>)
    , type (:>)
    )
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , CreateDocumentRequest
    , DocumentListQuery(..)
    , DocumentListResponse
    , DocumentStatusFilter
    , FolderListResponse
    , HealthStatus
    , ListLimit
    , ListOffset
    , SearchQuery
    , TagListResponse
    , UpdateDocumentRequest
    , defaultDocumentStatusFilter
    , defaultListLimitValue
    , defaultListOffsetValue
    , mkListLimit
    , mkListOffset
    , mkSearchQuery
    , parseDocumentStatusFilter
    , validationErrorToApiError
    )
import           TamaTsuzura.Common.Document
    ( Document
    , DocumentId
    , FolderPath
    , Tag
    , mkDocumentId
    , mkFolderPath
    , mkTag
    )
import           TamaTsuzura.Common.Validation (maxListLimit, minListLimit)
import           Text.Read                     (readMaybe)

-- | TamaTsuzura REST API 全体の Servant 型定義。
-- @docs/api/openapi.yaml@ の全エンドポイント・HTTP メソッド・ステータスコードと 1 対 1 に対応する。
type TamaTsuzuraApi
    = "api"
        :> (    "health"
                  :> Get '[JSON] HealthStatus
           :<|> "documents"
                  :> DocumentsApi
           :<|> "tags"
                  :> Get '[JSON] TagListResponse
           :<|> "folders"
                  :> Get '[JSON] FolderListResponse
           )

-- | 文書コレクション配下 (@/api/documents...@) の Servant 型定義。
type DocumentsApi
    =      QueryParam "status" Text
        :> QueryParam "tag" Text
        :> QueryParam "folder" Text
        :> QueryParam "include_subfolders" Text
        :> QueryParam "q" Text
        :> QueryParam "limit" Text
        :> QueryParam "offset" Text
        :> Get '[JSON] DocumentListResponse
      :<|> ReqBody '[JSON] CreateDocumentRequest
        :> PostCreated '[JSON] Document
      :<|> Capture "id" Text
        :> DocumentByIdApi

-- | 単一文書リソース (@/api/documents/{id}...@) の Servant 型定義。
type DocumentByIdApi
    =      Get '[JSON] Document
      :<|> ReqBody '[JSON] UpdateDocumentRequest
        :> Put '[JSON] Document
      :<|> Delete '[JSON] Document
      :<|> "restore"
        :> Post '[JSON] Document
      :<|> "permanent"
        :> Verb 'DELETE 204 '[JSON] NoContent

-- | Servant のルーティング解決に用いる API プロキシ。
tamaTsuzuraApiProxy :: Proxy TamaTsuzuraApi
tamaTsuzuraApiProxy = Proxy

-- | @GET /api/documents@ の未検証クエリパラメータ群。
-- 各パラメータを 'Text' として受け取り、'parseDocumentListQuery' でドメイン型へ検証変換することで
-- どのクエリパラメータが不正であったか (@field@) を厳密な 'ApiError' JSON として返却できる。
data RawDocumentListQuery = RawDocumentListQuery
    { rawStatus            :: !(Maybe Text)
      -- ^ @status@ クエリ文字列
    , rawTag               :: !(Maybe Text)
      -- ^ @tag@ クエリ文字列
    , rawFolder            :: !(Maybe Text)
      -- ^ @folder@ クエリ文字列
    , rawIncludeSubfolders :: !(Maybe Text)
      -- ^ @include_subfolders@ クエリ文字列
    , rawSearch            :: !(Maybe Text)
      -- ^ @q@ クエリ文字列
    , rawLimit             :: !(Maybe Text)
      -- ^ @limit@ クエリ文字列
    , rawOffset            :: !(Maybe Text)
      -- ^ @offset@ クエリ文字列
    } deriving (Eq, Show)

-- | 全クエリパラメータ未指定を表す 'RawDocumentListQuery'。
emptyRawDocumentListQuery :: RawDocumentListQuery
emptyRawDocumentListQuery = RawDocumentListQuery
    { rawStatus            = Nothing
    , rawTag               = Nothing
    , rawFolder            = Nothing
    , rawIncludeSubfolders = Nothing
    , rawSearch            = Nothing
    , rawLimit             = Nothing
    , rawOffset            = Nothing
    }

-- | 未検証クエリパラメータ群を検証済み 'DocumentListQuery' へ変換する。
parseDocumentListQuery :: RawDocumentListQuery -> Either ApiError DocumentListQuery
parseDocumentListQuery raw = do
    status            <- parseStatus raw.rawStatus
    mTag              <- parseTag raw.rawTag
    mFolder           <- parseFolder raw.rawFolder
    includeSubfolders <- parseIncludeSubfolders raw.rawIncludeSubfolders
    mSearch           <- parseSearch raw.rawSearch
    limit             <- parseLimit raw.rawLimit
    offset            <- parseOffset raw.rawOffset
    Right DocumentListQuery
        { status            = status
        , tag               = mTag
        , folder            = mFolder
        , includeSubfolders = includeSubfolders
        , search            = mSearch
        , limit             = limit
        , offset            = offset
        }
  where
    parseStatus :: Maybe Text -> Either ApiError DocumentStatusFilter
    parseStatus Nothing  = Right defaultDocumentStatusFilter
    parseStatus (Just s) = first validationErrorToApiError (parseDocumentStatusFilter s)

    parseTag :: Maybe Text -> Either ApiError (Maybe Tag)
    parseTag Nothing  = Right Nothing
    parseTag (Just s) =
        fmap Just (first (withField "tag" . validationErrorToApiError) (mkTag s))

    parseFolder :: Maybe Text -> Either ApiError (Maybe FolderPath)
    parseFolder Nothing  = Right Nothing
    parseFolder (Just s) = fmap Just (first validationErrorToApiError (mkFolderPath s))

    parseIncludeSubfolders :: Maybe Text -> Either ApiError Bool
    parseIncludeSubfolders Nothing        = Right False
    parseIncludeSubfolders (Just "true")  = Right True
    parseIncludeSubfolders (Just "false") = Right False
    parseIncludeSubfolders (Just other)   = Left ApiError
        { code    = ErrValidation
        , message = "Invalid boolean for include_subfolders: " <> other <> " (expected \"true\" or \"false\")"
        , field   = Just "include_subfolders"
        }

    parseSearch :: Maybe Text -> Either ApiError (Maybe SearchQuery)
    parseSearch Nothing  = Right Nothing
    parseSearch (Just s) = first validationErrorToApiError (mkSearchQuery s)

    parseLimit :: Maybe Text -> Either ApiError ListLimit
    parseLimit Nothing  = Right defaultListLimitValue
    parseLimit (Just s) = do
        rawInt <- parseBoundedInteger "limit" s (toInteger minListLimit) (toInteger maxListLimit) limitRangeMessage
        first validationErrorToApiError (mkListLimit rawInt)

    limitRangeMessage :: Text -> Text
    limitRangeMessage rawVal =
        "List limit "
            <> rawVal
            <> " is out of range ("
            <> Txt.pack (show minListLimit)
            <> ".."
            <> Txt.pack (show maxListLimit)
            <> ")"

    parseOffset :: Maybe Text -> Either ApiError ListOffset
    parseOffset Nothing  = Right defaultListOffsetValue
    parseOffset (Just s) = do
        rawInt <- parseBoundedInteger "offset" s 0 (toInteger (maxBound :: Int)) offsetRangeMessage
        first validationErrorToApiError (mkListOffset rawInt)

    offsetRangeMessage :: Text -> Text
    offsetRangeMessage rawVal =
        "List offset must be a non-negative integer within supported range, got " <> rawVal

    withField :: Text -> ApiError -> ApiError
    withField fieldName err = ApiError
        { code    = err.code
        , message = err.message
        , field   = Just fieldName
        }

-- | URL パスセグメントの文書 ID を検証して 'DocumentId' へ変換する。
parseDocumentIdParam :: Text -> Either ApiError DocumentId
parseDocumentIdParam raw = first validationErrorToApiError (mkDocumentId raw)

-- | REST API の標準 JSON Content-Type ヘッダ値。
jsonContentType :: ByteString
jsonContentType = "application/json; charset=utf-8"

-- | 'ApiErrorCode' を対応する HTTP ステータスコードへ写像する。
apiErrorHttpStatus :: ApiErrorCode -> Http.Status
apiErrorHttpStatus ErrValidation       = Http.status400
apiErrorHttpStatus ErrUnauthorized     = Http.status401
apiErrorHttpStatus ErrForbidden        = Http.status403
apiErrorHttpStatus ErrNotFound         = Http.status404
apiErrorHttpStatus ErrConflict         = Http.status409
apiErrorHttpStatus ErrPayloadTooLarge  = Http.status413
apiErrorHttpStatus ErrMethodNotAllowed = Http.status405
apiErrorHttpStatus ErrInternalServer   = Http.status500

-- | 構造化 'ApiError' を Servant の JSON 'ServerError' へ変換する。
apiErrorToServerError :: ApiError -> ServerError
apiErrorToServerError apiErr =
    let status :: Http.Status
        status = apiErrorHttpStatus apiErr.code
    in  ServerError
            { errHTTPCode     = Http.statusCode status
            , errReasonPhrase = BS8.unpack (Http.statusMessage status)
            , errBody         = encode apiErr
            , errHeaders      = [(Http.hContentType, jsonContentType)]
            }

-- | Servant のフレームワーク起因エラー (JSON デコード失敗・404 等) を
-- OpenAPI 準拠の JSON 'ApiError' 形式で返却するための 'ErrorFormatters'。
apiErrorFormatters :: ErrorFormatters
apiErrorFormatters = defaultErrorFormatters
    { bodyParserErrorFormatter  = formatBadRequest
    , urlParseErrorFormatter    = formatBadRequest
    , headerParseErrorFormatter = formatBadRequest
    , notFoundErrorFormatter    = formatNotFound
    }
  where
    formatBadRequest :: a -> b -> String -> ServerError
    formatBadRequest _ _ msg =
        apiErrorToServerError ApiError
            { code    = ErrValidation
            , message = Txt.pack msg
            , field   = Nothing
            }

    formatNotFound :: a -> ServerError
    formatNotFound _ =
        apiErrorToServerError ApiError
            { code    = ErrNotFound
            , message = "Endpoint not found"
            , field   = Nothing
            }

parseBoundedInteger ::
       Text
    -> Text
    -> Integer
    -> Integer
    -> (Text -> Text)
    -> Either ApiError Int
parseBoundedInteger paramName raw minVal maxVal mkOutOfRangeMsg =
    case parseStrictIntegerText raw of
        Nothing -> Left ApiError
            { code    = ErrValidation
            , message = "Invalid integer for " <> paramName <> ": " <> raw
            , field   = Just paramName
            }
        Just val
            | val < minVal || val > maxVal -> Left ApiError
                { code    = ErrValidation
                , message = mkOutOfRangeMsg raw
                , field   = Just paramName
                }
            | otherwise -> Right (fromInteger val)

parseStrictIntegerText :: Text -> Maybe Integer
parseStrictIntegerText raw = case Txt.unpack raw of
    "" -> Nothing
    '-' : digits
        | not (null digits) && all Char.isDigit digits -> readMaybe ('-' : digits)
        | otherwise                                    -> Nothing
    digits
        | all Char.isDigit digits -> readMaybe digits
        | otherwise               -> Nothing
