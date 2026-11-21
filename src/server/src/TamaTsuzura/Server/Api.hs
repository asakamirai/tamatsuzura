{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module TamaTsuzura.Server.Api
    ( -- * 認証コンテキスト・ヘッダ型
      AuthScheme(..)
    , SessionAuth
    , SessionCsrfAuth
    , SessionAuthContext(..)
    , CsrfVerifiedSession(..)
    , RawCookieHeader(..)
      -- * Servant API 型定義
    , TamaTsuzuraApi
    , AuthApi
    , DocumentsApi
    , DocumentByIdApi
    , DocumentRevisionsApi
    , DocumentRevisionByNumberApi
    , tamaTsuzuraApiProxy
      -- * リクエストパラメータ検証
    , RawDocumentListQuery(..)
    , emptyRawDocumentListQuery
    , parseDocumentListQuery
    , RawRevisionListQuery(..)
    , emptyRawRevisionListQuery
    , RevisionListQuery(..)
    , defaultRevisionListQuery
    , parseRevisionListQuery
    , parseDocumentIdParam
    , parseRevisionNumberParam
      -- * エラーレスポンス変換・フォーマッタ
    , jsonContentType
    , apiErrorHttpStatus
    , apiErrorToServerError
    , apiErrorFormatters
    ) where

import           Data.Aeson                       (encode)
import           Data.Bifunctor                   (first)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS8
import qualified Data.Char                        as Char
import           Data.Proxy                       (Proxy(..))
import           Data.Text                        (Text)
import qualified Data.Text                        as Txt
import qualified Data.Text.Encoding               as TxtEnc
import qualified Network.HTTP.Types               as Http
import           Servant
    ( AuthProtect
    , Capture
    , Delete
    , ErrorFormatters(..)
    , FromHttpApiData(..)
    , Get
    , Header
    , Headers
    , IsSecure
    , JSON
    , NoContent
    , Post
    , PostCreated
    , Put
    , QueryParam
    , ReqBody
    , ServerError(..)
    , StdMethod(DELETE, POST)
    , Verb
    , defaultErrorFormatters
    , type (:<|>)
    , type (:>)
    )
import           Servant.Server.Experimental.Auth (AuthServerData)
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
import           TamaTsuzura.Common.Auth
    ( AuthSessionResponse
    , LoginRequest
    , RegisterRequest
    )
import           TamaTsuzura.Common.Document
    ( Document
    , DocumentId
    , FolderPath
    , RevisionNumber
    , Tag
    , mkDocumentId
    , mkFolderPath
    , mkRevisionNumber
    , mkTag
    )
import           TamaTsuzura.Common.Revision
    ( RestoreRevisionRequest
    , RevisionDetail
    , RevisionListResponse
    )
import           TamaTsuzura.Common.Validation    (maxListLimit, minListLimit, minRevisionNumber)
import           TamaTsuzura.Server.Storage.Auth  (AuthenticatedSession)
import           Text.Read                        (readMaybe)

-- | Servant 'AuthProtect' 用の独自認証方式タグ。
-- 型レベル文字列 (@Symbol@) を直接タグに用いると 'AuthServerData' が孤立型族インスタンス
-- (@-Worphans@) となるため、本モジュール固有の昇格データ型を用いる。
data AuthScheme
    = CookieSession
      -- ^ 読み取り専用エンドポイント向け Cookie セッション認証
    | CookieSessionCsrf
      -- ^ 状態変更エンドポイント向け Cookie セッション＋CSRF トークン認証

-- | 読み取り専用エンドポイント向け Cookie セッション認証タグ。
type SessionAuth = AuthProtect 'CookieSession

-- | 状態変更エンドポイント向け Cookie セッション＋CSRF トークン認証タグ。
type SessionCsrfAuth = AuthProtect 'CookieSessionCsrf

-- | 読み取り専用認証 ('SessionAuth') を通過したセッションコンテキスト。
-- 'CsrfVerifiedSession' と別型に分離することで、Servant の 'Context' 解決を一意にしつつ
-- 状態変更ハンドラでの CSRF 検証漏れをコンパイル時に防止する。
newtype SessionAuthContext = SessionAuthContext
    { authenticated :: AuthenticatedSession
      -- ^ SQLite で検証済みのセッションおよび所有ユーザープロフィール
    } deriving (Eq, Show)

-- | Cookie セッション認証および @X-CSRF-Token@ 照合 ('SessionCsrfAuth') を通過したセッションコンテキスト。
-- すべての状態変更エンドポイントハンドラは本型を引数に要求することで CSRF 検証を型レベルで強制する。
newtype CsrfVerifiedSession = CsrfVerifiedSession
    { authenticated :: AuthenticatedSession
      -- ^ SQLite で検証済みかつ CSRF 照合済みのセッションおよび所有ユーザープロフィール
    } deriving (Eq, Show)

type instance AuthServerData SessionAuth     = SessionAuthContext
type instance AuthServerData SessionCsrfAuth = CsrfVerifiedSession

-- | @Cookie@ ヘッダを UTF-8 検証で 400 にせず生のバイト列のまま受け取るための薄いラッパ。
-- 同一オリジン上の無関係な第三者 Cookie に非 UTF-8 値が含まれていても、ログイン・新規登録が
-- 誤って 400 Bad Request にならないようにするために用いる。
newtype RawCookieHeader = RawCookieHeader
    { unWrap :: ByteString
      -- ^ 生の @Cookie@ ヘッダ値バイト列
    } deriving (Eq, Show)

instance FromHttpApiData RawCookieHeader where
    parseUrlPiece = Right . RawCookieHeader . TxtEnc.encodeUtf8
    parseHeader   = Right . RawCookieHeader

-- | TamaTsuzura REST API 全体の Servant 型定義。
-- @docs/api/openapi.yaml@ の全エンドポイント・HTTP メソッド・ステータスコード・認証要件と 1 対 1 に対応する。
type TamaTsuzuraApi
    = "api"
        :> (    "health"
                  :> Get '[JSON] HealthStatus
           :<|> "auth"
                  :> AuthApi
           :<|> "documents"
                  :> DocumentsApi
           :<|> "tags"
                  :> SessionAuth
                  :> Get '[JSON] TagListResponse
           :<|> "folders"
                  :> SessionAuth
                  :> Get '[JSON] FolderListResponse
           )

-- | 認証・セッション管理 (@/api/auth/...@) の Servant 型定義。
type AuthApi
    =      "register"
        :> IsSecure
        :> Header "Cookie" RawCookieHeader
        :> ReqBody '[JSON] RegisterRequest
        :> PostCreated '[JSON] (Headers '[Header "Set-Cookie" Text] AuthSessionResponse)
      :<|> "login"
        :> IsSecure
        :> Header "Cookie" RawCookieHeader
        :> ReqBody '[JSON] LoginRequest
        :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] AuthSessionResponse)
      :<|> "logout"
        :> SessionCsrfAuth
        :> IsSecure
        :> Verb 'POST 204 '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)
      :<|> "me"
        :> SessionAuth
        :> Get '[JSON] AuthSessionResponse

-- | 文書コレクション配下 (@/api/documents...@) の Servant 型定義。
type DocumentsApi
    =      SessionAuth
        :> QueryParam "status" Text
        :> QueryParam "tag" Text
        :> QueryParam "folder" Text
        :> QueryParam "include_subfolders" Text
        :> QueryParam "q" Text
        :> QueryParam "limit" Text
        :> QueryParam "offset" Text
        :> Get '[JSON] DocumentListResponse
      :<|> SessionCsrfAuth
        :> ReqBody '[JSON] CreateDocumentRequest
        :> PostCreated '[JSON] Document
      :<|> Capture "id" Text
        :> DocumentByIdApi

-- | 単一文書リソース (@/api/documents/{id}...@) の Servant 型定義。
type DocumentByIdApi
    =      SessionAuth
        :> Get '[JSON] Document
      :<|> SessionCsrfAuth
        :> ReqBody '[JSON] UpdateDocumentRequest
        :> Put '[JSON] Document
      :<|> SessionCsrfAuth
        :> Delete '[JSON] Document
      :<|> "restore"
        :> SessionCsrfAuth
        :> Post '[JSON] Document
      :<|> "permanent"
        :> SessionCsrfAuth
        :> Verb 'DELETE 204 '[JSON] NoContent
      :<|> "revisions"
        :> DocumentRevisionsApi

-- | 単一文書のリビジョン履歴リソース (@/api/documents/{id}/revisions...@) の Servant 型定義。
type DocumentRevisionsApi
    =      SessionAuth
        :> QueryParam "limit" Text
        :> QueryParam "offset" Text
        :> Get '[JSON] RevisionListResponse
      :<|> Capture "revision" Text
        :> DocumentRevisionByNumberApi

-- | 特定リビジョンリソース (@/api/documents/{id}/revisions/{revision}...@) の Servant 型定義。
type DocumentRevisionByNumberApi
    =      SessionAuth
        :> Get '[JSON] RevisionDetail
      :<|> "restore"
        :> SessionCsrfAuth
        :> ReqBody '[JSON] RestoreRevisionRequest
        :> Post '[JSON] Document

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
    parseLimit = parseListLimitParam

    parseOffset :: Maybe Text -> Either ApiError ListOffset
    parseOffset = parseListOffsetParam

    withField :: Text -> ApiError -> ApiError
    withField fieldName err = ApiError
        { code    = err.code
        , message = err.message
        , field   = Just fieldName
        }

-- | @GET /api/documents/{id}/revisions@ の未検証クエリパラメータ群。
data RawRevisionListQuery = RawRevisionListQuery
    { rawLimit  :: !(Maybe Text)
      -- ^ @limit@ クエリ文字列
    , rawOffset :: !(Maybe Text)
      -- ^ @offset@ クエリ文字列
    } deriving (Eq, Show)

-- | 全クエリパラメータ未指定を表す 'RawRevisionListQuery'。
emptyRawRevisionListQuery :: RawRevisionListQuery
emptyRawRevisionListQuery = RawRevisionListQuery
    { rawLimit  = Nothing
    , rawOffset = Nothing
    }

-- | リビジョン履歴一覧取得の検証済みページネーション条件。
data RevisionListQuery = RevisionListQuery
    { limit  :: !ListLimit
      -- ^ 1 回の取得件数上限
    , offset :: !ListOffset
      -- ^ 取得開始位置のオフセット
    } deriving (Eq, Show)

-- | パラメータ未指定時の既定リビジョン一覧クエリ条件。
defaultRevisionListQuery :: RevisionListQuery
defaultRevisionListQuery = RevisionListQuery
    { limit  = defaultListLimitValue
    , offset = defaultListOffsetValue
    }

-- | 未検証リビジョン一覧クエリを検証済み 'RevisionListQuery' へ変換する。
parseRevisionListQuery :: RawRevisionListQuery -> Either ApiError RevisionListQuery
parseRevisionListQuery raw = do
    limit  <- parseListLimitParam raw.rawLimit
    offset <- parseListOffsetParam raw.rawOffset
    Right RevisionListQuery
        { limit  = limit
        , offset = offset
        }

-- | URL パスセグメントの文書 ID を検証して 'DocumentId' へ変換する。
parseDocumentIdParam :: Text -> Either ApiError DocumentId
parseDocumentIdParam raw = first validationErrorToApiError (mkDocumentId raw)

-- | URL パスセグメントのリビジョン番号 (1 以上の整数) を検証して 'RevisionNumber' へ変換する。
parseRevisionNumberParam :: Text -> Either ApiError RevisionNumber
parseRevisionNumberParam raw = do
    rawInt <- parseBoundedInteger
        "revision"
        raw
        (toInteger minRevisionNumber)
        (toInteger (maxBound :: Int))
        revisionRangeMessage
    first validationErrorToApiError (mkRevisionNumber rawInt)
  where
    revisionRangeMessage :: Text -> Text
    revisionRangeMessage rawVal =
        "Revision number must be an integer >= "
            <> Txt.pack (show minRevisionNumber)
            <> " within supported range, got "
            <> rawVal

parseListLimitParam :: Maybe Text -> Either ApiError ListLimit
parseListLimitParam Nothing  = Right defaultListLimitValue
parseListLimitParam (Just s) = do
    rawInt <- parseBoundedInteger "limit" s (toInteger minListLimit) (toInteger maxListLimit) limitRangeMessage
    first validationErrorToApiError (mkListLimit rawInt)
  where
    limitRangeMessage :: Text -> Text
    limitRangeMessage rawVal =
        "List limit "
            <> rawVal
            <> " is out of range ("
            <> Txt.pack (show minListLimit)
            <> ".."
            <> Txt.pack (show maxListLimit)
            <> ")"

parseListOffsetParam :: Maybe Text -> Either ApiError ListOffset
parseListOffsetParam Nothing  = Right defaultListOffsetValue
parseListOffsetParam (Just s) = do
    rawInt <- parseBoundedInteger "offset" s 0 (toInteger (maxBound :: Int)) offsetRangeMessage
    first validationErrorToApiError (mkListOffset rawInt)
  where
    offsetRangeMessage :: Text -> Text
    offsetRangeMessage rawVal =
        "List offset must be a non-negative integer within supported range, got " <> rawVal

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
parseStrictIntegerText raw
    | Txt.length raw > maxStrictIntChars = Nothing
    | otherwise = case Txt.unpack raw of
        "" -> Nothing
        '-' : digits
            | not (null digits) && all Char.isDigit digits -> readMaybe ('-' : digits)
            | otherwise                                    -> Nothing
        digits
            | all Char.isDigit digits -> readMaybe digits
            | otherwise               -> Nothing
  where
    -- 64-bit Int (19 桁 + 符号 1 文字) を大きく超える極長数字列での多倍長整数変換を事前に拒否する
    maxStrictIntChars :: Int
    maxStrictIntChars = 20
