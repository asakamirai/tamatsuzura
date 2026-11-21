{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module TamaTsuzura.Client.Api
    ( -- * ベース URL・HTTP トランスポート抽象
      ApiBaseUrl
    , defaultApiBaseUrl
    , mkApiBaseUrl
    , csrfHeaderName
    , HttpMethod(..)
    , HttpRequest(..)
    , HttpResponse(..)
    , HttpTransport(..)
    , browserFetchTransport
    , ApiClient(..)
    , defaultApiClient
      -- * クライアントエラー型
    , ApiClientError(..)
    , isUnauthorizedError
      -- * 純粋な URL 構築・リクエスト生成・レスポンス解析
    , percentEncodeQueryComponent
    , buildDocumentListQueryString
    , buildHealthRequest
    , buildRegisterRequest
    , buildLoginRequest
    , buildLogoutRequest
    , buildGetCurrentUserRequest
    , buildListDocumentsRequest
    , buildCreateDocumentRequest
    , buildGetDocumentRequest
    , buildUpdateDocumentRequest
    , buildSoftDeleteDocumentRequest
    , buildRestoreDocumentRequest
    , buildPermanentDeleteDocumentRequest
    , buildListTagsRequest
    , buildListFoldersRequest
    , buildRevisionListQueryString
    , buildListRevisionsRequest
    , buildGetRevisionRequest
    , buildRestoreRevisionRequest
    , parseJsonResponse
    , parseNoContentResponse
      -- * 認証 REST API クライアント操作
    , registerUser
    , loginUser
    , logoutUser
    , getCurrentUser
      -- * 文書 REST API クライアント操作
    , fetchHealth
    , listDocuments
    , createDocument
    , getDocument
    , updateDocument
    , softDeleteDocument
    , restoreDocument
    , permanentDeleteDocument
    , listTags
    , listFolders
      -- * リビジョン履歴 REST API クライアント操作
    , listDocumentRevisions
    , getDocumentRevision
    , restoreDocumentRevision
    ) where

import           Data.Aeson                    (FromJSON, ToJSON)
import qualified Data.Aeson                    as Aeson
import           Data.Bits                     (shiftR, (.&.))
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LazyBS
import qualified Data.Char                     as Char
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import qualified Data.Text.Encoding            as TxtEnc
import           Data.Word                     (Word8)
import           TamaTsuzura.Client.Ffi        (RawFetchResult(..), fetchRaw)
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , CreateDocumentRequest
    , DocumentListQuery(..)
    , DocumentListResponse
    , DocumentStatusFilter(..)
    , FolderListResponse
    , HealthStatus
    , ListLimit
    , ListOffset
    , TagListResponse
    , UpdateDocumentRequest
    , defaultListLimitValue
    , defaultListOffsetValue
    , validateCreateDocumentRequest
    , validateUpdateDocumentRequest
    )
import           TamaTsuzura.Common.Auth
    ( AuthSessionResponse
    , CsrfToken
    , LoginRequest
    , RegisterRequest
    , validateRegisterRequest
    )
import           TamaTsuzura.Common.Document   (Document, DocumentId, RevisionNumber)
import           TamaTsuzura.Common.Revision
    ( RestoreRevisionRequest
    , RevisionDetail
    , RevisionListResponse
    )
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation (ValidationError)

-- | REST API 接続先のベース URL (末尾スラッシュを正規化除去済み)。
-- 既定値は空文字 (同一オリジンの相対パス @/api/...@) とし、CGI サブディレクトリ配置やテスト接続時は
-- 任意のプレフィックス (例: @"/notes"@ や @"http://127.0.0.1:8080"@) を指定できる。
newtype ApiBaseUrl = ApiBaseUrl Text
    deriving (Eq, Ord, ToText)

instance Show ApiBaseUrl where
    show = Txt.unpack . toText

-- | 同一オリジン相対パス (@/api/...@) を指す既定の 'ApiBaseUrl'。
defaultApiBaseUrl :: ApiBaseUrl
defaultApiBaseUrl = ApiBaseUrl ""

-- | 文字列の前後の空白と末尾のスラッシュを正規化して 'ApiBaseUrl' を構築する。
mkApiBaseUrl :: Text -> ApiBaseUrl
mkApiBaseUrl raw = ApiBaseUrl (Txt.dropWhileEnd (== '/') (Txt.strip raw))

-- | 状態変更 API リクエストへ添付する CSRF トークン HTTP ヘッダ名 (@X-CSRF-Token@)。
csrfHeaderName :: Text
csrfHeaderName = "X-CSRF-Token"

-- | REST API 呼び出しで使用する HTTP メソッド。
data HttpMethod
    = MethodGet
      -- ^ HTTP GET
    | MethodPost
      -- ^ HTTP POST
    | MethodPut
      -- ^ HTTP PUT
    | MethodDelete
      -- ^ HTTP DELETE
    deriving (Eq, Ord, Enum, Bounded)

instance ToText HttpMethod where
    toText MethodGet    = "GET"
    toText MethodPost   = "POST"
    toText MethodPut    = "PUT"
    toText MethodDelete = "DELETE"

instance Show HttpMethod where
    show = Txt.unpack . toText

-- | トランスポート層へ渡す純粋な HTTP リクエスト表現。
data HttpRequest = HttpRequest
    { method  :: !HttpMethod
      -- ^ HTTP メソッド
    , url     :: !Text
      -- ^ ベース URL およびクエリ文字列を含むリクエスト先 URL
    , headers :: ![(Text, Text)]
      -- ^ HTTP リクエストヘッダ一覧
    , body    :: !(Maybe ByteString)
      -- ^ UTF-8 エンコード済みリクエストボディ (GET / DELETE 等では 'Nothing')
    } deriving (Eq, Show)

-- | トランスポート層から返却される生 HTTP レスポンス。
data HttpResponse = HttpResponse
    { statusCode :: !Int
      -- ^ HTTP ステータスコード
    , body       :: !ByteString
      -- ^ 生レスポンスボディ (UTF-8)
    } deriving (Eq, Show)

-- | HTTP 通信を実行する差し替え可能なトランスポート抽象。
-- 本番 (Wasm) では 'browserFetchTransport' を使用し、自動テストではモックや WAI 接続を注入する。
newtype HttpTransport = HttpTransport
    { sendRequest :: HttpRequest -> IO (Either Text HttpResponse)
      -- ^ 単一 HTTP リクエストを送信し、ネットワークエラーまたは生レスポンスを返す
    }

-- | ブラウザの Fetch API ('TamaTsuzura.Client.Ffi.fetchRaw') を用いる標準トランスポート。
browserFetchTransport :: HttpTransport
browserFetchTransport = HttpTransport $ \ req -> do
    rawRes <- fetchRaw req.url (toText req.method) req.headers req.body
    pure $ case rawRes of
        RawFetchOk code bytes -> Right HttpResponse
            { statusCode = code
            , body       = bytes
            }
        RawFetchFailed msg -> Left msg

-- | ベース URL とトランスポートを束ねた API クライアントハンドル。
data ApiClient = ApiClient
    { baseUrl   :: !ApiBaseUrl
      -- ^ API 接続先ベース URL
    , transport :: !HttpTransport
      -- ^ HTTP 送信トランスポート
    }

-- | ブラウザ Fetch トランスポートを用いる標準の 'ApiClient' を構築する。
defaultApiClient :: ApiBaseUrl -> ApiClient
defaultApiClient base = ApiClient
    { baseUrl   = base
    , transport = browserFetchTransport
    }

-- | API クライアント呼び出しで発生しうる失敗理由。
data ApiClientError
    = ClientValidationError !ValidationError
      -- ^ 送信前のドメイン検証に失敗した (例: 本文 5 MiB 超過)
    | ClientNetworkError !Text
      -- ^ ネットワーク切断・到達不能等のトランスポート層エラー
    | ClientApiError !Int !ApiError
      -- ^ サーバから OpenAPI 準拠の構造化 JSON エラーを受信した (HTTP ステータス, ApiError)
    | ClientHttpError !Int !Text
      -- ^ 予期しない HTTP ステータス、または非 JSON のエラー応答 (プロキシ/Webサーバ起因等)
    | ClientDecodeError !Text
      -- ^ 成功レスポンスの JSON デコードに失敗した
    deriving (Eq)

instance ToText ApiClientError where
    toText (ClientValidationError err) =
        "validation error: " <> toText err
    toText (ClientNetworkError msg) =
        "network error: " <> msg
    toText (ClientApiError status apiErr) =
        "HTTP " <> Txt.pack (show status) <> " (" <> toText apiErr <> ")"
    toText (ClientHttpError status detail)
        | Txt.null detail = "HTTP " <> Txt.pack (show status)
        | otherwise       = "HTTP " <> Txt.pack (show status) <> ": " <> detail
    toText (ClientDecodeError msg) =
        "response decode error: " <> msg

instance Show ApiClientError where
    show = Txt.unpack . toText

-- | エラーが HTTP 401 未認証 (セッション未開始・期限切れ・失効) に該当するか判定する。
isUnauthorizedError :: ApiClientError -> Bool
isUnauthorizedError (ClientApiError status apiErr) =
    status == 401 || apiErr.code == ErrUnauthorized
isUnauthorizedError (ClientHttpError status _) =
    status == 401
isUnauthorizedError _ = False

-- | クエリパラメータのキー・値を RFC 3986 非予約文字以外 UTF-8 パーセントエンコードする。
-- 日本語のタグ・フォルダ名・検索語や @&@ / @=@ / @+@ / @%@ / 空白を安全に転送するために使用する。
percentEncodeQueryComponent :: Text -> Text
percentEncodeQueryComponent =
    Txt.concat . map encodeWord8 . BS.unpack . TxtEnc.encodeUtf8
  where
    encodeWord8 :: Word8 -> Text
    encodeWord8 byte
        | isUnreservedByte byte = Txt.singleton (Char.chr (fromIntegral byte))
        | otherwise             = percentHex byte

    isUnreservedByte :: Word8 -> Bool
    isUnreservedByte b =
        (b >= 0x41 && b <= 0x5A)        -- A-Z
            || (b >= 0x61 && b <= 0x7A) -- a-z
            || (b >= 0x30 && b <= 0x39) -- 0-9
            || b == 0x2D                -- '-'
            || b == 0x2E                -- '.'
            || b == 0x5F                -- '_'
            || b == 0x7E                -- '~'

    percentHex :: Word8 -> Text
    percentHex b =
        let hi :: Word8
            hi = (b `shiftR` 4) .&. 0x0F

            lo :: Word8
            lo = b .&. 0x0F
        in  Txt.pack ['%', hexDigit hi, hexDigit lo]

    hexDigit :: Word8 -> Char
    hexDigit n
        | n < 10    = Char.chr (Char.ord '0' + fromIntegral n)
        | otherwise = Char.chr (Char.ord 'A' + fromIntegral (n - 10))

-- | 'DocumentListQuery' から先頭 @?@ を含むクエリ文字列を構築する (すべて既定値なら空文字)。
buildDocumentListQueryString :: DocumentListQuery -> Text
buildDocumentListQueryString query =
    case queryPairs of
        []    -> ""
        pairs -> "?" <> Txt.intercalate "&" (map formatPair pairs)
  where
    queryPairs :: [(Text, Text)]
    queryPairs = concat
        [ [ ("status", toText query.status) | query.status /= StatusActive ]
        , [ ("tag", toText t) | Just t <- [query.tag] ]
        , [ ("folder", toText f) | Just f <- [query.folder] ]
        , [ ("include_subfolders", "true")
          | query.includeSubfolders
          , Just _ <- [query.folder]
          ]
        , [ ("q", toText s) | Just s <- [query.search] ]
        , [ ("limit", toText query.limit) | query.limit /= defaultListLimitValue ]
        , [ ("offset", toText query.offset) | query.offset /= defaultListOffsetValue ]
        ]

    formatPair :: (Text, Text) -> Text
    formatPair (k, v) =
        percentEncodeQueryComponent k <> "=" <> percentEncodeQueryComponent v

-- | @GET /api/health@ のリクエストを構築する。
buildHealthRequest :: ApiBaseUrl -> HttpRequest
buildHealthRequest base = HttpRequest
    { method  = MethodGet
    , url     = endpointUrl base "/api/health"
    , headers = acceptJsonHeaders
    , body    = Nothing
    }

-- | @POST /api/auth/register@ のリクエストをドメイン検証付きで構築する。
buildRegisterRequest ::
       ApiBaseUrl
    -> RegisterRequest
    -> Either ValidationError HttpRequest
buildRegisterRequest base req = do
    validated <- validateRegisterRequest req
    Right HttpRequest
        { method  = MethodPost
        , url     = endpointUrl base "/api/auth/register"
        , headers = jsonBodyHeaders
        , body    = Just (encodeJsonBody validated)
        }

-- | @POST /api/auth/login@ のリクエストを構築する。
buildLoginRequest :: ApiBaseUrl -> LoginRequest -> HttpRequest
buildLoginRequest base req = HttpRequest
    { method  = MethodPost
    , url     = endpointUrl base "/api/auth/login"
    , headers = jsonBodyHeaders
    , body    = Just (encodeJsonBody req)
    }

-- | @POST /api/auth/logout@ のリクエストを CSRF ヘッダ付きで構築する。
buildLogoutRequest :: ApiBaseUrl -> CsrfToken -> HttpRequest
buildLogoutRequest base csrf = HttpRequest
    { method  = MethodPost
    , url     = endpointUrl base "/api/auth/logout"
    , headers = withCsrfHeader csrf acceptJsonHeaders
    , body    = Nothing
    }

-- | @GET /api/auth/me@ のリクエストを構築する。
buildGetCurrentUserRequest :: ApiBaseUrl -> HttpRequest
buildGetCurrentUserRequest base = HttpRequest
    { method  = MethodGet
    , url     = endpointUrl base "/api/auth/me"
    , headers = acceptJsonHeaders
    , body    = Nothing
    }

-- | @GET /api/documents@ のリクエストを構築する。
buildListDocumentsRequest :: ApiBaseUrl -> DocumentListQuery -> HttpRequest
buildListDocumentsRequest base query = HttpRequest
    { method  = MethodGet
    , url     = endpointUrl base ("/api/documents" <> buildDocumentListQueryString query)
    , headers = acceptJsonHeaders
    , body    = Nothing
    }

-- | @POST /api/documents@ のリクエストをドメイン検証・CSRF ヘッダ付きで構築する。
buildCreateDocumentRequest ::
       ApiBaseUrl
    -> CsrfToken
    -> CreateDocumentRequest
    -> Either ValidationError HttpRequest
buildCreateDocumentRequest base csrf req = do
    validated <- validateCreateDocumentRequest req
    Right HttpRequest
        { method  = MethodPost
        , url     = endpointUrl base "/api/documents"
        , headers = withCsrfHeader csrf jsonBodyHeaders
        , body    = Just (encodeJsonBody validated)
        }

-- | @GET /api/documents/{id}@ のリクエストを構築する。
buildGetDocumentRequest :: ApiBaseUrl -> DocumentId -> HttpRequest
buildGetDocumentRequest base docId = HttpRequest
    { method  = MethodGet
    , url     = documentEndpointUrl base docId ""
    , headers = acceptJsonHeaders
    , body    = Nothing
    }

-- | @PUT /api/documents/{id}@ のリクエストをドメイン検証・CSRF ヘッダ付きで構築する。
buildUpdateDocumentRequest ::
       ApiBaseUrl
    -> CsrfToken
    -> DocumentId
    -> UpdateDocumentRequest
    -> Either ValidationError HttpRequest
buildUpdateDocumentRequest base csrf docId req = do
    validated <- validateUpdateDocumentRequest req
    Right HttpRequest
        { method  = MethodPut
        , url     = documentEndpointUrl base docId ""
        , headers = withCsrfHeader csrf jsonBodyHeaders
        , body    = Just (encodeJsonBody validated)
        }

-- | @DELETE /api/documents/{id}@ (論理削除・ゴミ箱移動) のリクエストを CSRF ヘッダ付きで構築する。
buildSoftDeleteDocumentRequest :: ApiBaseUrl -> CsrfToken -> DocumentId -> HttpRequest
buildSoftDeleteDocumentRequest base csrf docId = HttpRequest
    { method  = MethodDelete
    , url     = documentEndpointUrl base docId ""
    , headers = withCsrfHeader csrf acceptJsonHeaders
    , body    = Nothing
    }

-- | @POST /api/documents/{id}/restore@ (ゴミ箱からの復元) のリクエストを CSRF ヘッダ付きで構築する。
buildRestoreDocumentRequest :: ApiBaseUrl -> CsrfToken -> DocumentId -> HttpRequest
buildRestoreDocumentRequest base csrf docId = HttpRequest
    { method  = MethodPost
    , url     = documentEndpointUrl base docId "/restore"
    , headers = withCsrfHeader csrf acceptJsonHeaders
    , body    = Nothing
    }

-- | @DELETE /api/documents/{id}/permanent@ (完全削除) のリクエストを CSRF ヘッダ付きで構築する。
buildPermanentDeleteDocumentRequest :: ApiBaseUrl -> CsrfToken -> DocumentId -> HttpRequest
buildPermanentDeleteDocumentRequest base csrf docId = HttpRequest
    { method  = MethodDelete
    , url     = documentEndpointUrl base docId "/permanent"
    , headers = withCsrfHeader csrf acceptJsonHeaders
    , body    = Nothing
    }

-- | @GET /api/tags@ のリクエストを構築する。
buildListTagsRequest :: ApiBaseUrl -> HttpRequest
buildListTagsRequest base = HttpRequest
    { method  = MethodGet
    , url     = endpointUrl base "/api/tags"
    , headers = acceptJsonHeaders
    , body    = Nothing
    }

-- | @GET /api/folders@ のリクエストを構築する。
buildListFoldersRequest :: ApiBaseUrl -> HttpRequest
buildListFoldersRequest base = HttpRequest
    { method  = MethodGet
    , url     = endpointUrl base "/api/folders"
    , headers = acceptJsonHeaders
    , body    = Nothing
    }

-- | リビジョン一覧取得用のクエリ文字列を構築する (指定なしまたは既定値のみなら空文字)。
buildRevisionListQueryString :: Maybe ListLimit -> Maybe ListOffset -> Text
buildRevisionListQueryString mLimit mOffset =
    case queryPairs of
        []    -> ""
        pairs -> "?" <> Txt.intercalate "&" (map formatPair pairs)
  where
    queryPairs :: [(Text, Text)]
    queryPairs =
        [ ("limit", toText l) | Just l <- [mLimit], l /= defaultListLimitValue ]
            <> [ ("offset", toText o) | Just o <- [mOffset], o /= defaultListOffsetValue ]

    formatPair :: (Text, Text) -> Text
    formatPair (k, v) =
        percentEncodeQueryComponent k <> "=" <> percentEncodeQueryComponent v

-- | @GET /api/documents/{id}/revisions@ のリクエストを構築する。
buildListRevisionsRequest ::
       ApiBaseUrl
    -> DocumentId
    -> Maybe ListLimit
    -> Maybe ListOffset
    -> HttpRequest
buildListRevisionsRequest base docId mLimit mOffset = HttpRequest
    { method  = MethodGet
    , url     = documentEndpointUrl base docId ("/revisions" <> buildRevisionListQueryString mLimit mOffset)
    , headers = acceptJsonHeaders
    , body    = Nothing
    }

-- | @GET /api/documents/{id}/revisions/{revision}@ のリクエストを構築する。
buildGetRevisionRequest :: ApiBaseUrl -> DocumentId -> RevisionNumber -> HttpRequest
buildGetRevisionRequest base docId revNo = HttpRequest
    { method  = MethodGet
    , url     = documentEndpointUrl base docId ("/revisions/" <> toText revNo)
    , headers = acceptJsonHeaders
    , body    = Nothing
    }

-- | @POST /api/documents/{id}/revisions/{revision}/restore@ のリクエストを CSRF ヘッダ付きで構築する。
buildRestoreRevisionRequest ::
       ApiBaseUrl
    -> CsrfToken
    -> DocumentId
    -> RevisionNumber
    -> RestoreRevisionRequest
    -> HttpRequest
buildRestoreRevisionRequest base csrf docId revNo req = HttpRequest
    { method  = MethodPost
    , url     = documentEndpointUrl base docId ("/revisions/" <> toText revNo <> "/restore")
    , headers = withCsrfHeader csrf jsonBodyHeaders
    , body    = Just (encodeJsonBody req)
    }

-- | 期待する HTTP ステータスコードの JSON レスポンスを解析する。
-- 4xx/5xx の場合は構造化 'ApiError' としてのデコードを優先し、非 JSON 応答は 'ClientHttpError' へフォールバックする。
parseJsonResponse :: FromJSON a => Int -> HttpResponse -> Either ApiClientError a
parseJsonResponse expectedStatus res
    | res.statusCode == expectedStatus =
        case Aeson.eitherDecodeStrict' res.body of
            Left errMsg -> Left (ClientDecodeError (Txt.pack errMsg))
            Right val   -> Right val
    | otherwise =
        Left (classifyErrorResponse res)

-- | @204 No Content@ レスポンスを解析する。
parseNoContentResponse :: HttpResponse -> Either ApiClientError ()
parseNoContentResponse res
    | res.statusCode == 204 = Right ()
    | otherwise             = Left (classifyErrorResponse res)

-- | 新規ユーザー登録 (@POST /api/auth/register@) を実行する。
registerUser :: ApiClient -> RegisterRequest -> IO (Either ApiClientError AuthSessionResponse)
registerUser client req =
    case buildRegisterRequest client.baseUrl req of
        Left vErr     -> pure (Left (ClientValidationError vErr))
        Right httpReq -> executeRequest client httpReq (parseJsonResponse 201)

-- | ログイン (@POST /api/auth/login@) を実行する。
loginUser :: ApiClient -> LoginRequest -> IO (Either ApiClientError AuthSessionResponse)
loginUser client req =
    executeRequest client (buildLoginRequest client.baseUrl req) (parseJsonResponse 200)

-- | ログアウト (@POST /api/auth/logout@) を実行する。
logoutUser :: ApiClient -> CsrfToken -> IO (Either ApiClientError ())
logoutUser client csrf =
    executeRequest client (buildLogoutRequest client.baseUrl csrf) parseNoContentResponse

-- | 現在の認証セッション情報 (@GET /api/auth/me@) を取得する。
getCurrentUser :: ApiClient -> IO (Either ApiClientError AuthSessionResponse)
getCurrentUser client =
    executeRequest client (buildGetCurrentUserRequest client.baseUrl) (parseJsonResponse 200)

-- | ヘルスチェック (@GET /api/health@) を実行する。
fetchHealth :: ApiClient -> IO (Either ApiClientError HealthStatus)
fetchHealth client =
    executeRequest client (buildHealthRequest client.baseUrl) (parseJsonResponse 200)

-- | 文書サマリ一覧 (@GET /api/documents@) を取得する。
listDocuments :: ApiClient -> DocumentListQuery -> IO (Either ApiClientError DocumentListResponse)
listDocuments client query =
    executeRequest client (buildListDocumentsRequest client.baseUrl query) (parseJsonResponse 200)

-- | 新規文書を作成 (@POST /api/documents@) する。
createDocument :: ApiClient -> CsrfToken -> CreateDocumentRequest -> IO (Either ApiClientError Document)
createDocument client csrf req =
    case buildCreateDocumentRequest client.baseUrl csrf req of
        Left vErr     -> pure (Left (ClientValidationError vErr))
        Right httpReq -> executeRequest client httpReq (parseJsonResponse 201)

-- | 単一文書 (@GET /api/documents/{id}@) を取得する。
getDocument :: ApiClient -> DocumentId -> IO (Either ApiClientError Document)
getDocument client docId =
    executeRequest client (buildGetDocumentRequest client.baseUrl docId) (parseJsonResponse 200)

-- | 稼働中文書を更新 (@PUT /api/documents/{id}@) する。
updateDocument ::
       ApiClient
    -> CsrfToken
    -> DocumentId
    -> UpdateDocumentRequest
    -> IO (Either ApiClientError Document)
updateDocument client csrf docId req =
    case buildUpdateDocumentRequest client.baseUrl csrf docId req of
        Left vErr     -> pure (Left (ClientValidationError vErr))
        Right httpReq -> executeRequest client httpReq (parseJsonResponse 200)

-- | 文書を論理削除 (ゴミ箱移動: @DELETE /api/documents/{id}@) する。
softDeleteDocument :: ApiClient -> CsrfToken -> DocumentId -> IO (Either ApiClientError Document)
softDeleteDocument client csrf docId =
    executeRequest client (buildSoftDeleteDocumentRequest client.baseUrl csrf docId) (parseJsonResponse 200)

-- | ゴミ箱内の文書を復元 (@POST /api/documents/{id}/restore@) する。
restoreDocument :: ApiClient -> CsrfToken -> DocumentId -> IO (Either ApiClientError Document)
restoreDocument client csrf docId =
    executeRequest client (buildRestoreDocumentRequest client.baseUrl csrf docId) (parseJsonResponse 200)

-- | ゴミ箱内の文書を完全削除 (@DELETE /api/documents/{id}/permanent@) する。
permanentDeleteDocument :: ApiClient -> CsrfToken -> DocumentId -> IO (Either ApiClientError ())
permanentDeleteDocument client csrf docId =
    executeRequest client (buildPermanentDeleteDocumentRequest client.baseUrl csrf docId) parseNoContentResponse

-- | 稼働中文書のタグ一覧 (@GET /api/tags@) を取得する。
listTags :: ApiClient -> IO (Either ApiClientError TagListResponse)
listTags client =
    executeRequest client (buildListTagsRequest client.baseUrl) (parseJsonResponse 200)

-- | 稼働中文書のフォルダ一覧 (@GET /api/folders@) を取得する。
listFolders :: ApiClient -> IO (Either ApiClientError FolderListResponse)
listFolders client =
    executeRequest client (buildListFoldersRequest client.baseUrl) (parseJsonResponse 200)

-- | 文書のリビジョン履歴サマリ一覧 (@GET /api/documents/{id}/revisions@) を取得する。
listDocumentRevisions ::
       ApiClient
    -> DocumentId
    -> Maybe ListLimit
    -> Maybe ListOffset
    -> IO (Either ApiClientError RevisionListResponse)
listDocumentRevisions client docId mLimit mOffset =
    executeRequest client (buildListRevisionsRequest client.baseUrl docId mLimit mOffset) (parseJsonResponse 200)

-- | 特定リビジョンの再構成済み詳細 (@GET /api/documents/{id}/revisions/{revision}@) を取得する。
getDocumentRevision ::
       ApiClient
    -> DocumentId
    -> RevisionNumber
    -> IO (Either ApiClientError RevisionDetail)
getDocumentRevision client docId revNo =
    executeRequest client (buildGetRevisionRequest client.baseUrl docId revNo) (parseJsonResponse 200)

-- | 稼働中文書を過去リビジョンへ復元 (@POST /api/documents/{id}/revisions/{revision}/restore@) する。
restoreDocumentRevision ::
       ApiClient
    -> CsrfToken
    -> DocumentId
    -> RevisionNumber
    -> RestoreRevisionRequest
    -> IO (Either ApiClientError Document)
restoreDocumentRevision client csrf docId revNo req =
    executeRequest client (buildRestoreRevisionRequest client.baseUrl csrf docId revNo req) (parseJsonResponse 200)

executeRequest ::
       ApiClient
    -> HttpRequest
    -> (HttpResponse -> Either ApiClientError a)
    -> IO (Either ApiClientError a)
executeRequest client httpReq parser = do
    eRaw <- client.transport.sendRequest httpReq
    pure $ case eRaw of
        Left netErr -> Left (ClientNetworkError netErr)
        Right res   -> parser res

classifyErrorResponse :: HttpResponse -> ApiClientError
classifyErrorResponse res
    | res.statusCode >= 400 =
        case Aeson.eitherDecodeStrict' res.body of
            Right apiErr -> ClientApiError res.statusCode apiErr
            Left _       -> ClientHttpError res.statusCode (extractBodySnippet res.body)
    | otherwise =
        ClientHttpError res.statusCode (extractBodySnippet res.body)

extractBodySnippet :: ByteString -> Text
extractBodySnippet raw =
    let decoded :: Text
        decoded = Txt.strip (TxtEnc.decodeUtf8Lenient raw)
    in  if Txt.length decoded > 200
            then Txt.take 200 decoded <> "..."
            else decoded

endpointUrl :: ApiBaseUrl -> Text -> Text
endpointUrl base path = toText base <> path

documentEndpointUrl :: ApiBaseUrl -> DocumentId -> Text -> Text
documentEndpointUrl base docId suffix =
    endpointUrl base ("/api/documents/" <> toText docId <> suffix)

acceptJsonHeaders :: [(Text, Text)]
acceptJsonHeaders =
    [ ("Accept", "application/json")
    ]

jsonBodyHeaders :: [(Text, Text)]
jsonBodyHeaders =
    [ ("Accept",       "application/json")
    , ("Content-Type", "application/json; charset=utf-8")
    ]

withCsrfHeader :: CsrfToken -> [(Text, Text)] -> [(Text, Text)]
withCsrfHeader csrf baseHeaders =
    baseHeaders <> [(csrfHeaderName, toText csrf)]

encodeJsonBody :: ToJSON a => a -> ByteString
encodeJsonBody = LazyBS.toStrict . Aeson.encode
