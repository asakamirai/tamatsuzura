{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.App
    ( -- * WAI アプリケーション構築
      maxRawRequestBodyBytes
    , mkApplication
    , mkApplicationWithAuth
    , mkWarpApplication
    , mkWarpApplicationWithAuth
    , mkCgiApplication
    , configErrorApplication
    , normalizeCgiRequest
      -- re-export
    , ServerPort
    , mkServerPort
    , serverPortValue
    , defaultServerPort
    , parsePortArgs
    , WarpConfig(..)
    , CgiConfig(..)
    ) where

import           Control.Exception                (Exception, catch, evaluate, throwIO)
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.Aeson                       as Aeson
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BS8
import qualified Data.Char                        as Char
import           Data.IORef                       (IORef, atomicModifyIORef', newIORef)
import           Data.Text                        (Text)
import qualified Data.Text                        as Txt
import qualified Data.Text.IO                     as TxtIO
import qualified Network.HTTP.Types               as Http
import qualified Network.Wai                      as Wai
import           Servant
    ( Context(..)
    , Handler
    , Header
    , Headers
    , IsSecure(..)
    , NoContent(..)
    , Server
    , (:<|>)(..)
    )
import qualified Servant
import qualified System.IO                        as Sys
import           System.OsPath                    (OsPath)
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , CreateDocumentRequest
    , DocumentListResponse
    , FolderListResponse
    , HealthStatus
    , TagListResponse
    , UpdateDocumentRequest
    , healthOk
    , validateCreateDocumentRequest
    , validateUpdateDocumentRequest
    , validationErrorToApiError
    )
import           TamaTsuzura.Common.Auth
    ( AuthSessionResponse(..)
    , LoginRequest(..)
    , RegisterRequest(..)
    , UserId
    , UserProfile(..)
    , validateRegisterRequest
    )
import           TamaTsuzura.Common.Document      (Document, DocumentId)
import           TamaTsuzura.Common.Text          (ToText(..))
import           TamaTsuzura.Server.Api
    ( AuthApi
    , CsrfVerifiedSession(..)
    , DocumentByIdApi
    , DocumentsApi
    , RawCookieHeader(..)
    , RawDocumentListQuery(..)
    , SessionAuthContext(..)
    , TamaTsuzuraApi
    , apiErrorFormatters
    , apiErrorToServerError
    , jsonContentType
    , parseDocumentIdParam
    , parseDocumentListQuery
    , tamaTsuzuraApiProxy
    )
import           TamaTsuzura.Server.Auth.Cookie
    ( AuthConfig(..)
    , RegistrationPolicy(..)
    , buildClearSessionCookieHeader
    , buildSessionCookieHeader
    , defaultAuthConfig
    , extractSessionTokenFromCookieValue
    )
import           TamaTsuzura.Server.Auth.Crypto
    ( SessionTokenHash
    , hashPassword
    , hashPasswordWithSalt
    , hashSessionToken
    , mkSessionToken
    , verifyPassword
    )
import           TamaTsuzura.Server.Auth.Middleware
    ( csrfSessionAuthHandler
    , noStoreCacheMiddleware
    , normalizeCookieHeadersMiddleware
    , normalizeRequestSecurityMiddleware
    , originCheckMiddleware
    , sessionAuthHandler
    )
import           TamaTsuzura.Server.Config
    ( CgiConfig(..)
    , ServerPort
    , WarpConfig(..)
    , defaultServerPort
    , mkServerPort
    , parsePortArgs
    , serverPortValue
    )
import           TamaTsuzura.Server.Static        (staticFilesMiddleware)
import           TamaTsuzura.Server.Storage
    ( StorageConfig
    , StorageError(..)
    , StorageHandle
    , storageErrorToApiError
    )
import qualified TamaTsuzura.Server.Storage       as Storage
import           TamaTsuzura.Server.Storage.Auth
    ( AuthenticatedSession(..)
    , StoredSession(..)
    , StoredUser(..)
    , getUserByUsername
    , logoutSession
    , registerUserWithSession
    , rotateLoginSession
    )

-- | 生の HTTP リクエストボディに対するハード上限 (16 MiB)。
-- 文書本文の上限 (5 MiB UTF-8) に JSON エスケープやメタデータの余裕を持たせつつ、
-- 過大なリクエストによるメモリ枯渇 (OOM) を JSON パース前に遮断する。
maxRawRequestBodyBytes :: Int
maxRawRequestBodyBytes = 16 * 1024 * 1024

data RequestBodyTooLarge = RequestBodyTooLarge
    deriving (Show)

instance Exception RequestBodyTooLarge

-- | 既定の 'defaultAuthConfig' と指定した 'StorageConfig' を用いて
-- CGI と Warp の双方で共有される完全ステートレスな WAI / Servant アプリケーションを構築する。
mkApplication :: StorageConfig -> Wai.Application
mkApplication = mkApplicationWithAuth defaultAuthConfig

-- | 指定した 'AuthConfig' と 'StorageConfig' を用いて
-- CGI と Warp の双方で共有される完全ステートレスな WAI / Servant アプリケーションを構築する。
mkApplicationWithAuth :: AuthConfig -> StorageConfig -> Wai.Application
mkApplicationWithAuth authCfg storageCfg =
    noStoreCacheMiddleware
        . limitRequestBodyMiddleware maxRawRequestBodyBytes
        . normalizeErrorResponseMiddleware
        . normalizeCookieHeadersMiddleware
        . normalizeRequestSecurityMiddleware authCfg.trustProxyHeaders
        . originCheckMiddleware authCfg.trustProxyHeaders
        $ Servant.serveWithContext
            tamaTsuzuraApiProxy
            ( apiErrorFormatters
                :. sessionAuthHandler storageCfg
                :. csrfSessionAuthHandler storageCfg
                :. EmptyContext
            )
            (serverHandlers authCfg storageCfg)

-- | 既定の 'defaultAuthConfig' を用いた Warp 開発・スタンドアロン運用向け WAI アプリケーションを構築する。
-- @staticDir@ が指定された場合、@/api/...@ 以外へのリクエストを静的アセット配信へ振り分ける。
mkWarpApplication :: Maybe OsPath -> StorageConfig -> Wai.Application
mkWarpApplication mDir = mkWarpApplicationWithAuth mDir defaultAuthConfig

-- | 指定した 'AuthConfig' を用いた Warp 開発・スタンドアロン運用向け WAI アプリケーションを構築する。
mkWarpApplicationWithAuth :: Maybe OsPath -> AuthConfig -> StorageConfig -> Wai.Application
mkWarpApplicationWithAuth Nothing authCfg storageCfg =
    mkApplicationWithAuth authCfg storageCfg
mkWarpApplicationWithAuth (Just dir) authCfg storageCfg =
    staticFilesMiddleware dir (mkApplicationWithAuth authCfg storageCfg)

-- | CGI 環境 (@SCRIPT_NAME@ による @/api@ マウント差分および @HTTPS@ 環境変数の吸収を含む) 向けの
-- WAI アプリケーションを構築する。
mkCgiApplication :: CgiConfig -> Wai.Application
mkCgiApplication cgiCfg =
    cgiRequestMiddleware cgiCfg.scriptName cgiCfg.isHttps (mkApplicationWithAuth cgiCfg.auth cgiCfg.storage)

-- | CGI 起動時の設定エラー発生時に、クライアントへ内部詳細を漏らさず
-- 構造化 JSON の HTTP 500 レスポンスを返すフォールバックアプリケーション。
configErrorApplication :: Wai.Application
configErrorApplication _ respond =
    respond $ jsonErrorResponse Http.status500 ApiError
        { code    = ErrInternalServer
        , message = "Internal server error"
        , field   = Nothing
        }

-- | Apache の @ScriptAlias /api /path/to/tamatsuzura-cgi@ 等のように
-- @SCRIPT_NAME@ 側が @/api@ を消費し @PATH_INFO@ が @/documents@ となる配置でも
-- Servant ルーティング (@/api/...@) と一致するようリクエストパスを正規化する。
-- 一方で @SCRIPT_NAME=/cgi-bin/tamatsuzura.cgi, PATH_INFO=/api/documents@ のように
-- 既に @/api@ が @PATH_INFO@ に含まれる場合は一切変更しない。
normalizeCgiRequest :: Maybe Text -> Wai.Request -> Wai.Request
normalizeCgiRequest Nothing req = req
normalizeCgiRequest (Just scriptName) req
    | scriptEndsWithApi && not pathStartsWithApi =
        req
            { Wai.pathInfo    = "api" : Wai.pathInfo req
            , Wai.rawPathInfo = prependApiRawPath (Wai.rawPathInfo req)
            }
    | otherwise = req
  where
    trimmedScript :: Text
    trimmedScript = Txt.dropWhileEnd (== '/') scriptName

    scriptEndsWithApi :: Bool
    scriptEndsWithApi = trimmedScript == "/api" || Txt.isSuffixOf "/api" trimmedScript

    pathStartsWithApi :: Bool
    pathStartsWithApi = case Wai.pathInfo req of
        "api" : _ -> True
        _         -> False

    prependApiRawPath :: ByteString -> ByteString
    prependApiRawPath raw
        | BS.null raw             = "/api"
        | BS8.isPrefixOf "/" raw  = "/api" <> raw
        | otherwise               = "/api/" <> raw

cgiRequestMiddleware :: Maybe Text -> Bool -> Wai.Middleware
cgiRequestMiddleware mScriptName cgiHttps app req =
    let normalizedPath :: Wai.Request
        normalizedPath = normalizeCgiRequest mScriptName req

        normalizedReq :: Wai.Request
        normalizedReq = normalizedPath
            { Wai.isSecure = Wai.isSecure normalizedPath || cgiHttps
            }
    in  app normalizedReq

limitRequestBodyMiddleware :: Int -> Wai.Middleware
limitRequestBodyMiddleware maxBytes app req respond =
    case Wai.requestBodyLength req of
        Wai.KnownLength len
            | len > fromIntegral maxBytes ->
                respond payloadTooLargeResponse
        _ -> do
            bytesReadRef <- newIORef (0 :: Int)
            let guardedReq :: Wai.Request
                guardedReq = Wai.setRequestBodyChunks (readLimitedChunk bytesReadRef) req
            app guardedReq respond `catch` handleTooLarge
  where
    readLimitedChunk :: IORef Int -> IO ByteString
    readLimitedChunk bytesReadRef = do
        chunk <- Wai.getRequestBodyChunk req
        if BS.null chunk
            then pure chunk
            else do
                exceeded <- atomicModifyIORef' bytesReadRef $ \ acc ->
                    let next :: Int
                        next = acc + BS.length chunk
                    in  (next, next > maxBytes)
                if exceeded
                    then throwIO RequestBodyTooLarge
                    else pure chunk

    handleTooLarge :: RequestBodyTooLarge -> IO Wai.ResponseReceived
    handleTooLarge RequestBodyTooLarge = respond payloadTooLargeResponse

    payloadTooLargeResponse :: Wai.Response
    payloadTooLargeResponse = jsonErrorResponse Http.status413 ApiError
        { code    = ErrPayloadTooLarge
        , message = "Request body exceeds maximum allowed size (" <> Txt.pack (show maxBytes) <> " bytes)"
        , field   = Nothing
        }

-- | Servant が空ボディやプレーンテキストで返すフレームワーク標準エラー (405 / 415 等) を
-- OpenAPI 仕様に準拠した JSON 'ApiError' レスポンスへ統一するミドルウェア。
normalizeErrorResponseMiddleware :: Wai.Middleware
normalizeErrorResponseMiddleware app req respond =
    app req $ \ res ->
        let status :: Http.Status
            status = Wai.responseStatus res

            headers :: Http.ResponseHeaders
            headers = Wai.responseHeaders res
        in  if Http.statusCode status >= 400 && not (hasJsonContentType headers)
                then respond (buildFallbackErrorResponse status headers)
                else respond res
  where
    hasJsonContentType :: Http.ResponseHeaders -> Bool
    hasJsonContentType headers = case lookup Http.hContentType headers of
        Nothing -> False
        Just ct -> BS8.isPrefixOf "application/json" (BS8.map toLowerAscii ct)

    toLowerAscii :: Char -> Char
    toLowerAscii ch
        | Char.isAsciiUpper ch = Char.toLower ch
        | otherwise            = ch

    buildFallbackErrorResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
    buildFallbackErrorResponse status origHeaders =
        let apiErr :: ApiError
            apiErr = statusToFallbackApiError status

            preservedHeaders :: Http.ResponseHeaders
            preservedHeaders = filter (\ (name, _) -> name == "Allow") origHeaders
        in  Wai.responseLBS
                status
                ((Http.hContentType, jsonContentType) : preservedHeaders)
                (Aeson.encode apiErr)

    statusToFallbackApiError :: Http.Status -> ApiError
    statusToFallbackApiError status = case Http.statusCode status of
        401 -> ApiError { code = ErrUnauthorized,     message = "Authentication required",                             field = Nothing }
        403 -> ApiError { code = ErrForbidden,        message = "Forbidden",                                           field = Nothing }
        404 -> ApiError { code = ErrNotFound,         message = "Endpoint not found",                                  field = Nothing }
        405 -> ApiError { code = ErrMethodNotAllowed, message = "Method not allowed",                                  field = Nothing }
        406 -> ApiError { code = ErrValidation,       message = "Not acceptable (only application/json is supported)", field = Nothing }
        413 -> ApiError { code = ErrPayloadTooLarge,  message = "Request payload too large",                           field = Nothing }
        415 -> ApiError { code = ErrValidation,       message = "Unsupported media type (expected application/json)",  field = Nothing }
        code
            | code >= 500 -> ApiError { code = ErrInternalServer, message = "Internal server error", field = Nothing }
            | otherwise   -> ApiError { code = ErrValidation,     message = "Bad request",           field = Nothing }

jsonErrorResponse :: Http.Status -> ApiError -> Wai.Response
jsonErrorResponse status apiErr =
    Wai.responseLBS
        status
        [(Http.hContentType, jsonContentType)]
        (Aeson.encode apiErr)

serverHandlers :: AuthConfig -> StorageConfig -> Server TamaTsuzuraApi
serverHandlers authCfg storageCfg =
    handleHealth
        :<|> authHandlers authCfg storageCfg
        :<|> documentsHandlers storageCfg
        :<|> handleListTags storageCfg
        :<|> handleListFolders storageCfg

authHandlers :: AuthConfig -> StorageConfig -> Server AuthApi
authHandlers authCfg storageCfg =
    handleRegister authCfg storageCfg
        :<|> handleLogin authCfg storageCfg
        :<|> handleLogout authCfg storageCfg
        :<|> handleMe

documentsHandlers :: StorageConfig -> Server DocumentsApi
documentsHandlers storageCfg =
    handleListDocuments storageCfg
        :<|> handleCreateDocument storageCfg
        :<|> documentByIdHandlers storageCfg

documentByIdHandlers :: StorageConfig -> Text -> Server DocumentByIdApi
documentByIdHandlers storageCfg rawId =
    handleGetDocument storageCfg rawId
        :<|> handleUpdateDocument storageCfg rawId
        :<|> handleSoftDeleteDocument storageCfg rawId
        :<|> handleRestoreDocument storageCfg rawId
        :<|> handlePermanentDeleteDocument storageCfg rawId

handleHealth :: Handler HealthStatus
handleHealth = pure healthOk

handleRegister ::
       AuthConfig
    -> StorageConfig
    -> IsSecure
    -> Maybe RawCookieHeader
    -> RegisterRequest
    -> Handler (Headers '[Header "Set-Cookie" Text] AuthSessionResponse)
handleRegister authCfg storageCfg isSec mCookieHdr req = do
    case authCfg.registrationPolicy of
        RegistrationClosed ->
            Servant.throwError $ apiErrorToServerError ApiError
                { code    = ErrForbidden
                , message = "User registration is currently disabled"
                , field   = Nothing
                }
        RegistrationOpen -> pure ()

    validatedReq <-
        either
            (Servant.throwError . apiErrorToServerError . validationErrorToApiError)
            pure
            (validateRegisterRequest req)

    -- 重複ユーザー名の場合は高コストな Argon2id 計算を省略して即時 409 を返す
    -- (レースコンディション時は createUser の BEGIN IMMEDIATE が原子的重複検査を保証する)
    mExisting <- runStorage storageCfg (`getUserByUsername` validatedReq.username)
    case mExisting of
        Just _ ->
            Servant.throwError (apiErrorToServerError (storageErrorToApiError (UsernameAlreadyExists validatedReq.username)))
        Nothing -> pure ()

    -- Argon2id 計算中は SQLite 接続・トランザクションを一切保持しない
    ePwHash <- liftIO (hashPassword authCfg.passwordHashParams validatedReq.password)
    pwHash <- case ePwHash of
        Right h -> pure h
        Left cryptoErr -> do
            logStderrText ("Password hashing failed: " <> toText cryptoErr)
            Servant.throwError (apiErrorToServerError internalServerError)

    let mOldSessionHash :: Maybe SessionTokenHash
        mOldSessionHash = extractPreviousSessionHash mCookieHdr

    (storedUser, rawToken, storedSession) <- runStorage storageCfg $ \ handle ->
        registerUserWithSession
            handle
            validatedReq.username
            validatedReq.displayName
            pwHash
            authCfg.sessionTtl
            mOldSessionHash

    let cookieHeader :: Text
        cookieHeader = buildSessionCookieHeader authCfg.cookieSecureMode (isSec == Secure) authCfg.sessionTtl rawToken
    pure $ Servant.addHeader cookieHeader AuthSessionResponse
        { user      = storedUser.profile
        , csrfToken = storedSession.csrfToken
        }

handleLogin ::
       AuthConfig
    -> StorageConfig
    -> IsSecure
    -> Maybe RawCookieHeader
    -> LoginRequest
    -> Handler (Headers '[Header "Set-Cookie" Text] AuthSessionResponse)
handleLogin authCfg storageCfg isSec mCookieHdr req = do
    mStoredUser <- runStorage storageCfg (`getUserByUsername` req.username)
    storedUser <- case mStoredUser of
        Nothing -> do
            -- 未登録ユーザーでも同一パラメータの Argon2id 計算を実行して応答時間の差によるユーザー列挙を抑制する
            _ <- liftIO (evaluate (hashPasswordWithSalt authCfg.passwordHashParams dummyTimingSalt req.password))
            Servant.throwError (apiErrorToServerError invalidCredentialsError)
        Just userRec ->
            if verifyPassword req.password userRec.passwordHash
                then pure userRec
                else Servant.throwError (apiErrorToServerError invalidCredentialsError)

    let mOldSessionHash :: Maybe SessionTokenHash
        mOldSessionHash = extractPreviousSessionHash mCookieHdr

    (rawToken, storedSession) <- runStorage storageCfg $ \ handle ->
        rotateLoginSession handle storedUser.profile.id authCfg.sessionTtl mOldSessionHash

    let cookieHeader :: Text
        cookieHeader = buildSessionCookieHeader authCfg.cookieSecureMode (isSec == Secure) authCfg.sessionTtl rawToken
    pure $ Servant.addHeader cookieHeader AuthSessionResponse
        { user      = storedUser.profile
        , csrfToken = storedSession.csrfToken
        }

handleLogout ::
       AuthConfig
    -> StorageConfig
    -> CsrfVerifiedSession
    -> IsSecure
    -> Handler (Headers '[Header "Set-Cookie" Text] NoContent)
handleLogout authCfg storageCfg csrfSession isSec = do
    () <- runStorage storageCfg (`logoutSession` csrfSession.authenticated.session.tokenHash)
    let clearHeader :: Text
        clearHeader = buildClearSessionCookieHeader authCfg.cookieSecureMode (isSec == Secure)
    pure (Servant.addHeader clearHeader NoContent)

handleMe :: SessionAuthContext -> Handler AuthSessionResponse
handleMe sessionCtx =
    pure AuthSessionResponse
        { user      = sessionCtx.authenticated.user
        , csrfToken = sessionCtx.authenticated.session.csrfToken
        }

handleListDocuments ::
       StorageConfig
    -> SessionAuthContext
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Handler DocumentListResponse
handleListDocuments storageCfg sessionCtx mStatus mTag mFolder mIncludeSub mSearch mLimit mOffset = do
    let ownerId :: UserId
        ownerId = sessionCtx.authenticated.user.id

        rawQuery :: RawDocumentListQuery
        rawQuery = RawDocumentListQuery
            { rawStatus            = mStatus
            , rawTag               = mTag
            , rawFolder            = mFolder
            , rawIncludeSubfolders = mIncludeSub
            , rawSearch            = mSearch
            , rawLimit             = mLimit
            , rawOffset            = mOffset
            }
    query <- either (Servant.throwError . apiErrorToServerError) pure (parseDocumentListQuery rawQuery)
    runStorage storageCfg (\ handle -> Storage.listDocuments handle ownerId query)

handleCreateDocument ::
       StorageConfig
    -> CsrfVerifiedSession
    -> CreateDocumentRequest
    -> Handler Document
handleCreateDocument storageCfg csrfSession req = do
    let ownerId :: UserId
        ownerId = csrfSession.authenticated.user.id
    validatedReq <-
        either
            (Servant.throwError . apiErrorToServerError . validationErrorToApiError)
            pure
            (validateCreateDocumentRequest req)
    runStorage storageCfg (\ handle -> Storage.createDocument handle ownerId validatedReq)

handleGetDocument ::
       StorageConfig
    -> Text
    -> SessionAuthContext
    -> Handler Document
handleGetDocument storageCfg rawId sessionCtx = do
    let ownerId :: UserId
        ownerId = sessionCtx.authenticated.user.id
    docId <- parseIdOrThrow rawId
    runStorage storageCfg (\ handle -> Storage.getDocument handle ownerId docId)

handleUpdateDocument ::
       StorageConfig
    -> Text
    -> CsrfVerifiedSession
    -> UpdateDocumentRequest
    -> Handler Document
handleUpdateDocument storageCfg rawId csrfSession req = do
    let ownerId :: UserId
        ownerId = csrfSession.authenticated.user.id
    docId <- parseIdOrThrow rawId
    validatedReq <-
        either
            (Servant.throwError . apiErrorToServerError . validationErrorToApiError)
            pure
            (validateUpdateDocumentRequest req)
    runStorage storageCfg (\ handle -> Storage.updateDocument handle ownerId docId validatedReq)

handleSoftDeleteDocument ::
       StorageConfig
    -> Text
    -> CsrfVerifiedSession
    -> Handler Document
handleSoftDeleteDocument storageCfg rawId csrfSession = do
    let ownerId :: UserId
        ownerId = csrfSession.authenticated.user.id
    docId <- parseIdOrThrow rawId
    runStorage storageCfg (\ handle -> Storage.softDeleteDocument handle ownerId docId)

handleRestoreDocument ::
       StorageConfig
    -> Text
    -> CsrfVerifiedSession
    -> Handler Document
handleRestoreDocument storageCfg rawId csrfSession = do
    let ownerId :: UserId
        ownerId = csrfSession.authenticated.user.id
    docId <- parseIdOrThrow rawId
    runStorage storageCfg (\ handle -> Storage.restoreDocument handle ownerId docId)

handlePermanentDeleteDocument ::
       StorageConfig
    -> Text
    -> CsrfVerifiedSession
    -> Handler NoContent
handlePermanentDeleteDocument storageCfg rawId csrfSession = do
    let ownerId :: UserId
        ownerId = csrfSession.authenticated.user.id
    docId <- parseIdOrThrow rawId
    () <- runStorage storageCfg (\ handle -> Storage.permanentDeleteDocument handle ownerId docId)
    pure NoContent

handleListTags :: StorageConfig -> SessionAuthContext -> Handler TagListResponse
handleListTags storageCfg sessionCtx =
    runStorage storageCfg (`Storage.listTags` sessionCtx.authenticated.user.id)

handleListFolders :: StorageConfig -> SessionAuthContext -> Handler FolderListResponse
handleListFolders storageCfg sessionCtx =
    runStorage storageCfg (`Storage.listFolders` sessionCtx.authenticated.user.id)

extractPreviousSessionHash :: Maybe RawCookieHeader -> Maybe SessionTokenHash
extractPreviousSessionHash Nothing = Nothing
extractPreviousSessionHash (Just rawHdr) =
    case extractSessionTokenFromCookieValue rawHdr.unWrap of
        Nothing -> Nothing
        Just rawTokText -> case mkSessionToken rawTokText of
            Left _       -> Nothing
            Right oldTok -> Just (hashSessionToken oldTok)

dummyTimingSalt :: ByteString
dummyTimingSalt = "tamatsuzura-salt"

invalidCredentialsError :: ApiError
invalidCredentialsError = ApiError
    { code    = ErrUnauthorized
    , message = "Invalid username or password"
    , field   = Nothing
    }

internalServerError :: ApiError
internalServerError = ApiError
    { code    = ErrInternalServer
    , message = "Internal server error"
    , field   = Nothing
    }

parseIdOrThrow :: Text -> Handler DocumentId
parseIdOrThrow rawId =
    either (Servant.throwError . apiErrorToServerError) pure (parseDocumentIdParam rawId)

runStorage ::
       StorageConfig
    -> (StorageHandle -> IO (Either StorageError a))
    -> Handler a
runStorage storageCfg action = do
    eRes <- liftIO (Storage.withStorage storageCfg action)
    case eRes of
        Right val -> pure val
        Left err  -> do
            logIfInternalError err
            Servant.throwError (apiErrorToServerError (storageErrorToApiError err))
  where
    logIfInternalError :: StorageError -> Handler ()
    logIfInternalError storageErr = case storageErr of
        UnsupportedSchemaVersion{} -> logStderrText (toText storageErr)
        CorruptedData{}            -> logStderrText (toText storageErr)
        SqliteOperationError{}     -> logStderrText (toText storageErr)
        _                          -> pure ()

logStderrText :: Text -> Handler ()
logStderrText msg =
    -- CGI 環境では stdout が HTTP レスポンス本体となるため、診断ログは必ず stderr へ出力する
    liftIO (TxtIO.hPutStrLn Sys.stderr ("[tamatsuzura] " <> msg))
