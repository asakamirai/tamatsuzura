{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.App
    ( -- * WAI アプリケーション構築
      maxRawRequestBodyBytes
    , mkApplication
    , mkWarpApplication
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

import           Control.Exception                (Exception, catch, throwIO)
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
import           TamaTsuzura.Common.Document      (Document, DocumentId)
import           TamaTsuzura.Common.Text          (ToText(..))
import           TamaTsuzura.Server.Api
    ( DocumentByIdApi
    , DocumentsApi
    , RawDocumentListQuery(..)
    , TamaTsuzuraApi
    , apiErrorFormatters
    , apiErrorToServerError
    , jsonContentType
    , parseDocumentIdParam
    , parseDocumentListQuery
    , tamaTsuzuraApiProxy
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

-- | 生の HTTP リクエストボディに対するハード上限 (16 MiB)。
-- 文書本文の上限 (5 MiB UTF-8) に JSON エスケープやメタデータの余裕を持たせつつ、
-- 過大なリクエストによるメモリ枯渇 (OOM) を JSON パース前に遮断する。
maxRawRequestBodyBytes :: Int
maxRawRequestBodyBytes = 16 * 1024 * 1024

data RequestBodyTooLarge = RequestBodyTooLarge
    deriving (Show)

instance Exception RequestBodyTooLarge

-- | 指定した 'StorageConfig' を用いて CGI と Warp の双方で共有される
-- 完全ステートレスな WAI / Servant アプリケーションを構築する。
mkApplication :: StorageConfig -> Wai.Application
mkApplication storageCfg =
    limitRequestBodyMiddleware maxRawRequestBodyBytes
        . normalizeErrorResponseMiddleware
        $ Servant.serveWithContext
            tamaTsuzuraApiProxy
            (apiErrorFormatters :. EmptyContext)
            (serverHandlers storageCfg)

-- | Warp 開発・スタンドアロン運用向けの WAI アプリケーションを構築する。
-- @staticDir@ が指定された場合、@/api/...@ 以外へのリクエストを静的アセット配信へ振り分ける。
mkWarpApplication :: Maybe OsPath -> StorageConfig -> Wai.Application
mkWarpApplication Nothing storageCfg =
    mkApplication storageCfg
mkWarpApplication (Just dir) storageCfg =
    staticFilesMiddleware dir (mkApplication storageCfg)

-- | CGI 環境 (@SCRIPT_NAME@ による @/api@ マウント差分の吸収を含む) 向けの
-- WAI アプリケーションを構築する。
mkCgiApplication :: CgiConfig -> Wai.Application
mkCgiApplication cgiCfg =
    cgiScriptNameMiddleware cgiCfg.scriptName (mkApplication cgiCfg.storage)

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

cgiScriptNameMiddleware :: Maybe Text -> Wai.Middleware
cgiScriptNameMiddleware mScriptName app req =
    app (normalizeCgiRequest mScriptName req)

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

serverHandlers :: StorageConfig -> Server TamaTsuzuraApi
serverHandlers storageCfg =
    handleHealth
        :<|> documentsHandlers storageCfg
        :<|> handleListTags storageCfg
        :<|> handleListFolders storageCfg

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

handleListDocuments ::
       StorageConfig
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Handler DocumentListResponse
handleListDocuments storageCfg mStatus mTag mFolder mIncludeSub mSearch mLimit mOffset = do
    let rawQuery :: RawDocumentListQuery
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
    runStorage storageCfg (`Storage.listDocuments` query)

handleCreateDocument :: StorageConfig -> CreateDocumentRequest -> Handler Document
handleCreateDocument storageCfg req = do
    validatedReq <-
        either
            (Servant.throwError . apiErrorToServerError . validationErrorToApiError)
            pure
            (validateCreateDocumentRequest req)
    runStorage storageCfg (`Storage.createDocument` validatedReq)

handleGetDocument :: StorageConfig -> Text -> Handler Document
handleGetDocument storageCfg rawId = do
    docId <- parseIdOrThrow rawId
    runStorage storageCfg (`Storage.getDocument` docId)

handleUpdateDocument :: StorageConfig -> Text -> UpdateDocumentRequest -> Handler Document
handleUpdateDocument storageCfg rawId req = do
    docId <- parseIdOrThrow rawId
    validatedReq <-
        either
            (Servant.throwError . apiErrorToServerError . validationErrorToApiError)
            pure
            (validateUpdateDocumentRequest req)
    runStorage storageCfg (\ handle -> Storage.updateDocument handle docId validatedReq)

handleSoftDeleteDocument :: StorageConfig -> Text -> Handler Document
handleSoftDeleteDocument storageCfg rawId = do
    docId <- parseIdOrThrow rawId
    runStorage storageCfg (`Storage.softDeleteDocument` docId)

handleRestoreDocument :: StorageConfig -> Text -> Handler Document
handleRestoreDocument storageCfg rawId = do
    docId <- parseIdOrThrow rawId
    runStorage storageCfg (`Storage.restoreDocument` docId)

handlePermanentDeleteDocument :: StorageConfig -> Text -> Handler NoContent
handlePermanentDeleteDocument storageCfg rawId = do
    docId <- parseIdOrThrow rawId
    () <- runStorage storageCfg (`Storage.permanentDeleteDocument` docId)
    pure NoContent

handleListTags :: StorageConfig -> Handler TagListResponse
handleListTags storageCfg = runStorage storageCfg Storage.listTags

handleListFolders :: StorageConfig -> Handler FolderListResponse
handleListFolders storageCfg = runStorage storageCfg Storage.listFolders

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
        UnsupportedSchemaVersion{} -> logStderr storageErr
        CorruptedData{}            -> logStderr storageErr
        SqliteOperationError{}     -> logStderr storageErr
        _                          -> pure ()

    logStderr :: StorageError -> Handler ()
    logStderr storageErr =
        -- CGI 環境では stdout が HTTP レスポンス本体となるため、診断ログは必ず stderr へ出力する
        liftIO (TxtIO.hPutStrLn Sys.stderr ("[tamatsuzura] " <> toText storageErr))
