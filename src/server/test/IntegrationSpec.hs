{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module IntegrationSpec
    ( integrationSpec
    ) where

import           Control.Exception          (bracket)
import qualified Data.Aeson                 as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as LazyBS
import           Data.IORef                 (modifyIORef', newIORef, readIORef)
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as Txt
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID4
import qualified Network.HTTP.Client        as HttpClient
import qualified Network.HTTP.Types         as Http
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.CGI    as CGI
import qualified Network.Wai.Handler.Warp   as Warp
import qualified System.Directory.OsPath    as DirOS
import           System.OsPath              (OsPath)
import qualified System.OsPath              as OS
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , DocumentListResponse(..)
    , FolderListResponse(..)
    , HealthStatus(..)
    , TagListResponse(..)
    , healthOk
    )
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentFormat(..)
    , DocumentSummary(..)
    )
import           TamaTsuzura.Common.Text    (ToText(..))
import           TamaTsuzura.Server.App
    ( configErrorApplication
    , mkApplication
    , mkCgiApplication
    )
import           TamaTsuzura.Server.Config
    ( envDbBusyTimeoutMs
    , envDbPath
    , resolveCgiConfig
    )
import           TamaTsuzura.Server.Storage
    ( SqlitePragmas(..)
    , defaultStorageConfig
    , inspectStoragePragmas
    , withStorage
    )
import           Test.Hspec                 (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import           Text.Read                  (readMaybe)

data CgiResponse = CgiResponse
    { statusCode :: !Int
    , headers    :: ![(Text, Text)]
    , body       :: !LazyBS.ByteString
    } deriving (Eq, Show)

integrationSpec :: Spec
integrationSpec = do
    warpIntegrationSpec
    cgiIntegrationSpec

warpIntegrationSpec :: Spec
warpIntegrationSpec = describe "Warp standalone server end-to-end integration" $ do
    it "serves full document CRUD, trash/restore/permanent-delete, and persists to WAL SQLite over TCP HTTP" $
        withTempDbPath "warp-e2e.sqlite3" $ \ dbPath -> do
            let storageCfg = defaultStorageConfig dbPath
            manager <- HttpClient.newManager HttpClient.defaultManagerSettings
            Warp.testWithApplication (pure (mkApplication storageCfg)) $ \ port -> do
                let baseUrl :: String
                    baseUrl = "http://127.0.0.1:" <> show port

                -- 1. Health check
                resHealth <- httpJson manager Http.methodGet (baseUrl <> "/api/health") Nothing
                Http.statusCode (HttpClient.responseStatus resHealth) `shouldBe` 200
                healthVal <- decodeJson (HttpClient.responseBody resHealth) :: IO HealthStatus
                healthVal `shouldBe` healthOk

                -- 2. Create document
                let createBody :: LazyBS.ByteString
                    createBody = Aeson.encode $ Aeson.object
                        [ "title"  Aeson..= ("Warp 疎通文書" :: Text)
                        , "body"   Aeson..= ("# Warp\nTCP 経由の永続化テスト" :: Text)
                        , "format" Aeson..= ("markdown" :: Text)
                        , "folder" Aeson..= ("warp/test" :: Text)
                        , "tags"   Aeson..= (["warp", "e2e"] :: [Text])
                        ]
                resCreate <- httpJson manager Http.methodPost (baseUrl <> "/api/documents") (Just createBody)
                Http.statusCode (HttpClient.responseStatus resCreate) `shouldBe` 201
                created <- decodeJson (HttpClient.responseBody resCreate) :: IO Document

                let docUrl :: String
                    docUrl = baseUrl <> "/api/documents/" <> Txt.unpack (toText created.id)

                -- 3. Get and Update document
                resGet <- httpJson manager Http.methodGet docUrl Nothing
                Http.statusCode (HttpClient.responseStatus resGet) `shouldBe` 200
                fetched <- decodeJson (HttpClient.responseBody resGet) :: IO Document
                fetched `shouldBe` created

                let updateBody :: LazyBS.ByteString
                    updateBody = Aeson.encode $ Aeson.object
                        [ "title"  Aeson..= ("Warp 更新済み" :: Text)
                        , "body"   Aeson..= ("更新後の本文" :: Text)
                        , "format" Aeson..= ("plain_text" :: Text)
                        , "folder" Aeson..= ("warp/updated" :: Text)
                        , "tags"   Aeson..= (["warp", "updated"] :: [Text])
                        ]
                resUpdate <- httpJson manager Http.methodPut docUrl (Just updateBody)
                Http.statusCode (HttpClient.responseStatus resUpdate) `shouldBe` 200
                updated <- decodeJson (HttpClient.responseBody resUpdate) :: IO Document
                toText updated.title `shouldBe` "Warp 更新済み"
                updated.format `shouldBe` FormatPlainText
                updated.createdAt `shouldBe` created.createdAt

                -- 4. List documents, tags, folders
                resList <- httpJson manager Http.methodGet (baseUrl <> "/api/documents?folder=warp&include_subfolders=true&tag=updated") Nothing
                Http.statusCode (HttpClient.responseStatus resList) `shouldBe` 200
                listed <- decodeJson (HttpClient.responseBody resList) :: IO DocumentListResponse
                listed.total `shouldBe` 1
                map (\ s -> s.id) listed.documents `shouldBe` [created.id]

                resTags <- httpJson manager Http.methodGet (baseUrl <> "/api/tags") Nothing
                Http.statusCode (HttpClient.responseStatus resTags) `shouldBe` 200
                tagList <- decodeJson (HttpClient.responseBody resTags) :: IO TagListResponse
                map toText tagList.tags `shouldBe` ["updated", "warp"]

                resFolders <- httpJson manager Http.methodGet (baseUrl <> "/api/folders") Nothing
                Http.statusCode (HttpClient.responseStatus resFolders) `shouldBe` 200
                folderList <- decodeJson (HttpClient.responseBody resFolders) :: IO FolderListResponse
                map toText folderList.folders `shouldBe` ["warp/updated"]

                -- 5. Soft delete, conflict on edit, restore, permanent delete (204)
                resTrash <- httpJson manager Http.methodDelete docUrl Nothing
                Http.statusCode (HttpClient.responseStatus resTrash) `shouldBe` 200
                trashed <- decodeJson (HttpClient.responseBody resTrash) :: IO Document
                trashed.deletedAt `shouldSatisfy` isJust

                resEditTrashed <- httpJson manager Http.methodPut docUrl (Just updateBody)
                Http.statusCode (HttpClient.responseStatus resEditTrashed) `shouldBe` 409
                editErr <- decodeJson (HttpClient.responseBody resEditTrashed) :: IO ApiError
                editErr.code `shouldBe` ErrConflict

                resRestore <- httpJson manager Http.methodPost (docUrl <> "/restore") Nothing
                Http.statusCode (HttpClient.responseStatus resRestore) `shouldBe` 200
                restored <- decodeJson (HttpClient.responseBody resRestore) :: IO Document
                restored.deletedAt `shouldBe` Nothing

                resPermActive <- httpJson manager Http.methodDelete (docUrl <> "/permanent") Nothing
                Http.statusCode (HttpClient.responseStatus resPermActive) `shouldBe` 409

                resTrashAgain <- httpJson manager Http.methodDelete docUrl Nothing
                Http.statusCode (HttpClient.responseStatus resTrashAgain) `shouldBe` 200

                resPerm <- httpJson manager Http.methodDelete (docUrl <> "/permanent") Nothing
                Http.statusCode (HttpClient.responseStatus resPerm) `shouldBe` 204
                HttpClient.responseBody resPerm `shouldBe` ""

                resGone <- httpJson manager Http.methodGet docUrl Nothing
                Http.statusCode (HttpClient.responseStatus resGone) `shouldBe` 404

            -- SQLite が WAL モードで永続化されていることを確認
            pragmas <- mustRight =<< withStorage storageCfg inspectStoragePragmas
            pragmas.journalMode `shouldBe` "wal"

cgiIntegrationSpec :: Spec
cgiIntegrationSpec = describe "CGI/1.1 handler end-to-end integration" $ do
    it "executes stateless multi-invocation CRUD and trash lifecycle via CGI/1.1 protocol and SQLite persistence" $
        withTempDbPath "cgi-e2e.sqlite3" $ \ dbPath -> do
            -- 環境変数経由の CGI 設定解決を再現するため境界で String へデコードする
            dbPathStr <- OS.decodeFS dbPath

            -- 1. Health check (SCRIPT_NAME=/cgi-bin/tamatsuzura.cgi, PATH_INFO=/api/health)
            resHealth <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "GET" "/api/health" "" Nothing
            resHealth.statusCode `shouldBe` 200
            healthVal <- decodeJson resHealth.body :: IO HealthStatus
            healthVal `shouldBe` healthOk

            -- 2. Create document via CGI (1st independent invocation)
            let createBody :: LazyBS.ByteString
                createBody = Aeson.encode $ Aeson.object
                    [ "title"  Aeson..= ("CGI 疎通メモ" :: Text)
                    , "body"   Aeson..= ("レンタルサーバ CGI 互換テスト" :: Text)
                    , "format" Aeson..= ("markdown" :: Text)
                    , "folder" Aeson..= ("cgi/notes" :: Text)
                    , "tags"   Aeson..= (["cgi", "sqlite"] :: [Text])
                    ]
            resCreate <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "POST" "/api/documents" "" (Just createBody)
            resCreate.statusCode `shouldBe` 201
            created <- decodeJson resCreate.body :: IO Document

            let idSegment :: String
                idSegment = Txt.unpack (toText created.id)

            -- 3. Read document in a separate CGI invocation using ScriptAlias /api mount style
            resGet <- invokeCgi dbPathStr "/api" "GET" ("/documents/" <> idSegment) "" Nothing
            resGet.statusCode `shouldBe` 200
            fetched <- decodeJson resGet.body :: IO Document
            fetched `shouldBe` created

            -- 4. Update document via CGI
            let updateBody :: LazyBS.ByteString
                updateBody = Aeson.encode $ Aeson.object
                    [ "title"  Aeson..= ("CGI 更新済み" :: Text)
                    , "body"   Aeson..= ("CGI 経由で更新" :: Text)
                    , "format" Aeson..= ("plain_text" :: Text)
                    , "folder" Aeson..= ("cgi/archive" :: Text)
                    , "tags"   Aeson..= (["cgi"] :: [Text])
                    ]
            resUpdate <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "PUT" ("/api/documents/" <> idSegment) "" (Just updateBody)
            resUpdate.statusCode `shouldBe` 200
            updated <- decodeJson resUpdate.body :: IO Document
            toText updated.title `shouldBe` "CGI 更新済み"

            -- 5. List with query string via CGI
            resList <- invokeCgi dbPathStr "/api" "GET" "/documents" "tag=cgi&folder=cgi&include_subfolders=true&q=CGI" Nothing
            resList.statusCode `shouldBe` 200
            listed <- decodeJson resList.body :: IO DocumentListResponse
            listed.total `shouldBe` 1
            map (\ s -> s.id) listed.documents `shouldBe` [created.id]

            -- 6. Soft delete -> conflict on PUT -> restore -> soft delete -> permanent delete (204)
            resTrash <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "DELETE" ("/api/documents/" <> idSegment) "" Nothing
            resTrash.statusCode `shouldBe` 200
            trashed <- decodeJson resTrash.body :: IO Document
            trashed.deletedAt `shouldSatisfy` isJust

            resConflict <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "PUT" ("/api/documents/" <> idSegment) "" (Just updateBody)
            resConflict.statusCode `shouldBe` 409
            conflictErr <- decodeJson resConflict.body :: IO ApiError
            conflictErr.code `shouldBe` ErrConflict

            resRestore <- invokeCgi dbPathStr "/api" "POST" ("/documents/" <> idSegment <> "/restore") "" Nothing
            resRestore.statusCode `shouldBe` 200
            restored <- decodeJson resRestore.body :: IO Document
            restored.deletedAt `shouldBe` Nothing

            resTrash2 <- invokeCgi dbPathStr "/api" "DELETE" ("/documents/" <> idSegment) "" Nothing
            resTrash2.statusCode `shouldBe` 200

            resPerm <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "DELETE" ("/api/documents/" <> idSegment <> "/permanent") "" Nothing
            resPerm.statusCode `shouldBe` 204
            resPerm.body `shouldBe` ""

            -- 7. After permanent delete -> 404, and invalid method -> 405
            resGone <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "GET" ("/api/documents/" <> idSegment) "" Nothing
            resGone.statusCode `shouldBe` 404
            goneErr <- decodeJson resGone.body :: IO ApiError
            goneErr.code `shouldBe` ErrNotFound

            res405 <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "DELETE" "/api/documents" "" Nothing
            res405.statusCode `shouldBe` 405
            err405 <- decodeJson res405.body :: IO ApiError
            err405.code `shouldBe` ErrMethodNotAllowed

            -- SQLite が WAL モードで永続化されていることを確認
            pragmas <- mustRight =<< withStorage (defaultStorageConfig dbPath) inspectStoragePragmas
            pragmas.journalMode `shouldBe` "wal"

    it "returns structured 500 JSON response from configErrorApplication on CGI configuration failure" $ do
        let envVars :: [(String, String)]
            envVars =
                [ ("GATEWAY_INTERFACE", "CGI/1.1")
                , ("REQUEST_METHOD", "GET")
                , ("SCRIPT_NAME", "/cgi-bin/tamatsuzura.cgi")
                , ("PATH_INFO", "/api/documents")
                , ("SERVER_PROTOCOL", "HTTP/1.1")
                , ("SERVER_NAME", "localhost")
                , ("SERVER_PORT", "80")
                , ("REMOTE_ADDR", "127.0.0.1")
                , (envDbBusyTimeoutMs, "invalid-timeout")
                ]
        eCfg <- resolveCgiConfig envVars
        case eCfg of
            Right _ -> expectationFailure "Expected invalid TAMATSUZURA_DB_BUSY_TIMEOUT_MS to fail"
            Left _  -> do
                res <- runCgiApp envVars Nothing configErrorApplication
                res.statusCode `shouldBe` 500
                apiErr <- decodeJson res.body :: IO ApiError
                apiErr.code `shouldBe` ErrInternalServer
                apiErr.message `shouldBe` "Internal server error"

httpJson ::
       HttpClient.Manager
    -> Http.Method
    -> String
    -> Maybe LazyBS.ByteString
    -> IO (HttpClient.Response LazyBS.ByteString)
httpJson manager method url mBody = do
    initReq <- HttpClient.parseRequest url
    let req :: HttpClient.Request
        req = case mBody of
            Nothing -> initReq
                { HttpClient.method         = method
                , HttpClient.requestHeaders = [(Http.hAccept, "application/json")]
                }
            Just bodyBytes -> initReq
                { HttpClient.method         = method
                , HttpClient.requestHeaders =
                    [ (Http.hContentType, "application/json")
                    , (Http.hAccept, "application/json")
                    ]
                , HttpClient.requestBody    = HttpClient.RequestBodyLBS bodyBytes
                }
    HttpClient.httpLbs req manager

invokeCgi ::
       String
    -> String
    -> String
    -> String
    -> String
    -> Maybe LazyBS.ByteString
    -> IO CgiResponse
invokeCgi dbPathStr scriptName method pathInfo queryString mBody = do
    let strictBody :: ByteString
        strictBody = maybe BS.empty LazyBS.toStrict mBody

        baseEnv :: [(String, String)]
        baseEnv =
            [ ("GATEWAY_INTERFACE", "CGI/1.1")
            , ("REQUEST_METHOD", method)
            , ("SCRIPT_NAME", scriptName)
            , ("PATH_INFO", pathInfo)
            , ("QUERY_STRING", queryString)
            , ("SERVER_PROTOCOL", "HTTP/1.1")
            , ("SERVER_NAME", "localhost")
            , ("SERVER_PORT", "80")
            , ("REMOTE_ADDR", "127.0.0.1")
            , ("HTTP_ACCEPT", "application/json")
            , (envDbPath, dbPathStr)
            ]

        bodyEnv :: [(String, String)]
        bodyEnv = case mBody of
            Nothing -> []
            Just _  ->
                [ ("CONTENT_TYPE", "application/json")
                , ("CONTENT_LENGTH", show (BS.length strictBody))
                ]

        envVars :: [(String, String)]
        envVars = baseEnv <> bodyEnv

    cgiCfg <- mustRight =<< resolveCgiConfig envVars
    runCgiApp envVars mBody (mkCgiApplication cgiCfg)

runCgiApp ::
       [(String, String)]
    -> Maybe LazyBS.ByteString
    -> Wai.Application
    -> IO CgiResponse
runCgiApp envVars mBody app = do
    let strictBody :: ByteString
        strictBody = maybe BS.empty LazyBS.toStrict mBody
    remainingRef <- newIORef strictBody
    outChunksRef <- newIORef ([] :: [ByteString])
    let readChunk :: Int -> IO (Maybe ByteString)
        readChunk maxLen = do
            remBytes <- readIORef remainingRef
            if BS.null remBytes
                then pure Nothing
                else do
                    let (chunk, rest) = BS.splitAt maxLen remBytes
                    modifyIORef' remainingRef (const rest)
                    pure (Just chunk)

        writeChunk :: ByteString -> IO ()
        writeChunk chunk = modifyIORef' outChunksRef (chunk :)
    CGI.runGeneric envVars (CGI.requestBodyFunc readChunk) writeChunk Nothing app
    rawOutput <- LazyBS.fromChunks . reverse <$> readIORef outChunksRef
    parseCgiWireResponse (LazyBS.toStrict rawOutput)

parseCgiWireResponse :: ByteString -> IO CgiResponse
parseCgiWireResponse raw =
    case splitHeaderBody raw of
        Nothing -> do
            expectationFailure ("Missing header/body separator in CGI output: " <> show raw)
            fail "Unreachable after expectationFailure"
        Just (headerBlock, bodyStrict) -> do
            let bodyBytes :: LazyBS.ByteString
                bodyBytes = LazyBS.fromStrict bodyStrict

                headerLines :: [ByteString]
                headerLines = filter (not . BS.null) (map stripTrailingCr (BS8.lines headerBlock))

                parsedHeaders :: [(Text, Text)]
                parsedHeaders = map parseHeaderLine headerLines

                statusCode :: Int
                statusCode = extractStatusCode parsedHeaders
            pure CgiResponse
                { statusCode = statusCode
                , headers    = parsedHeaders
                , body       = bodyBytes
                }
  where
    splitHeaderBody :: ByteString -> Maybe (ByteString, ByteString)
    splitHeaderBody bs =
        let (hCrlf, rCrlf) = BS.breakSubstring "\r\n\r\n" bs
            (hLf, rLf)     = BS.breakSubstring "\n\n" bs
        in  case (BS.null rCrlf, BS.null rLf) of
                (True,  True)  -> Nothing
                (False, True)  -> Just (hCrlf, BS.drop 4 rCrlf)
                (True,  False) -> Just (hLf, BS.drop 2 rLf)
                (False, False)
                    | BS.length hCrlf <= BS.length hLf -> Just (hCrlf, BS.drop 4 rCrlf)
                    | otherwise                        -> Just (hLf, BS.drop 2 rLf)

    stripTrailingCr :: ByteString -> ByteString
    stripTrailingCr line = case BS8.unsnoc line of
        Just (initBs, '\r') -> initBs
        _                   -> line

    parseHeaderLine :: ByteString -> (Text, Text)
    parseHeaderLine line =
        let (k, v) = BS8.break (== ':') line
            keyText :: Text
            keyText = Txt.toLower (Txt.strip (Txt.pack (BS8.unpack k)))

            valText :: Text
            valText = Txt.strip (Txt.pack (BS8.unpack (BS.drop 1 v)))
        in  (keyText, valText)

    extractStatusCode :: [(Text, Text)] -> Int
    extractStatusCode hdrs = case lookup "status" hdrs of
        Nothing -> 200
        Just rawStatus -> case Txt.words rawStatus of
            codeTxt : _ -> fromMaybe 0 (readMaybe (Txt.unpack codeTxt))
            []          -> 0

decodeJson :: Aeson.FromJSON a => LazyBS.ByteString -> IO a
decodeJson bs = case Aeson.eitherDecode bs of
    Left err  -> do
        expectationFailure ("Failed to decode JSON: " <> err <> " (body=" <> show bs <> ")")
        fail "Unreachable after expectationFailure"
    Right val -> pure val

withTempDbPath :: String -> (OsPath -> IO a) -> IO a
withTempDbPath relFileName action = do
    tmpRoot <- DirOS.getTemporaryDirectory
    rawUuid <- UUID4.nextRandom
    -- UUID 文字列およびテスト用ファイル名から OsPath を生成するため encodeFS を使用する
    dirPart <- OS.encodeFS ("tamatsuzura-integration-test-" <> UUID.toString rawUuid)
    filePart <- OS.encodeFS relFileName
    let testDir :: OsPath
        testDir = tmpRoot OS.</> dirPart

        dbPath :: OsPath
        dbPath = testDir OS.</> filePart
    bracket
        (DirOS.createDirectoryIfMissing True testDir >> pure dbPath)
        (const (DirOS.removePathForcibly testDir))
        action

mustRight :: Show e => Either e a -> IO a
mustRight (Right val) = pure val
mustRight (Left err)  = do
    expectationFailure ("Expected Right, got Left: " <> show err)
    fail "Unreachable after expectationFailure"
