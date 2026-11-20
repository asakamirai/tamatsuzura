{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module ApiSpec
    ( apiSpec
    ) where

import           Control.Exception                  (bracket)
import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Aeson                         as Aeson
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Builder            as Builder
import qualified Data.ByteString.Char8              as BS8
import qualified Data.ByteString.Lazy               as LazyBS
import           Data.Either                        (isLeft)
import           Data.IORef                         (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import           Data.Maybe                         (fromMaybe, isJust)
import           Data.Text                          (Text)
import qualified Data.Text                          as Txt
import qualified Data.Text.Encoding                 as TxtEnc
import qualified Data.UUID                          as UUID
import qualified Data.UUID.V4                       as UUID4
import qualified Database.SQLite.Simple             as SQLite
import qualified Network.HTTP.Types                 as Http
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Internal               as WaiInternal
import qualified Network.Wai.Test                   as WaiTest
import qualified System.Directory.OsPath            as DirOS
import           System.OsPath                      (OsPath)
import qualified System.OsPath                      as OS
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , DocumentListQuery(..)
    , DocumentListResponse(..)
    , DocumentStatusFilter(..)
    , FolderListResponse(..)
    , HealthStatus(..)
    , TagListResponse(..)
    , defaultDocumentListQuery
    , healthOk
    , listLimitValue
    , listOffsetValue
    )
import           TamaTsuzura.Common.Auth            (AuthSessionResponse(..))
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentFormat(..)
    , DocumentSummary(..)
    , emptyDocumentBody
    , emptyDocumentTitle
    , emptyTagSet
    )
import           TamaTsuzura.Common.Text            (ToText(..))
import           TamaTsuzura.Common.Validation      (maxDocumentBodyBytes)
import           TamaTsuzura.Server.Api
    ( RawDocumentListQuery(..)
    , emptyRawDocumentListQuery
    , parseDocumentIdParam
    , parseDocumentListQuery
    )
import           TamaTsuzura.Server.App
    ( maxRawRequestBodyBytes
    , mkApplication
    , mkApplicationWithAuth
    , normalizeCgiRequest
    )
import           TamaTsuzura.Server.Auth.Cookie
    ( AuthConfig(..)
    , CookieSecureMode(..)
    , RegistrationPolicy(..)
    , defaultAuthConfig
    )
import           TamaTsuzura.Server.Auth.Crypto     (fastTestPasswordHashParams)
import           TamaTsuzura.Server.Auth.Middleware (csrfHeaderName)
import           TamaTsuzura.Server.Config
    ( CgiConfig(..)
    , RawWarpOptions(..)
    , WarpConfig(..)
    , defaultServerPort
    , envAllowRegistration
    , envCookieSecure
    , envDbBusyTimeoutMs
    , envDbPath
    , envHttps
    , envPort
    , envRequestScheme
    , envScriptName
    , envSessionTtlSeconds
    , envTrustProxyHeaders
    , mkServerPort
    , parsePortArgs
    , parseWarpArgs
    , resolveAuthConfig
    , resolveCgiConfig
    , resolveWarpConfig
    , serverPortValue
    )
import           TamaTsuzura.Server.Storage
    ( StorageConfig(..)
    , StorageTarget(..)
    , busyTimeoutMsValue
    , defaultStorageConfig
    )
import           TamaTsuzura.Server.Storage.Auth    (sessionTtlSecondsValue)
import           Test.Hspec                         (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

data SessionHeaders = SessionHeaders
    { cookie :: !ByteString
    , csrf   :: !ByteString
    } deriving (Eq, Show)

apiSpec :: Spec
apiSpec = do
    configSpec
    queryParserSpec
    restEndpointSpec
    errorHandlingSpec

configSpec :: Spec
configSpec = describe "Server configuration and CGI path normalization" $ do
    it "parses legacy parsePortArgs boundaries and rejects invalid values" $ do
        parsePortArgs [] `shouldBe` Right defaultServerPort
        fmap serverPortValue (parsePortArgs ["--port", "1"]) `shouldBe` Right 1
        fmap serverPortValue (parsePortArgs ["--port", "3000"]) `shouldBe` Right 3000
        fmap serverPortValue (parsePortArgs ["--port", "65535"]) `shouldBe` Right 65535
        parsePortArgs ["--port", "0"] `shouldBe` Left "Invalid port number: 0"
        parsePortArgs ["--port", "65536"] `shouldBe` Left "Invalid port number: 65536"
        parsePortArgs ["--port", "999999999999999999999"] `shouldBe` Left "Invalid port number: 999999999999999999999"
        mkServerPort (-1) `shouldBe` Left "Invalid port number: -1"
        parsePortArgs ["--port", "abc"] `shouldBe` Left "Invalid port number: abc"
        parsePortArgs ["--port"] `shouldBe` Left "Usage: tamatsuzura-server [--port PORT]"
        parsePortArgs ["--foo"] `shouldBe` Left "Usage: tamatsuzura-server [--port PORT]"

    it "parses extended Warp CLI flags and rejects duplicates or malformed flags" $ do
        parseWarpArgs ["--port", "9090", "--db-path", "tmp/app.sqlite3", "--busy-timeout-ms", "2500"]
            `shouldBe` Right RawWarpOptions
                { rawPort          = Just "9090"
                , rawDbPath        = Just "tmp/app.sqlite3"
                , rawBusyTimeoutMs = Just "2500"
                , rawStaticDir     = Nothing
                }
        parseWarpArgs ["--port", "8080", "--port", "9090"] `shouldSatisfy` isLeft
        parseWarpArgs ["--db-path"] `shouldSatisfy` isLeft
        parseWarpArgs ["--unknown", "1"] `shouldSatisfy` isLeft

    it "resolves AuthConfig environment variables and rejects invalid values" $ do
        defaultResolved <- mustRight (resolveAuthConfig [])
        defaultResolved `shouldBe` defaultAuthConfig

        customResolved <- mustRight $ resolveAuthConfig
            [ (envAllowRegistration, "false")
            , (envSessionTtlSeconds, "3600")
            , (envCookieSecure, "always")
            , (envTrustProxyHeaders, "true")
            ]
        customResolved.registrationPolicy `shouldBe` RegistrationClosed
        sessionTtlSecondsValue customResolved.sessionTtl `shouldBe` 3600
        customResolved.cookieSecureMode `shouldBe` CookieSecureAlways
        customResolved.trustProxyHeaders `shouldBe` True

        neverResolved <- mustRight $ resolveAuthConfig [(envCookieSecure, "never")]
        neverResolved.cookieSecureMode `shouldBe` CookieSecureNever

        resolveAuthConfig [(envAllowRegistration, "maybe")] `shouldSatisfy` isLeft
        resolveAuthConfig [(envSessionTtlSeconds, "59")] `shouldSatisfy` isLeft
        resolveAuthConfig [(envSessionTtlSeconds, "abc")] `shouldSatisfy` isLeft
        resolveAuthConfig [(envCookieSecure, "strict")] `shouldSatisfy` isLeft
        resolveAuthConfig [(envTrustProxyHeaders, "yes")] `shouldSatisfy` isLeft

    it "resolves WarpConfig with CLI > env > defaults precedence" $ do
        eDefault <- resolveWarpConfig [] []
        cfgDefault <- mustRight eDefault
        serverPortValue cfgDefault.port `shouldBe` 8080
        busyTimeoutMsValue cfgDefault.storage.busyTimeoutMs `shouldBe` 5000
        cfgDefault.auth `shouldBe` defaultAuthConfig

        let envVars :: [(String, String)]
            envVars =
                [ (envPort, "4000")
                , (envDbPath, "/tmp/env.sqlite3")
                , (envDbBusyTimeoutMs, "1200")
                , (envAllowRegistration, "false")
                ]
        cfgFromEnv <- mustRight =<< resolveWarpConfig [] envVars
        serverPortValue cfgFromEnv.port `shouldBe` 4000
        busyTimeoutMsValue cfgFromEnv.storage.busyTimeoutMs `shouldBe` 1200
        cfgFromEnv.auth.registrationPolicy `shouldBe` RegistrationClosed

        cfgCliOverride <- mustRight =<< resolveWarpConfig ["--port", "5000", "--busy-timeout-ms", "0"] envVars
        serverPortValue cfgCliOverride.port `shouldBe` 5000
        busyTimeoutMsValue cfgCliOverride.storage.busyTimeoutMs `shouldBe` 0

        eEmptyPath <- resolveWarpConfig ["--db-path", "   "] []
        eEmptyPath `shouldSatisfy` isLeft
        eBadTimeout <- resolveWarpConfig [] [(envDbBusyTimeoutMs, "99999999999999999999")]
        eBadTimeout `shouldSatisfy` isLeft

    it "resolves CgiConfig from CGI environment variables including HTTPS detection" $ do
        cfg <- mustRight =<< resolveCgiConfig
            [ (envDbPath, "/tmp/cgi.sqlite3")
            , (envDbBusyTimeoutMs, "3500")
            , (envScriptName, "/api")
            , (envHttps, "on")
            , (envSessionTtlSeconds, "7200")
            ]
        cfg.scriptName `shouldBe` Just "/api"
        cfg.isHttps `shouldBe` True
        busyTimeoutMsValue cfg.storage.busyTimeoutMs `shouldBe` 3500
        sessionTtlSecondsValue cfg.auth.sessionTtl `shouldBe` 7200
        -- テスト用にエンコードして一致を確認する
        expectedPath <- OS.encodeFS "/tmp/cgi.sqlite3"
        cfg.storage.target `shouldBe` StorageFile expectedPath

        cfgScheme <- mustRight =<< resolveCgiConfig [(envRequestScheme, "https")]
        cfgScheme.isHttps `shouldBe` True

        cfgHttp <- mustRight =<< resolveCgiConfig []
        cfgHttp.isHttps `shouldBe` False

    it "normalizes CGI requests when SCRIPT_NAME consumes /api without double-prefixing" $ do
        let reqHealth :: Wai.Request
            reqHealth = WaiTest.setPath WaiTest.defaultRequest "/health"

            reqApiHealth :: Wai.Request
            reqApiHealth = WaiTest.setPath WaiTest.defaultRequest "/api/health"

            reqDocs :: Wai.Request
            reqDocs = WaiTest.setPath WaiTest.defaultRequest "/documents/doc-1"

            reqEmpty :: Wai.Request
            reqEmpty = WaiTest.defaultRequest
                { Wai.pathInfo    = []
                , Wai.rawPathInfo = ""
                }

        Wai.pathInfo (normalizeCgiRequest (Just "/api") reqHealth) `shouldBe` ["api", "health"]
        Wai.rawPathInfo (normalizeCgiRequest (Just "/api") reqHealth) `shouldBe` "/api/health"
        Wai.pathInfo (normalizeCgiRequest (Just "/app/api/") reqDocs) `shouldBe` ["api", "documents", "doc-1"]
        Wai.pathInfo (normalizeCgiRequest (Just "/api") reqEmpty) `shouldBe` ["api"]
        Wai.rawPathInfo (normalizeCgiRequest (Just "/api") reqEmpty) `shouldBe` "/api"
        Wai.pathInfo (normalizeCgiRequest (Just "/api") reqApiHealth) `shouldBe` ["api", "health"]
        Wai.pathInfo (normalizeCgiRequest (Just "/cgi-bin/tamatsuzura.cgi") reqHealth) `shouldBe` ["health"]
        Wai.pathInfo (normalizeCgiRequest Nothing reqHealth) `shouldBe` ["health"]

queryParserSpec :: Spec
queryParserSpec = describe "Query and path parameter validation" $ do
    it "returns defaultDocumentListQuery when all raw parameters are absent" $
        parseDocumentListQuery emptyRawDocumentListQuery `shouldBe` Right defaultDocumentListQuery

    it "parses valid query combinations and treats whitespace-only q as Nothing" $ do
        parsed <- mustRight $ parseDocumentListQuery RawDocumentListQuery
            { rawStatus            = Just "all"
            , rawTag               = Just "  仕事  "
            , rawFolder            = Just "/work/notes/"
            , rawIncludeSubfolders = Just "true"
            , rawSearch            = Just "   "
            , rawLimit             = Just "25"
            , rawOffset            = Just "10"
            }
        parsed.status `shouldBe` StatusAll
        fmap toText parsed.tag `shouldBe` Just "仕事"
        fmap toText parsed.folder `shouldBe` Just "work/notes"
        parsed.includeSubfolders `shouldBe` True
        parsed.search `shouldBe` Nothing
        listLimitValue parsed.limit `shouldBe` 25
        listOffsetValue parsed.offset `shouldBe` 10

    it "reports exact API field names on invalid query and path parameters" $ do
        assertFieldError "status" (parseDocumentListQuery emptyRawDocumentListQuery { rawStatus = Just "deleted" })
        assertFieldError "tag" (parseDocumentListQuery emptyRawDocumentListQuery { rawTag = Just "" })
        assertFieldError "folder" (parseDocumentListQuery emptyRawDocumentListQuery { rawFolder = Just "../escape" })
        assertFieldError "include_subfolders" (parseDocumentListQuery emptyRawDocumentListQuery { rawIncludeSubfolders = Just "yes" })
        assertFieldError "q" (parseDocumentListQuery emptyRawDocumentListQuery { rawSearch = Just "bad\nquery" })
        assertFieldError "limit" (parseDocumentListQuery emptyRawDocumentListQuery { rawLimit = Just "abc" })
        assertFieldError "limit" (parseDocumentListQuery emptyRawDocumentListQuery { rawLimit = Just "0" })
        assertFieldError "limit" (parseDocumentListQuery emptyRawDocumentListQuery { rawLimit = Just "201" })
        assertFieldError "limit" (parseDocumentListQuery emptyRawDocumentListQuery { rawLimit = Just "99999999999999999999999999" })
        assertFieldError "offset" (parseDocumentListQuery emptyRawDocumentListQuery { rawOffset = Just "xyz" })
        assertFieldError "offset" (parseDocumentListQuery emptyRawDocumentListQuery { rawOffset = Just "-1" })
        assertFieldError "offset" (parseDocumentListQuery emptyRawDocumentListQuery { rawOffset = Just "99999999999999999999999999" })
        assertFieldError "id" (parseDocumentIdParam "bad!id")

restEndpointSpec :: Spec
restEndpointSpec = describe "WAI / Servant REST API endpoints" $ do
    it "serves GET /api/health with 200 OK JSON without authentication" $
        withTestApp $ \ app ->
            WaiTest.runSession
                ( do
                    res <- sendJsonRequest Http.methodGet "/api/health" [] Nothing
                    WaiTest.assertStatus 200 res
                    assertJsonContentType res
                    body <- liftDecode res :: WaiTest.Session HealthStatus
                    liftEq body healthOk
                )
                app

    it "executes full authenticated CRUD, trash, restore, permanent delete, filtering, and metadata listing" $
        withTestApp $ \ app ->
            WaiTest.runSession
                ( do
                    sess <- registerTestUser "api_user"
                    let readHdrs :: Http.RequestHeaders
                        readHdrs = [(Http.hCookie, sess.cookie)]

                        writeHdrs :: Http.RequestHeaders
                        writeHdrs = [(Http.hCookie, sess.cookie), (csrfHeaderName, sess.csrf)]

                    -- 1. 空オブジェクト {} による即時メモ作成 (201 Created)
                    resBlank <- sendJsonRequest Http.methodPost "/api/documents" writeHdrs (Just "{}")
                    WaiTest.assertStatus 201 resBlank
                    assertJsonContentType resBlank
                    blankDoc <- liftDecode resBlank :: WaiTest.Session Document
                    liftEq blankDoc.title emptyDocumentTitle
                    liftEq blankDoc.body emptyDocumentBody
                    liftEq blankDoc.format FormatMarkdown
                    liftEq blankDoc.folder Nothing
                    liftEq blankDoc.tags emptyTagSet
                    liftEq blankDoc.deletedAt Nothing

                    -- 2. 全項目指定による文書作成 (201 Created)
                    let createPayload :: LazyBS.ByteString
                        createPayload = Aeson.encode $ Aeson.object
                            [ "title"  Aeson..= ("設計メモ" :: Text)
                            , "body"   Aeson..= ("# 見出し\nWAI Servant 実装" :: Text)
                            , "format" Aeson..= ("markdown" :: Text)
                            , "folder" Aeson..= ("/work/notes/" :: Text)
                            , "tags"   Aeson..= (["haskell", "設計", "haskell"] :: [Text])
                            ]
                    resFull <- sendJsonRequest Http.methodPost "/api/documents" writeHdrs (Just createPayload)
                    WaiTest.assertStatus 201 resFull
                    fullDoc <- liftDecode resFull :: WaiTest.Session Document
                    liftEq (toText fullDoc.title) "設計メモ"
                    liftEq (fmap toText fullDoc.folder) (Just "work/notes")

                    let fullIdPath :: ByteString
                        fullIdPath = "/api/documents/" <> TxtEnc.encodeUtf8 (toText fullDoc.id)

                    -- 3. 単一取得 (200 OK)
                    resGet <- sendJsonRequest Http.methodGet fullIdPath readHdrs Nothing
                    WaiTest.assertStatus 200 resGet
                    fetchedDoc <- liftDecode resGet :: WaiTest.Session Document
                    liftEq fetchedDoc fullDoc

                    -- 4. 更新 (PUT /api/documents/{id} -> 200 OK)
                    let updatePayload :: LazyBS.ByteString
                        updatePayload = Aeson.encode $ Aeson.object
                            [ "title"  Aeson..= ("設計メモ 改訂" :: Text)
                            , "body"   Aeson..= ("改訂後の本文 Servant" :: Text)
                            , "format" Aeson..= ("plain_text" :: Text)
                            , "folder" Aeson..= ("work/notes/v2" :: Text)
                            , "tags"   Aeson..= (["haskell", "v2"] :: [Text])
                            ]
                    resUpdate <- sendJsonRequest Http.methodPut fullIdPath writeHdrs (Just updatePayload)
                    WaiTest.assertStatus 200 resUpdate
                    updatedDoc <- liftDecode resUpdate :: WaiTest.Session Document
                    liftEq (toText updatedDoc.title) "設計メモ 改訂"
                    liftEq updatedDoc.format FormatPlainText
                    liftEq (fmap toText updatedDoc.folder) (Just "work/notes/v2")
                    liftEq updatedDoc.createdAt fullDoc.createdAt

                    -- 5. 一覧・フィルタ・検索・ページネーション (GET /api/documents)
                    resListFiltered <- sendJsonRequest
                        Http.methodGet
                        "/api/documents?status=active&tag=haskell&folder=work&include_subfolders=true&q=servant&limit=10&offset=0"
                        readHdrs
                        Nothing
                    WaiTest.assertStatus 200 resListFiltered
                    filteredList <- liftDecode resListFiltered :: WaiTest.Session DocumentListResponse
                    liftEq filteredList.total 1
                    liftEq (map (\ s -> s.id) filteredList.documents) [fullDoc.id]

                    -- 6. タグ・フォルダ一覧 (GET /api/tags, GET /api/folders)
                    resTags <- sendJsonRequest Http.methodGet "/api/tags" readHdrs Nothing
                    WaiTest.assertStatus 200 resTags
                    tagList <- liftDecode resTags :: WaiTest.Session TagListResponse
                    liftEq (map toText tagList.tags) ["haskell", "v2"]

                    resFolders <- sendJsonRequest Http.methodGet "/api/folders" readHdrs Nothing
                    WaiTest.assertStatus 200 resFolders
                    folderList <- liftDecode resFolders :: WaiTest.Session FolderListResponse
                    liftEq (map toText folderList.folders) ["work/notes/v2"]

                    -- 7. 稼働中文書の完全削除は 409 Conflict
                    resPermActive <- sendJsonRequest Http.methodDelete (fullIdPath <> "/permanent") writeHdrs Nothing
                    WaiTest.assertStatus 409 resPermActive
                    permActiveErr <- liftDecode resPermActive :: WaiTest.Session ApiError
                    liftEq permActiveErr.code ErrConflict

                    -- 8. 論理削除 (DELETE /api/documents/{id} -> 200 OK) および冪等性
                    resTrash1 <- sendJsonRequest Http.methodDelete fullIdPath writeHdrs Nothing
                    WaiTest.assertStatus 200 resTrash1
                    trashed1 <- liftDecode resTrash1 :: WaiTest.Session Document
                    liftSatisfy trashed1.deletedAt isJust
                    liftEq trashed1.updatedAt updatedDoc.updatedAt

                    resTrash2 <- sendJsonRequest Http.methodDelete fullIdPath writeHdrs Nothing
                    WaiTest.assertStatus 200 resTrash2
                    trashed2 <- liftDecode resTrash2 :: WaiTest.Session Document
                    liftEq trashed2 trashed1

                    -- 9. ゴミ箱内文書のプレビュー取得は 200 OK、更新は 409 Conflict
                    resPreview <- sendJsonRequest Http.methodGet fullIdPath readHdrs Nothing
                    WaiTest.assertStatus 200 resPreview
                    previewed <- liftDecode resPreview :: WaiTest.Session Document
                    liftEq previewed trashed1

                    resUpdateTrashed <- sendJsonRequest Http.methodPut fullIdPath writeHdrs (Just updatePayload)
                    WaiTest.assertStatus 409 resUpdateTrashed
                    updateTrashedErr <- liftDecode resUpdateTrashed :: WaiTest.Session ApiError
                    liftEq updateTrashedErr.code ErrConflict

                    -- 10. ゴミ箱内文書は active フィルタおよび /api/tags, /api/folders から除外される
                    resTrashedList <- sendJsonRequest Http.methodGet "/api/documents?status=trashed" readHdrs Nothing
                    WaiTest.assertStatus 200 resTrashedList
                    trashedList <- liftDecode resTrashedList :: WaiTest.Session DocumentListResponse
                    liftEq trashedList.total 1
                    liftEq (map (\ s -> s.id) trashedList.documents) [fullDoc.id]

                    resEmptyTags <- sendJsonRequest Http.methodGet "/api/tags" readHdrs Nothing
                    WaiTest.assertStatus 200 resEmptyTags
                    emptyTagList <- liftDecode resEmptyTags :: WaiTest.Session TagListResponse
                    liftEq emptyTagList.tags []

                    -- 11. 復元 (POST /api/documents/{id}/restore -> 200 OK) および冪等性
                    resRestore1 <- sendJsonRequest Http.methodPost (fullIdPath <> "/restore") writeHdrs Nothing
                    WaiTest.assertStatus 200 resRestore1
                    restored1 <- liftDecode resRestore1 :: WaiTest.Session Document
                    liftEq restored1.deletedAt Nothing
                    liftEq restored1 updatedDoc

                    resRestore2 <- sendJsonRequest Http.methodPost (fullIdPath <> "/restore") writeHdrs Nothing
                    WaiTest.assertStatus 200 resRestore2
                    restored2 <- liftDecode resRestore2 :: WaiTest.Session Document
                    liftEq restored2 restored1

                    -- 12. 再度ゴミ箱移動後に完全削除 (DELETE /api/documents/{id}/permanent -> 204 No Content)
                    resTrash3 <- sendJsonRequest Http.methodDelete fullIdPath writeHdrs Nothing
                    WaiTest.assertStatus 200 resTrash3

                    resPerm <- sendJsonRequest Http.methodDelete (fullIdPath <> "/permanent") writeHdrs Nothing
                    WaiTest.assertStatus 204 resPerm
                    liftEq (WaiTest.simpleBody resPerm) ""

                    -- 13. 完全削除後は 404 Not Found
                    resGone <- sendJsonRequest Http.methodGet fullIdPath readHdrs Nothing
                    WaiTest.assertStatus 404 resGone
                    goneErr <- liftDecode resGone :: WaiTest.Session ApiError
                    liftEq goneErr.code ErrNotFound
                )
                app

errorHandlingSpec :: Spec
errorHandlingSpec = describe "Structured JSON error handling across status codes" $ do
    it "returns 400 JSON ApiError for invalid IDs, invalid queries, malformed JSON, and missing PUT fields" $
        withTestApp $ \ app ->
            WaiTest.runSession
                ( do
                    sess <- registerTestUser "err_user"
                    let readHdrs :: Http.RequestHeaders
                        readHdrs = [(Http.hCookie, sess.cookie)]

                        writeHdrs :: Http.RequestHeaders
                        writeHdrs = [(Http.hCookie, sess.cookie), (csrfHeaderName, sess.csrf)]

                    -- 不正パス ID
                    resBadId <- sendJsonRequest Http.methodGet "/api/documents/bad!id" readHdrs Nothing
                    WaiTest.assertStatus 400 resBadId
                    assertJsonContentType resBadId
                    badIdErr <- liftDecode resBadId :: WaiTest.Session ApiError
                    liftEq badIdErr.code ErrValidation
                    liftEq badIdErr.field (Just "id")

                    -- 不正クエリパラメータ (tag の field は "tag")
                    resBadQuery <- sendJsonRequest Http.methodGet "/api/documents?tag=" readHdrs Nothing
                    WaiTest.assertStatus 400 resBadQuery
                    badQueryErr <- liftDecode resBadQuery :: WaiTest.Session ApiError
                    liftEq badQueryErr.code ErrValidation
                    liftEq badQueryErr.field (Just "tag")

                    -- 壊れた JSON
                    resMalformed <- sendJsonRequest Http.methodPost "/api/documents" writeHdrs (Just "{not-json")
                    WaiTest.assertStatus 400 resMalformed
                    assertJsonContentType resMalformed
                    malformedErr <- liftDecode resMalformed :: WaiTest.Session ApiError
                    liftEq malformedErr.code ErrValidation

                    -- 制御文字を含むタイトル
                    resBadTitle <- sendJsonRequest Http.methodPost "/api/documents" writeHdrs (Just "{\"title\":\"a\\nb\"}")
                    WaiTest.assertStatus 400 resBadTitle
                    badTitleErr <- liftDecode resBadTitle :: WaiTest.Session ApiError
                    liftEq badTitleErr.code ErrValidation

                    -- PUT 必須項目欠落
                    resIncompletePut <- sendJsonRequest Http.methodPut "/api/documents/doc-1" writeHdrs (Just "{\"title\":\"only\"}")
                    WaiTest.assertStatus 400 resIncompletePut
                    incompleteErr <- liftDecode resIncompletePut :: WaiTest.Session ApiError
                    liftEq incompleteErr.code ErrValidation
                )
                app

    it "returns 413 JSON ApiError when document body exceeds 5 MiB (POST/PUT) or raw HTTP body exceeds 16 MiB (KnownLength and ChunkedBody)" $
        withTestApp $ \ app -> do
            sess <- WaiTest.runSession (registerTestUser "size_user") app
            let writeHdrs :: Http.RequestHeaders
                writeHdrs = [(Http.hCookie, sess.cookie), (csrfHeaderName, sess.csrf)]

            WaiTest.runSession
                ( do
                    let oversizedBodyJson :: LazyBS.ByteString
                        oversizedBodyJson = Aeson.encode $ Aeson.object
                            [ "body" Aeson..= Txt.replicate (maxDocumentBodyBytes + 1) "a"
                            ]
                    resTooLargeDoc <- sendJsonRequest Http.methodPost "/api/documents" writeHdrs (Just oversizedBodyJson)
                    WaiTest.assertStatus 413 resTooLargeDoc
                    assertJsonContentType resTooLargeDoc
                    docErr <- liftDecode resTooLargeDoc :: WaiTest.Session ApiError
                    liftEq docErr.code ErrPayloadTooLarge
                    liftEq docErr.field (Just "body")

                    -- PUT でも 5 MiB 超の本文を 413 として拒否する
                    resSeed <- sendJsonRequest Http.methodPost "/api/documents" writeHdrs (Just "{}")
                    WaiTest.assertStatus 201 resSeed
                    seeded <- liftDecode resSeed :: WaiTest.Session Document
                    let seedPath :: ByteString
                        seedPath = "/api/documents/" <> TxtEnc.encodeUtf8 (toText seeded.id)

                        oversizedUpdateJson :: LazyBS.ByteString
                        oversizedUpdateJson = Aeson.encode $ Aeson.object
                            [ "title"  Aeson..= ("" :: Text)
                            , "body"   Aeson..= Txt.replicate (maxDocumentBodyBytes + 1) "a"
                            , "format" Aeson..= ("markdown" :: Text)
                            , "folder" Aeson..= (Nothing :: Maybe Text)
                            , "tags"   Aeson..= ([] :: [Text])
                            ]
                    resTooLargePut <- sendJsonRequest Http.methodPut seedPath writeHdrs (Just oversizedUpdateJson)
                    WaiTest.assertStatus 413 resTooLargePut
                    assertJsonContentType resTooLargePut
                    putErr <- liftDecode resTooLargePut :: WaiTest.Session ApiError
                    liftEq putErr.code ErrPayloadTooLarge
                    liftEq putErr.field (Just "body")

                    let oversizedRawReq :: Wai.Request
                        oversizedRawReq =
                            (WaiTest.setPath WaiTest.defaultRequest "/api/documents")
                                { Wai.requestMethod     = Http.methodPost
                                , Wai.requestHeaders    = (Http.hContentType, "application/json") : writeHdrs
                                , Wai.requestBodyLength = Wai.KnownLength (fromIntegral (maxRawRequestBodyBytes + 1))
                                }
                    resTooLargeRaw <- WaiTest.srequest (WaiTest.SRequest oversizedRawReq "{}")
                    WaiTest.assertStatus 413 resTooLargeRaw
                    assertJsonContentType resTooLargeRaw
                    liftEq (lookup Http.hCacheControl (WaiTest.simpleHeaders resTooLargeRaw)) (Just "no-store")
                    rawErr <- liftDecode resTooLargeRaw :: WaiTest.Session ApiError
                    liftEq rawErr.code ErrPayloadTooLarge
                )
                app

            -- ChunkedBody (Content-Length 不明) のストリーミング超過が Servant 越しに 413 JSON となることを直接検証
            -- 有効な JSON 文字列の途中をストリーム送信することで、Aeson が 16 MiB 境界を越えて読み込み続けるパスを確実に踏む
            let chunkSize :: Int
                chunkSize = 1024 * 1024

                oneMbChunk :: ByteString
                oneMbChunk = BS8.replicate chunkSize 'a'

                streamChunks :: [ByteString]
                streamChunks =
                    "{\"body\":\""
                        : replicate ((maxRawRequestBodyBytes `div` chunkSize) + 1) oneMbChunk
                        <> ["\"}"]
            remainingChunksRef <- newIORef streamChunks
            let popChunk :: [ByteString] -> ([ByteString], ByteString)
                popChunk []           = ([], BS.empty)
                popChunk (chunk : rs) = (rs, chunk)

                nextChunk :: IO ByteString
                nextChunk = atomicModifyIORef' remainingChunksRef popChunk

                chunkedReq :: Wai.Request
                chunkedReq = Wai.setRequestBodyChunks nextChunk $
                    (WaiTest.setPath WaiTest.defaultRequest "/api/documents")
                        { Wai.requestMethod     = Http.methodPost
                        , Wai.requestHeaders    = (Http.hContentType, "application/json") : writeHdrs
                        , Wai.requestBodyLength = Wai.ChunkedBody
                        }
            (chunkStatus, chunkHeaders, chunkBody) <- runRawWaiRequest app chunkedReq
            chunkStatus `shouldBe` Http.status413
            case lookup Http.hContentType chunkHeaders of
                Nothing -> expectationFailure "Missing Content-Type on chunked 413 response"
                Just ct -> Txt.toLower (TxtEnc.decodeUtf8Lenient ct) `shouldSatisfy` Txt.isPrefixOf "application/json"
            case Aeson.eitherDecode chunkBody :: Either String ApiError of
                Left err     -> expectationFailure ("Failed to decode chunked 413 JSON: " <> err)
                Right apiErr -> apiErr.code `shouldBe` ErrPayloadTooLarge

    it "returns 404, 405, 406, and 415 as structured JSON ApiError" $
        withTestApp $ \ app ->
            WaiTest.runSession
                ( do
                    sess <- registerTestUser "status_user"
                    let writeHdrs :: Http.RequestHeaders
                        writeHdrs = [(Http.hCookie, sess.cookie), (csrfHeaderName, sess.csrf)]

                    -- 404 Not Found (未知パス)
                    res404 <- sendJsonRequest Http.methodGet "/api/unknown" [] Nothing
                    WaiTest.assertStatus 404 res404
                    assertJsonContentType res404
                    err404 <- liftDecode res404 :: WaiTest.Session ApiError
                    liftEq err404.code ErrNotFound

                    -- 405 Method Not Allowed
                    res405Health <- sendJsonRequest Http.methodPost "/api/health" [] (Just "{}")
                    WaiTest.assertStatus 405 res405Health
                    assertJsonContentType res405Health
                    err405Health <- liftDecode res405Health :: WaiTest.Session ApiError
                    liftEq err405Health.code ErrMethodNotAllowed

                    res405Docs <- sendJsonRequest Http.methodDelete "/api/documents" [] Nothing
                    WaiTest.assertStatus 405 res405Docs
                    assertJsonContentType res405Docs
                    err405Docs <- liftDecode res405Docs :: WaiTest.Session ApiError
                    liftEq err405Docs.code ErrMethodNotAllowed

                    res405Restore <- sendJsonRequest Http.methodGet "/api/documents/doc-1/restore" [] Nothing
                    WaiTest.assertStatus 405 res405Restore
                    assertJsonContentType res405Restore
                    err405Restore <- liftDecode res405Restore :: WaiTest.Session ApiError
                    liftEq err405Restore.code ErrMethodNotAllowed

                    -- 406 Not Acceptable
                    let notAcceptableReq :: Wai.Request
                        notAcceptableReq =
                            (WaiTest.setPath WaiTest.defaultRequest "/api/health")
                                { Wai.requestMethod  = Http.methodGet
                                , Wai.requestHeaders = [(Http.hAccept, "text/plain")]
                                }
                    res406 <- WaiTest.request notAcceptableReq
                    WaiTest.assertStatus 406 res406
                    assertJsonContentType res406
                    err406 <- liftDecode res406 :: WaiTest.Session ApiError
                    liftEq err406.code ErrValidation

                    -- 415 Unsupported Media Type
                    let plainReq :: Wai.Request
                        plainReq =
                            (WaiTest.setPath WaiTest.defaultRequest "/api/documents")
                                { Wai.requestMethod  = Http.methodPost
                                , Wai.requestHeaders = (Http.hContentType, "text/plain") : writeHdrs
                                }
                    res415 <- WaiTest.srequest (WaiTest.SRequest plainReq "{}")
                    WaiTest.assertStatus 415 res415
                    assertJsonContentType res415
                    err415 <- liftDecode res415 :: WaiTest.Session ApiError
                    liftEq err415.code ErrValidation
                )
                app

    it "returns masked 500 JSON ApiError when storage fails with unsupported schema version" $
        withTempDbPath "unsupported.sqlite3" $ \ dbPath -> do
            -- sqlite-simple の withConnection には OsPath 版がないためテスト準備時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            SQLite.withConnection dbStr $ \ conn ->
                SQLite.execute_ conn "PRAGMA user_version = 99"
            let app :: Wai.Application
                app = mkApplication (defaultStorageConfig dbPath)
            WaiTest.runSession
                ( do
                    -- 形式妥当なセッショントークンで SQLite 照合経路へ到達させ、マスクされた 500 JSON を検証する
                    let validFormatCookie :: ByteString
                        validFormatCookie = "tamatsuzura_session=" <> BS8.replicate 43 'a'
                    res <- sendJsonRequest Http.methodGet "/api/documents" [(Http.hCookie, validFormatCookie)] Nothing
                    WaiTest.assertStatus 500 res
                    assertJsonContentType res
                    err500 <- liftDecode res :: WaiTest.Session ApiError
                    liftEq err500.code ErrInternalServer
                    liftEq err500.message "Internal server error"
                    liftEq err500.field Nothing
                )
                app

registerTestUser :: Text -> WaiTest.Session SessionHeaders
registerTestUser uname = do
    let payload :: LazyBS.ByteString
        payload = Aeson.encode $ Aeson.object
            [ "username" Aeson..= uname
            , "password" Aeson..= ("valid-test-password" :: Text)
            ]
    res <- sendJsonRequest Http.methodPost "/api/auth/register" [] (Just payload)
    WaiTest.assertStatus 201 res
    authRes <- liftDecode res :: WaiTest.Session AuthSessionResponse
    case lookup "Set-Cookie" (WaiTest.simpleHeaders res) of
        Nothing -> do
            liftIO (expectationFailure "Missing Set-Cookie on register")
            liftIO (fail "Unreachable after expectationFailure")
        Just rawSetCookie ->
            case BS8.split ';' rawSetCookie of
                pair : _ -> pure SessionHeaders
                    { cookie = pair
                    , csrf   = TxtEnc.encodeUtf8 (toText authRes.csrfToken)
                    }
                [] -> do
                    liftIO (expectationFailure "Malformed Set-Cookie header")
                    liftIO (fail "Unreachable after expectationFailure")

sendJsonRequest ::
       Http.Method
    -> ByteString
    -> Http.RequestHeaders
    -> Maybe LazyBS.ByteString
    -> WaiTest.Session WaiTest.SResponse
sendJsonRequest method rawPath extraHeaders mBody =
    let headers :: Http.RequestHeaders
        headers = case mBody of
            Nothing -> (Http.hAccept, "application/json") : extraHeaders
            Just _  ->
                [ (Http.hContentType, "application/json")
                , (Http.hAccept, "application/json")
                ] <> extraHeaders

        bodyBytes :: LazyBS.ByteString
        bodyBytes = fromMaybe "" mBody

        req :: Wai.Request
        req =
            (WaiTest.setPath WaiTest.defaultRequest rawPath)
                { Wai.requestMethod  = method
                , Wai.requestHeaders = headers
                }
    in  WaiTest.srequest (WaiTest.SRequest req bodyBytes)

assertJsonContentType :: WaiTest.SResponse -> WaiTest.Session ()
assertJsonContentType res =
    case lookup Http.hContentType (WaiTest.simpleHeaders res) of
        Just ct ->
            if "application/json" `Txt.isPrefixOf` Txt.toLower (TxtEnc.decodeUtf8Lenient ct)
                then pure ()
                else liftIO (expectationFailure ("Expected application/json Content-Type, got: " <> show ct))
        Nothing ->
            liftIO (expectationFailure "Missing Content-Type header in response")

liftDecode :: Aeson.FromJSON a => WaiTest.SResponse -> WaiTest.Session a
liftDecode res =
    case Aeson.eitherDecode (WaiTest.simpleBody res) of
        Left err  -> do
            liftIO (expectationFailure ("Failed to decode JSON body: " <> err <> " (body=" <> show (WaiTest.simpleBody res) <> ")"))
            liftIO (fail "Unreachable after expectationFailure")
        Right val -> pure val

liftEq :: (Eq a, Show a) => a -> a -> WaiTest.Session ()
liftEq actual expected =
    liftIO (actual `shouldBe` expected)

liftSatisfy :: Show a => a -> (a -> Bool) -> WaiTest.Session ()
liftSatisfy actual predicate =
    liftIO (actual `shouldSatisfy` predicate)

assertFieldError :: Show a => Text -> Either ApiError a -> IO ()
assertFieldError expectedField (Left err) = do
    err.code `shouldBe` ErrValidation
    err.field `shouldBe` Just expectedField
assertFieldError expectedField (Right val) =
    expectationFailure ("Expected validation error on field " <> Txt.unpack expectedField <> ", got Right: " <> show val)

withTestApp :: (Wai.Application -> IO a) -> IO a
withTestApp action =
    withTempDbPath "api.sqlite3" $ \ dbPath ->
        action (mkApplicationWithAuth fastAuthCfg (defaultStorageConfig dbPath))
  where
    fastAuthCfg :: AuthConfig
    fastAuthCfg = defaultAuthConfig { passwordHashParams = fastTestPasswordHashParams }

withTempDbPath :: String -> (OsPath -> IO a) -> IO a
withTempDbPath relFileName action = do
    tmpRoot <- DirOS.getTemporaryDirectory
    rawUuid <- UUID4.nextRandom
    -- UUID 文字列およびテスト用ファイル名から OsPath を生成するため encodeFS を使用する
    dirPart <- OS.encodeFS ("tamatsuzura-api-test-" <> UUID.toString rawUuid)
    filePart <- OS.encodeFS relFileName
    let testDir :: OsPath
        testDir = tmpRoot OS.</> dirPart

        dbPath :: OsPath
        dbPath = testDir OS.</> filePart
    bracket
        (DirOS.createDirectoryIfMissing True testDir >> pure dbPath)
        (const (DirOS.removePathForcibly testDir))
        action

runRawWaiRequest ::
       Wai.Application
    -> Wai.Request
    -> IO (Http.Status, Http.ResponseHeaders, LazyBS.ByteString)
runRawWaiRequest app req = do
    resultRef <- newIORef Nothing
    _ <- app req $ \ res -> do
        let (status, headers, withBody) = Wai.responseToStream res
        chunksRef <- newIORef ([] :: [ByteString])
        withBody $ \ streamingBody ->
            streamingBody
                (\ builder -> modifyIORef' chunksRef (LazyBS.toStrict (Builder.toLazyByteString builder) :))
                (pure ())
        bodyChunks <- reverse <$> readIORef chunksRef
        writeIORef resultRef (Just (status, headers, LazyBS.fromChunks bodyChunks))
        pure WaiInternal.ResponseReceived
    mResult <- readIORef resultRef
    case mResult of
        Just result -> pure result
        Nothing     -> do
            expectationFailure "WAI application did not invoke respond callback"
            fail "Unreachable after expectationFailure"

mustRight :: Show e => Either e a -> IO a
mustRight (Right val) = pure val
mustRight (Left err)  = do
    expectationFailure ("Expected Right, got Left: " <> show err)
    fail "Unreachable after expectationFailure"
