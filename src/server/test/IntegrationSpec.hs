{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module IntegrationSpec
    ( integrationSpec
    ) where

import           Control.Exception                  (bracket)
import qualified Data.Aeson                         as Aeson
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Char8              as BS8
import qualified Data.ByteString.Lazy               as LazyBS
import           Data.IORef                         (modifyIORef', newIORef, readIORef)
import           Data.Maybe                         (fromMaybe, isJust)
import           Data.Text                          (Text)
import qualified Data.Text                          as Txt
import qualified Data.Text.Encoding                 as TxtEnc
import qualified Data.UUID                          as UUID
import qualified Data.UUID.V4                       as UUID4
import qualified Network.HTTP.Client                as HttpClient
import qualified Network.HTTP.Types                 as Http
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Handler.CGI            as CGI
import qualified Network.Wai.Handler.Warp           as Warp
import qualified System.Directory.OsPath            as DirOS
import           System.OsPath                      (OsPath)
import qualified System.OsPath                      as OS
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , DocumentListResponse(..)
    , FolderListResponse(..)
    , HealthStatus(..)
    , TagListResponse(..)
    , healthOk
    )
import           TamaTsuzura.Common.Auth            (AuthSessionResponse(..), UserProfile(..))
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentFormat(..)
    , DocumentSummary(..)
    , revisionNumberValue
    )
import           TamaTsuzura.Common.Revision
    ( RevisionDetail(..)
    , RevisionListResponse(..)
    , RevisionSummary(..)
    )
import           TamaTsuzura.Common.Text            (ToText(..))
import           TamaTsuzura.Server.App
    ( configErrorApplication
    , mkApplicationWithAuth
    , mkCgiApplication
    )
import           TamaTsuzura.Server.Auth.Cookie     (AuthConfig(..), defaultAuthConfig)
import           TamaTsuzura.Server.Auth.Crypto     (fastTestPasswordHashParams)
import           TamaTsuzura.Server.Auth.Middleware (csrfHeaderName)
import           TamaTsuzura.Server.Config
    ( CgiConfig(..)
    , envAllowRegistration
    , envDbBusyTimeoutMs
    , envDbPath
    , envHttps
    , resolveCgiConfig
    )
import           TamaTsuzura.Server.Storage
    ( SqlitePragmas(..)
    , defaultStorageConfig
    , inspectStoragePragmas
    , withStorage
    )
import           Test.Hspec                         (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import           Text.Read                          (readMaybe)

data CgiResponse = CgiResponse
    { statusCode :: !Int
    , headers    :: ![(Text, Text)]
    , body       :: !LazyBS.ByteString
    } deriving (Eq, Show)

data SessionCreds = SessionCreds
    { cookiePair :: !ByteString
    , csrfToken  :: !ByteString
    } deriving (Eq, Show)

integrationSpec :: Spec
integrationSpec = do
    warpIntegrationSpec
    cgiIntegrationSpec

warpIntegrationSpec :: Spec
warpIntegrationSpec = describe "Warp standalone server end-to-end integration" $ do
    it "serves auth, CSRF, multi-user owner isolation, and full document CRUD over TCP HTTP with WAL SQLite" $
        withTempDbPath "warp-e2e.sqlite3" $ \ dbPath -> do
            let storageCfg = defaultStorageConfig dbPath
                authCfg    = defaultAuthConfig { passwordHashParams = fastTestPasswordHashParams }
            manager <- HttpClient.newManager HttpClient.defaultManagerSettings
            Warp.testWithApplication (pure (mkApplicationWithAuth authCfg storageCfg)) $ \ port -> do
                let baseUrl :: String
                    baseUrl = "http://127.0.0.1:" <> show port

                -- 1. Health check (public) & unauthenticated /api/documents -> 401
                resHealth <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/health") [] Nothing
                Http.statusCode (HttpClient.responseStatus resHealth) `shouldBe` 200
                healthVal <- decodeJson (HttpClient.responseBody resHealth) :: IO HealthStatus
                healthVal `shouldBe` healthOk

                resUnauth <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/documents") [] Nothing
                Http.statusCode (HttpClient.responseStatus resUnauth) `shouldBe` 401

                -- 2. Register Alice & Bob
                (alice, aliceProfile) <- warpRegister manager baseUrl "warp_alice" "alice-password-1"
                (bob, _) <- warpRegister manager baseUrl "warp_bob" "bob-password-1"

                let aliceRead :: Http.RequestHeaders
                    aliceRead = [(Http.hCookie, alice.cookiePair)]

                    aliceWrite :: Http.RequestHeaders
                    aliceWrite = [(Http.hCookie, alice.cookiePair), (csrfHeaderName, alice.csrfToken)]

                    bobRead :: Http.RequestHeaders
                    bobRead = [(Http.hCookie, bob.cookiePair)]

                    bobWrite :: Http.RequestHeaders
                    bobWrite = [(Http.hCookie, bob.cookiePair), (csrfHeaderName, bob.csrfToken)]

                -- 3. /api/auth/me returns Alice profile
                resMe <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/auth/me") aliceRead Nothing
                Http.statusCode (HttpClient.responseStatus resMe) `shouldBe` 200
                meRes <- decodeJson (HttpClient.responseBody resMe) :: IO AuthSessionResponse
                meRes.user `shouldBe` aliceProfile

                -- 4. Missing CSRF token -> 403
                resNoCsrf <- httpJsonWith manager Http.methodPost (baseUrl <> "/api/documents") aliceRead (Just "{}")
                Http.statusCode (HttpClient.responseStatus resNoCsrf) `shouldBe` 403

                -- 5. Create document as Alice
                let createBody :: LazyBS.ByteString
                    createBody = Aeson.encode $ Aeson.object
                        [ "title"  Aeson..= ("Warp 疎通文書" :: Text)
                        , "body"   Aeson..= ("# Warp\nTCP 経由の永続化テスト" :: Text)
                        , "format" Aeson..= ("markdown" :: Text)
                        , "folder" Aeson..= ("warp/test" :: Text)
                        , "tags"   Aeson..= (["warp", "e2e"] :: [Text])
                        ]
                resCreate <- httpJsonWith manager Http.methodPost (baseUrl <> "/api/documents") aliceWrite (Just createBody)
                Http.statusCode (HttpClient.responseStatus resCreate) `shouldBe` 201
                created <- decodeJson (HttpClient.responseBody resCreate) :: IO Document

                let docUrl :: String
                    docUrl = baseUrl <> "/api/documents/" <> Txt.unpack (toText created.id)

                -- 6. Bob cannot see or mutate Alice's document, tags, or folders
                resBobGet <- httpJsonWith manager Http.methodGet docUrl bobRead Nothing
                Http.statusCode (HttpClient.responseStatus resBobGet) `shouldBe` 404
                resBobList <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/documents?status=all") bobRead Nothing
                Http.statusCode (HttpClient.responseStatus resBobList) `shouldBe` 200
                bobList <- decodeJson (HttpClient.responseBody resBobList) :: IO DocumentListResponse
                bobList.total `shouldBe` 0
                resBobTags <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/tags") bobRead Nothing
                bobTags <- decodeJson (HttpClient.responseBody resBobTags) :: IO TagListResponse
                bobTags.tags `shouldBe` []
                resBobDel <- httpJsonWith manager Http.methodDelete docUrl bobWrite Nothing
                Http.statusCode (HttpClient.responseStatus resBobDel) `shouldBe` 404

                -- 7. Alice Get, Update, List, Tags, Folders
                resGet <- httpJsonWith manager Http.methodGet docUrl aliceRead Nothing
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
                resUpdate <- httpJsonWith manager Http.methodPut docUrl aliceWrite (Just updateBody)
                Http.statusCode (HttpClient.responseStatus resUpdate) `shouldBe` 200
                updated <- decodeJson (HttpClient.responseBody resUpdate) :: IO Document
                toText updated.title `shouldBe` "Warp 更新済み"
                updated.format `shouldBe` FormatPlainText
                updated.createdAt `shouldBe` created.createdAt

                resList <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/documents?folder=warp&include_subfolders=true&tag=updated") aliceRead Nothing
                Http.statusCode (HttpClient.responseStatus resList) `shouldBe` 200
                listed <- decodeJson (HttpClient.responseBody resList) :: IO DocumentListResponse
                listed.total `shouldBe` 1
                map (\ s -> s.id) listed.documents `shouldBe` [created.id]

                resTags <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/tags") aliceRead Nothing
                tagList <- decodeJson (HttpClient.responseBody resTags) :: IO TagListResponse
                map toText tagList.tags `shouldBe` ["updated", "warp"]

                resFolders <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/folders") aliceRead Nothing
                folderList <- decodeJson (HttpClient.responseBody resFolders) :: IO FolderListResponse
                map toText folderList.folders `shouldBe` ["warp/updated"]

                -- 7b. Revision history inspection, owner isolation, base_revision conflict, and rollback over Warp
                resBobRevs <- httpJsonWith manager Http.methodGet (docUrl <> "/revisions") bobRead Nothing
                Http.statusCode (HttpClient.responseStatus resBobRevs) `shouldBe` 404

                resRevs <- httpJsonWith manager Http.methodGet (docUrl <> "/revisions") aliceRead Nothing
                Http.statusCode (HttpClient.responseStatus resRevs) `shouldBe` 200
                revList <- decodeJson (HttpClient.responseBody resRevs) :: IO RevisionListResponse
                revList.total `shouldBe` 2
                map (\ s -> revisionNumberValue s.revision) revList.revisions `shouldBe` [2, 1]

                resRev1 <- httpJsonWith manager Http.methodGet (docUrl <> "/revisions/1") aliceRead Nothing
                Http.statusCode (HttpClient.responseStatus resRev1) `shouldBe` 200
                rev1 <- decodeJson (HttpClient.responseBody resRev1) :: IO RevisionDetail
                toText rev1.title `shouldBe` "Warp 疎通文書"
                toText rev1.body `shouldBe` "# Warp\nTCP 経由の永続化テスト"

                resRollbackConflict <- httpJsonWith manager Http.methodPost (docUrl <> "/revisions/1/restore") aliceWrite (Just "{\"base_revision\":1}")
                Http.statusCode (HttpClient.responseStatus resRollbackConflict) `shouldBe` 409

                resRollback <- httpJsonWith manager Http.methodPost (docUrl <> "/revisions/1/restore") aliceWrite (Just "{\"base_revision\":2}")
                Http.statusCode (HttpClient.responseStatus resRollback) `shouldBe` 200
                rolledBack <- decodeJson (HttpClient.responseBody resRollback) :: IO Document
                revisionNumberValue rolledBack.revision `shouldBe` 3
                toText rolledBack.title `shouldBe` "Warp 疎通文書"
                toText rolledBack.body `shouldBe` "# Warp\nTCP 経由の永続化テスト"
                fmap toText rolledBack.folder `shouldBe` Just "warp/updated"

                -- 8. Soft delete, conflict on edit, restore, permanent delete (204)
                resTrash <- httpJsonWith manager Http.methodDelete docUrl aliceWrite Nothing
                Http.statusCode (HttpClient.responseStatus resTrash) `shouldBe` 200
                trashed <- decodeJson (HttpClient.responseBody resTrash) :: IO Document
                trashed.deletedAt `shouldSatisfy` isJust

                resEditTrashed <- httpJsonWith manager Http.methodPut docUrl aliceWrite (Just updateBody)
                Http.statusCode (HttpClient.responseStatus resEditTrashed) `shouldBe` 409
                editErr <- decodeJson (HttpClient.responseBody resEditTrashed) :: IO ApiError
                editErr.code `shouldBe` ErrConflict

                resRestore <- httpJsonWith manager Http.methodPost (docUrl <> "/restore") aliceWrite Nothing
                Http.statusCode (HttpClient.responseStatus resRestore) `shouldBe` 200
                restored <- decodeJson (HttpClient.responseBody resRestore) :: IO Document
                restored.deletedAt `shouldBe` Nothing

                resPermActive <- httpJsonWith manager Http.methodDelete (docUrl <> "/permanent") aliceWrite Nothing
                Http.statusCode (HttpClient.responseStatus resPermActive) `shouldBe` 409

                resTrashAgain <- httpJsonWith manager Http.methodDelete docUrl aliceWrite Nothing
                Http.statusCode (HttpClient.responseStatus resTrashAgain) `shouldBe` 200

                resPerm <- httpJsonWith manager Http.methodDelete (docUrl <> "/permanent") aliceWrite Nothing
                Http.statusCode (HttpClient.responseStatus resPerm) `shouldBe` 204
                HttpClient.responseBody resPerm `shouldBe` ""

                resGone <- httpJsonWith manager Http.methodGet docUrl aliceRead Nothing
                Http.statusCode (HttpClient.responseStatus resGone) `shouldBe` 404

                -- 9. Logout invalidates session, re-login issues a fresh session
                resLogout <- httpJsonWith manager Http.methodPost (baseUrl <> "/api/auth/logout") aliceWrite Nothing
                Http.statusCode (HttpClient.responseStatus resLogout) `shouldBe` 204

                resAfterLogout <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/auth/me") aliceRead Nothing
                Http.statusCode (HttpClient.responseStatus resAfterLogout) `shouldBe` 401

                let loginBody :: LazyBS.ByteString
                    loginBody = Aeson.encode $ Aeson.object
                        [ "username" Aeson..= ("warp_alice" :: Text)
                        , "password" Aeson..= ("alice-password-1" :: Text)
                        ]
                resLogin <- httpJsonWith manager Http.methodPost (baseUrl <> "/api/auth/login") [] (Just loginBody)
                Http.statusCode (HttpClient.responseStatus resLogin) `shouldBe` 200
                freshCookie <- extractHttpClientCookiePair resLogin
                resFreshMe <- httpJsonWith manager Http.methodGet (baseUrl <> "/api/auth/me") [(Http.hCookie, freshCookie)] Nothing
                Http.statusCode (HttpClient.responseStatus resFreshMe) `shouldBe` 200

            -- SQLite が WAL モードで永続化されていることを確認
            pragmas <- mustRight =<< withStorage storageCfg inspectStoragePragmas
            pragmas.journalMode `shouldBe` "wal"

cgiIntegrationSpec :: Spec
cgiIntegrationSpec = describe "CGI/1.1 handler end-to-end integration" $ do
    it "executes stateless multi-invocation auth, CSRF, owner isolation, and CRUD lifecycle via CGI/1.1 and SQLite" $
        withTempDbPath "cgi-e2e.sqlite3" $ \ dbPath -> do
            -- 環境変数経由の CGI 設定解決を再現するため境界で String へデコードする
            dbPathStr <- OS.decodeFS dbPath

            -- 1. Health check (public) & unauthenticated /api/documents -> 401
            resHealth <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "GET" "/api/health" "" [] Nothing
            resHealth.statusCode `shouldBe` 200
            healthVal <- decodeJson resHealth.body :: IO HealthStatus
            healthVal `shouldBe` healthOk

            resUnauth <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "GET" "/api/documents" "" [] Nothing
            resUnauth.statusCode `shouldBe` 401

            -- 2. Register Alice over HTTPS CGI (Secure cookie verified) & Bob over HTTP ScriptAlias /api
            let regAliceBody :: LazyBS.ByteString
                regAliceBody = Aeson.encode $ Aeson.object
                    [ "username"     Aeson..= ("cgi_alice" :: Text)
                    , "display_name" Aeson..= ("CGI アリス" :: Text)
                    , "password"     Aeson..= ("cgi-alice-pass-1" :: Text)
                    ]
            resRegAlice <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "POST" "/api/auth/register" "" [(envHttps, "on")] (Just regAliceBody)
            resRegAlice.statusCode `shouldBe` 201
            aliceAuth <- decodeJson resRegAlice.body :: IO AuthSessionResponse
            aliceSetCookie <- extractCgiSetCookie resRegAlice
            aliceSetCookie `shouldSatisfy` Txt.isInfixOf "HttpOnly"
            aliceSetCookie `shouldSatisfy` Txt.isInfixOf "SameSite=Lax"
            aliceSetCookie `shouldSatisfy` Txt.isInfixOf "; Secure"
            aliceCookieStr <- extractCgiCookiePairStr aliceSetCookie
            let aliceCsrfStr :: String
                aliceCsrfStr = Txt.unpack (toText aliceAuth.csrfToken)

                aliceReadEnv :: [(String, String)]
                aliceReadEnv = [("HTTP_COOKIE", aliceCookieStr)]

                aliceWriteEnv :: [(String, String)]
                aliceWriteEnv =
                    [ ("HTTP_COOKIE", aliceCookieStr)
                    , ("HTTP_X_CSRF_TOKEN", aliceCsrfStr)
                    ]

            let regBobBody :: LazyBS.ByteString
                regBobBody = Aeson.encode $ Aeson.object
                    [ "username" Aeson..= ("cgi_bob" :: Text)
                    , "password" Aeson..= ("cgi-bob-pass-1" :: Text)
                    ]
            resRegBob <- invokeCgi dbPathStr "/api" "POST" "/auth/register" "" [] (Just regBobBody)
            resRegBob.statusCode `shouldBe` 201
            bobAuth <- decodeJson resRegBob.body :: IO AuthSessionResponse
            bobSetCookie <- extractCgiSetCookie resRegBob
            bobSetCookie `shouldSatisfy` (not . Txt.isInfixOf "Secure")
            bobCookieStr <- extractCgiCookiePairStr bobSetCookie
            let bobCsrfStr :: String
                bobCsrfStr = Txt.unpack (toText bobAuth.csrfToken)

                bobReadEnv :: [(String, String)]
                bobReadEnv = [("HTTP_COOKIE", bobCookieStr)]

                bobWriteEnv :: [(String, String)]
                bobWriteEnv =
                    [ ("HTTP_COOKIE", bobCookieStr)
                    , ("HTTP_X_CSRF_TOKEN", bobCsrfStr)
                    ]

            -- 3. Stateless CGI session inspection (/api/auth/me)
            resMe <- invokeCgi dbPathStr "/api" "GET" "/auth/me" "" aliceReadEnv Nothing
            resMe.statusCode `shouldBe` 200
            meBody <- decodeJson resMe.body :: IO AuthSessionResponse
            meBody `shouldBe` aliceAuth

            -- 4. Missing CSRF token and bad Origin rejected with 403 in CGI
            resNoCsrf <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "POST" "/api/documents" "" aliceReadEnv (Just "{}")
            resNoCsrf.statusCode `shouldBe` 403

            resBadOrigin <- invokeCgi
                dbPathStr
                "/cgi-bin/tamatsuzura.cgi"
                "POST"
                "/api/documents"
                ""
                (("HTTP_HOST", "notes.example") : ("HTTP_ORIGIN", "https://evil.example") : aliceWriteEnv)
                (Just "{}")
            resBadOrigin.statusCode `shouldBe` 403

            -- 5. Create document as Alice via HTTPS same-origin CGI (independent invocation)
            let createBody :: LazyBS.ByteString
                createBody = Aeson.encode $ Aeson.object
                    [ "title"  Aeson..= ("CGI 疎通メモ" :: Text)
                    , "body"   Aeson..= ("レンタルサーバ CGI 互換テスト" :: Text)
                    , "format" Aeson..= ("markdown" :: Text)
                    , "folder" Aeson..= ("cgi/notes" :: Text)
                    , "tags"   Aeson..= (["cgi", "sqlite"] :: [Text])
                    ]
            resCreate <- invokeCgi
                dbPathStr
                "/cgi-bin/tamatsuzura.cgi"
                "POST"
                "/api/documents"
                ""
                ( (envHttps, "on")
                    : ("HTTP_HOST", "notes.example")
                    : ("HTTP_ORIGIN", "https://notes.example")
                    : ("HTTP_SEC_FETCH_SITE", "same-origin")
                    : aliceWriteEnv
                )
                (Just createBody)
            resCreate.statusCode `shouldBe` 201
            created <- decodeJson resCreate.body :: IO Document

            let idSegment :: String
                idSegment = Txt.unpack (toText created.id)

            -- 6. Bob is completely isolated from Alice's document, tags, and folders in CGI
            resBobGet <- invokeCgi dbPathStr "/api" "GET" ("/documents/" <> idSegment) "" bobReadEnv Nothing
            resBobGet.statusCode `shouldBe` 404

            resBobList <- invokeCgi dbPathStr "/api" "GET" "/documents" "status=all" bobReadEnv Nothing
            resBobList.statusCode `shouldBe` 200
            bobList <- decodeJson resBobList.body :: IO DocumentListResponse
            bobList.total `shouldBe` 0

            resBobTags <- invokeCgi dbPathStr "/api" "GET" "/tags" "" bobReadEnv Nothing
            bobTags <- decodeJson resBobTags.body :: IO TagListResponse
            bobTags.tags `shouldBe` []

            resBobDel <- invokeCgi dbPathStr "/api" "DELETE" ("/documents/" <> idSegment) "" bobWriteEnv Nothing
            resBobDel.statusCode `shouldBe` 404

            -- 7. Alice reads, updates, queries, trashes, restores, and permanently deletes via CGI
            resGet <- invokeCgi dbPathStr "/api" "GET" ("/documents/" <> idSegment) "" aliceReadEnv Nothing
            resGet.statusCode `shouldBe` 200
            fetched <- decodeJson resGet.body :: IO Document
            fetched `shouldBe` created

            let updateBody :: LazyBS.ByteString
                updateBody = Aeson.encode $ Aeson.object
                    [ "title"  Aeson..= ("CGI 更新済み" :: Text)
                    , "body"   Aeson..= ("CGI 経由で更新" :: Text)
                    , "format" Aeson..= ("plain_text" :: Text)
                    , "folder" Aeson..= ("cgi/archive" :: Text)
                    , "tags"   Aeson..= (["cgi"] :: [Text])
                    ]
            resUpdate <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "PUT" ("/api/documents/" <> idSegment) "" aliceWriteEnv (Just updateBody)
            resUpdate.statusCode `shouldBe` 200
            updated <- decodeJson resUpdate.body :: IO Document
            toText updated.title `shouldBe` "CGI 更新済み"

            resList <- invokeCgi dbPathStr "/api" "GET" "/documents" "tag=cgi&folder=cgi&include_subfolders=true&q=CGI" aliceReadEnv Nothing
            resList.statusCode `shouldBe` 200
            listed <- decodeJson resList.body :: IO DocumentListResponse
            listed.total `shouldBe` 1
            map (\ s -> s.id) listed.documents `shouldBe` [created.id]

            -- 7b. Revision history inspection, owner isolation, base_revision conflict, and rollback via CGI
            resBobRevs <- invokeCgi dbPathStr "/api" "GET" ("/documents/" <> idSegment <> "/revisions") "" bobReadEnv Nothing
            resBobRevs.statusCode `shouldBe` 404

            resCgiRevs <- invokeCgi dbPathStr "/api" "GET" ("/documents/" <> idSegment <> "/revisions") "" aliceReadEnv Nothing
            resCgiRevs.statusCode `shouldBe` 200
            cgiRevList <- decodeJson resCgiRevs.body :: IO RevisionListResponse
            cgiRevList.total `shouldBe` 2
            map (\ s -> revisionNumberValue s.revision) cgiRevList.revisions `shouldBe` [2, 1]

            resCgiRev1 <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "GET" ("/api/documents/" <> idSegment <> "/revisions/1") "" aliceReadEnv Nothing
            resCgiRev1.statusCode `shouldBe` 200
            cgiRev1 <- decodeJson resCgiRev1.body :: IO RevisionDetail
            toText cgiRev1.title `shouldBe` "CGI 疎通メモ"
            toText cgiRev1.body `shouldBe` "レンタルサーバ CGI 互換テスト"

            resCgiRollbackConflict <- invokeCgi dbPathStr "/api" "POST" ("/documents/" <> idSegment <> "/revisions/1/restore") "" aliceWriteEnv (Just "{\"base_revision\":1}")
            resCgiRollbackConflict.statusCode `shouldBe` 409

            resCgiRollback <- invokeCgi dbPathStr "/api" "POST" ("/documents/" <> idSegment <> "/revisions/1/restore") "" aliceWriteEnv (Just "{\"base_revision\":2}")
            resCgiRollback.statusCode `shouldBe` 200
            cgiRolledBack <- decodeJson resCgiRollback.body :: IO Document
            revisionNumberValue cgiRolledBack.revision `shouldBe` 3
            toText cgiRolledBack.title `shouldBe` "CGI 疎通メモ"
            toText cgiRolledBack.body `shouldBe` "レンタルサーバ CGI 互換テスト"
            fmap toText cgiRolledBack.folder `shouldBe` Just "cgi/archive"

            resTrash <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "DELETE" ("/api/documents/" <> idSegment) "" aliceWriteEnv Nothing
            resTrash.statusCode `shouldBe` 200
            trashed <- decodeJson resTrash.body :: IO Document
            trashed.deletedAt `shouldSatisfy` isJust

            resConflict <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "PUT" ("/api/documents/" <> idSegment) "" aliceWriteEnv (Just updateBody)
            resConflict.statusCode `shouldBe` 409
            conflictErr <- decodeJson resConflict.body :: IO ApiError
            conflictErr.code `shouldBe` ErrConflict

            resRestore <- invokeCgi dbPathStr "/api" "POST" ("/documents/" <> idSegment <> "/restore") "" aliceWriteEnv Nothing
            resRestore.statusCode `shouldBe` 200
            restored <- decodeJson resRestore.body :: IO Document
            restored.deletedAt `shouldBe` Nothing

            resTrash2 <- invokeCgi dbPathStr "/api" "DELETE" ("/documents/" <> idSegment) "" aliceWriteEnv Nothing
            resTrash2.statusCode `shouldBe` 200

            resPerm <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "DELETE" ("/api/documents/" <> idSegment <> "/permanent") "" aliceWriteEnv Nothing
            resPerm.statusCode `shouldBe` 204
            resPerm.body `shouldBe` ""

            resGone <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "GET" ("/api/documents/" <> idSegment) "" aliceReadEnv Nothing
            resGone.statusCode `shouldBe` 404
            goneErr <- decodeJson resGone.body :: IO ApiError
            goneErr.code `shouldBe` ErrNotFound

            res405 <- invokeCgi dbPathStr "/cgi-bin/tamatsuzura.cgi" "DELETE" "/api/documents" "" [] Nothing
            res405.statusCode `shouldBe` 405
            err405 <- decodeJson res405.body :: IO ApiError
            err405.code `shouldBe` ErrMethodNotAllowed

            -- 8. Logout invalidates SQLite session and re-login succeeds across CGI invocations
            resLogout <- invokeCgi dbPathStr "/api" "POST" "/auth/logout" "" ((envHttps, "on") : aliceWriteEnv) Nothing
            resLogout.statusCode `shouldBe` 204
            clearSetCookie <- extractCgiSetCookie resLogout
            clearSetCookie `shouldSatisfy` Txt.isInfixOf "Max-Age=0"
            clearSetCookie `shouldSatisfy` Txt.isInfixOf "; Secure"

            resAfterLogout <- invokeCgi dbPathStr "/api" "GET" "/auth/me" "" aliceReadEnv Nothing
            resAfterLogout.statusCode `shouldBe` 401

            let loginBody :: LazyBS.ByteString
                loginBody = Aeson.encode $ Aeson.object
                    [ "username" Aeson..= ("cgi_alice" :: Text)
                    , "password" Aeson..= ("cgi-alice-pass-1" :: Text)
                    ]
            resLogin <- invokeCgi dbPathStr "/api" "POST" "/auth/login" "" [] (Just loginBody)
            resLogin.statusCode `shouldBe` 200
            reloginSetCookie <- extractCgiSetCookie resLogin
            reloginCookieStr <- extractCgiCookiePairStr reloginSetCookie
            resReloginMe <- invokeCgi dbPathStr "/api" "GET" "/auth/me" "" [("HTTP_COOKIE", reloginCookieStr)] Nothing
            resReloginMe.statusCode `shouldBe` 200

            -- 9. Closed signup policy via TAMATSUZURA_ALLOW_REGISTRATION=false returns 403 in CGI
            let regClosedBody :: LazyBS.ByteString
                regClosedBody = Aeson.encode $ Aeson.object
                    [ "username" Aeson..= ("cgi_charlie" :: Text)
                    , "password" Aeson..= ("cgi-charlie-pass" :: Text)
                    ]
            resClosed <- invokeCgi dbPathStr "/api" "POST" "/auth/register" "" [(envAllowRegistration, "false")] (Just regClosedBody)
            resClosed.statusCode `shouldBe` 403
            closedErr <- decodeJson resClosed.body :: IO ApiError
            closedErr.code `shouldBe` ErrForbidden

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

warpRegister ::
       HttpClient.Manager
    -> String
    -> Text
    -> Text
    -> IO (SessionCreds, UserProfile)
warpRegister manager baseUrl uname pw = do
    let payload :: LazyBS.ByteString
        payload = Aeson.encode $ Aeson.object
            [ "username" Aeson..= uname
            , "password" Aeson..= pw
            ]
    res <- httpJsonWith manager Http.methodPost (baseUrl <> "/api/auth/register") [] (Just payload)
    Http.statusCode (HttpClient.responseStatus res) `shouldBe` 201
    authRes <- decodeJson (HttpClient.responseBody res) :: IO AuthSessionResponse
    cookiePair <- extractHttpClientCookiePair res
    pure
        ( SessionCreds
            { cookiePair = cookiePair
            , csrfToken  = TxtEnc.encodeUtf8 (toText authRes.csrfToken)
            }
        , authRes.user
        )

extractHttpClientCookiePair :: HttpClient.Response a -> IO ByteString
extractHttpClientCookiePair res =
    case lookup "Set-Cookie" (HttpClient.responseHeaders res) of
        Nothing -> do
            expectationFailure "Missing Set-Cookie header in Warp response"
            fail "Unreachable after expectationFailure"
        Just rawVal ->
            case BS8.split ';' rawVal of
                pair : _ -> pure pair
                []       -> do
                    expectationFailure "Malformed Set-Cookie header in Warp response"
                    fail "Unreachable after expectationFailure"

extractCgiSetCookie :: CgiResponse -> IO Text
extractCgiSetCookie res =
    case lookup "set-cookie" res.headers of
        Nothing -> do
            expectationFailure ("Missing Set-Cookie header in CGI response: " <> show res.headers)
            fail "Unreachable after expectationFailure"
        Just val -> pure val

extractCgiCookiePairStr :: Text -> IO String
extractCgiCookiePairStr setCookieVal =
    case Txt.splitOn ";" setCookieVal of
        pair : _ -> pure (Txt.unpack (Txt.strip pair))
        []       -> do
            expectationFailure ("Malformed Set-Cookie in CGI response: " <> Txt.unpack setCookieVal)
            fail "Unreachable after expectationFailure"

httpJsonWith ::
       HttpClient.Manager
    -> Http.Method
    -> String
    -> Http.RequestHeaders
    -> Maybe LazyBS.ByteString
    -> IO (HttpClient.Response LazyBS.ByteString)
httpJsonWith manager method url extraHeaders mBody = do
    initReq <- HttpClient.parseRequest url
    let req :: HttpClient.Request
        req = case mBody of
            Nothing -> initReq
                { HttpClient.method         = method
                , HttpClient.requestHeaders = (Http.hAccept, "application/json") : extraHeaders
                }
            Just bodyBytes -> initReq
                { HttpClient.method         = method
                , HttpClient.requestHeaders =
                    [ (Http.hContentType, "application/json")
                    , (Http.hAccept, "application/json")
                    ] <> extraHeaders
                , HttpClient.requestBody    = HttpClient.RequestBodyLBS bodyBytes
                }
    HttpClient.httpLbs req manager

invokeCgi ::
       String
    -> String
    -> String
    -> String
    -> String
    -> [(String, String)]
    -> Maybe LazyBS.ByteString
    -> IO CgiResponse
invokeCgi dbPathStr scriptName method pathInfo queryString extraEnv mBody = do
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
        envVars = extraEnv <> baseEnv <> bodyEnv

    rawCfg <- mustRight =<< resolveCgiConfig envVars
    let fastCfg :: CgiConfig
        fastCfg = rawCfg
            { auth = rawCfg.auth { passwordHashParams = fastTestPasswordHashParams }
            }
    runCgiApp envVars mBody (mkCgiApplication fastCfg)

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
