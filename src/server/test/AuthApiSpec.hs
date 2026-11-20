{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module AuthApiSpec
    ( authApiSpec
    ) where

import           Control.Concurrent                (forkIO, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import           Control.Exception                 (bracket, finally, try)
import qualified Data.Aeson                        as Aeson
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as BS8
import qualified Data.ByteString.Lazy              as LazyBS
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Txt
import qualified Data.Text.Encoding                as TxtEnc
import           Data.Time.Clock                   (addUTCTime, getCurrentTime)
import qualified Data.UUID                         as UUID
import qualified Data.UUID.V4                      as UUID4
import qualified Database.SQLite.Simple            as SQLite
import qualified Network.HTTP.Types                as Http
import qualified Network.Wai                       as Wai
import qualified Network.Wai.Test                  as WaiTest
import qualified System.Directory.OsPath           as DirOS
import           System.OsPath                     (OsPath)
import qualified System.OsPath                     as OS
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , DocumentListResponse(..)
    , FolderListResponse(..)
    , TagListResponse(..)
    )
import           TamaTsuzura.Common.Auth
    ( AuthSessionResponse(..)
    , UserProfile(..)
    , mkCsrfToken
    )
import           TamaTsuzura.Common.Document       (Document(..), DocumentSummary(..))
import           TamaTsuzura.Common.Text           (ToText(..))
import           TamaTsuzura.Server.App            (mkApplicationWithAuth)
import           TamaTsuzura.Server.Auth.Cookie
    ( AuthConfig(..)
    , CookieSecureMode(..)
    , RegistrationPolicy(..)
    , buildClearSessionCookieHeader
    , buildSessionCookieHeader
    , defaultAuthConfig
    , extractSessionTokenFromHeaders
    , parseCookieHeader
    , sessionCookieName
    )
import           TamaTsuzura.Server.Auth.Crypto
    ( fastTestPasswordHashParams
    , hashSessionToken
    , mkSessionToken
    )
import           TamaTsuzura.Server.Auth.Middleware
    ( csrfHeaderName
    , isRequestHttps
    , validateRequestOrigin
    )
import           TamaTsuzura.Server.Storage
    ( StorageConfig(..)
    , StorageTarget(..)
    , defaultStorageConfig
    , mkBusyTimeoutMs
    , withStorage
    )
import           TamaTsuzura.Server.Storage.Auth
    ( createSessionWith
    , defaultSessionTtlSeconds
    , mkSessionTtlSeconds
    )
import           Test.Hspec                        (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

data TestSessionCreds = TestSessionCreds
    { cookieHeader :: !ByteString
    , csrfToken    :: !ByteString
    , profile      :: !UserProfile
    } deriving (Eq, Show)

authApiSpec :: Spec
authApiSpec = do
    cookieUtilSpec
    originValidationSpec
    authLifecycleSpec
    csrfAndIsolationSpec
    walNonBlockingMeSpec

cookieUtilSpec :: Spec
cookieUtilSpec = describe "Session cookie parsing and Set-Cookie formatting" $ do
    it "parses RFC 6265 Cookie headers with multiple pairs, whitespace, quoted values, and multiple Cookie headers" $ do
        parseCookieHeader "foo=bar; tamatsuzura_session=tok123; theme=\"dark\"; invalid; =noval"
            `shouldBe`
                [ ("foo", "bar")
                , ("tamatsuzura_session", "tok123")
                , ("theme", "dark")
                ]
        extractSessionTokenFromHeaders
            [ (Http.hCookie, "a=1; b=2")
            , (Http.hCookie, "tamatsuzura_session=\"my_token_val\"")
            ] `shouldBe` Just "my_token_val"
        extractSessionTokenFromHeaders [(Http.hCookie, "other=value")] `shouldBe` Nothing

    it "formats Set-Cookie and clearing headers with HttpOnly, SameSite=Lax, Path=/, Max-Age, and Secure modes" $ do
        tok <- mustRight (mkSessionToken (Txt.replicate 43 "a"))
        ttl <- mustRight (mkSessionTtlSeconds 3600)

        let autoHttp :: Text
            autoHttp = buildSessionCookieHeader CookieSecureAuto False ttl tok

            autoHttps :: Text
            autoHttps = buildSessionCookieHeader CookieSecureAuto True ttl tok

            alwaysHttp :: Text
            alwaysHttp = buildSessionCookieHeader CookieSecureAlways False ttl tok

            neverHttps :: Text
            neverHttps = buildSessionCookieHeader CookieSecureNever True ttl tok

            clearHttps :: Text
            clearHttps = buildClearSessionCookieHeader CookieSecureAuto True

            clearHttp :: Text
            clearHttp = buildClearSessionCookieHeader CookieSecureAuto False

        autoHttp `shouldBe` ("tamatsuzura_session=" <> Txt.replicate 43 "a" <> "; Path=/; HttpOnly; SameSite=Lax; Max-Age=3600")
        autoHttps `shouldBe` (autoHttp <> "; Secure")
        alwaysHttp `shouldSatisfy` Txt.isSuffixOf "; Secure"
        neverHttps `shouldSatisfy` (not . Txt.isInfixOf "Secure")
        clearHttps `shouldBe` "tamatsuzura_session=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0; Expires=Thu, 01 Jan 1970 00:00:00 GMT; Secure"
        clearHttp `shouldBe` "tamatsuzura_session=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0; Expires=Thu, 01 Jan 1970 00:00:00 GMT"

originValidationSpec :: Spec
originValidationSpec = describe "Origin / Referer / Sec-Fetch-Site / Proxy HTTPS validation" $ do
    it "allows safe methods, absent Origin/Referer, and same-origin requests including default-port and IPv6 normalization" $ do
        let mkPost :: Bool -> Http.RequestHeaders -> Wai.Request
            mkPost isSec hdrs =
                (WaiTest.setPath WaiTest.defaultRequest "/api/documents")
                    { Wai.requestMethod  = Http.methodPost
                    , Wai.isSecure       = isSec
                    , Wai.requestHeaders = hdrs
                    }

            getCrossOrigin :: Wai.Request
            getCrossOrigin =
                (WaiTest.setPath WaiTest.defaultRequest "/api/documents")
                    { Wai.requestMethod  = Http.methodGet
                    , Wai.requestHeaders =
                        [ (Http.hHost, "notes.example.com")
                        , ("Origin", "https://evil.example")
                        , ("Sec-Fetch-Site", "cross-site")
                        ]
                    }

        validateRequestOrigin False getCrossOrigin `shouldBe` Right ()
        validateRequestOrigin False (mkPost False [(Http.hHost, "127.0.0.1:8080")]) `shouldBe` Right ()
        validateRequestOrigin False (mkPost False [(Http.hHost, "example.com"), ("Origin", "http://EXAMPLE.COM:80")]) `shouldBe` Right ()
        validateRequestOrigin True (mkPost True [(Http.hHost, "example.com:443"), ("Origin", "https://example.com")]) `shouldBe` Right ()
        validateRequestOrigin False (mkPost False [(Http.hHost, "[::1]:8080"), ("Origin", "http://[::1]:8080")]) `shouldBe` Right ()
        validateRequestOrigin False (mkPost False [(Http.hHost, "example.com"), (Http.hReferer, "http://example.com/editor?doc=1")]) `shouldBe` Right ()
        validateRequestOrigin True (mkPost False [(Http.hHost, "internal:8080"), ("X-Forwarded-Proto", "https, http"), ("X-Forwarded-Host", "notes.example.com"), ("Origin", "https://notes.example.com")]) `shouldBe` Right ()
        isRequestHttps True (mkPost False [("X-Forwarded-Proto", "https")]) `shouldBe` True
        isRequestHttps False (mkPost False [("X-Forwarded-Proto", "https")]) `shouldBe` False

    it "rejects mismatched Origin, opaque null Origin, mismatched Referer, cross-site Sec-Fetch-Site, and untrusted proxy headers" $ do
        let mkPost :: Bool -> Http.RequestHeaders -> Wai.Request
            mkPost isSec hdrs =
                (WaiTest.setPath WaiTest.defaultRequest "/api/documents")
                    { Wai.requestMethod  = Http.methodPost
                    , Wai.isSecure       = isSec
                    , Wai.requestHeaders = hdrs
                    }
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), ("Origin", "https://evil.example")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), ("Origin", "null")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), ("Origin", "https://notes.example.com")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com:8080"), ("Origin", "http://notes.example.com:9090")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [("Origin", "http://notes.example.com")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), (Http.hReferer, "http://evil.example/phish")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), ("Sec-Fetch-Site", "cross-site")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), ("Sec-Fetch-Site", "same-site")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), ("Sec-Fetch-Site", "same-origin"), ("Sec-Fetch-Site", "same-origin")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), ("Origin", "http://notes.example.com"), ("Origin", "http://notes.example.com")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), (Http.hReferer, "http://notes.example.com/a"), (Http.hReferer, "http://notes.example.com/b")]) `shouldSatisfy` isForbidden
        validateRequestOrigin False (mkPost False [(Http.hHost, "notes.example.com"), ("Sec-Fetch-Site", "same-origin")]) `shouldBe` Right ()
        validateRequestOrigin False (mkPost False [(Http.hHost, "internal:8080"), ("X-Forwarded-Proto", "https"), ("X-Forwarded-Host", "notes.example.com"), ("Origin", "https://notes.example.com")]) `shouldSatisfy` isForbidden

authLifecycleSpec :: Spec
authLifecycleSpec = describe "Auth endpoints (/api/auth/register, /login, /me, /logout)" $ do
    it "registers, validates inputs, rejects duplicate usernames, rotates sessions on login, inspects /me, and logs out" $
        withFastTestApp defaultFastAuthConfig $ \ app dbPath -> do
            -- 1. パスワード 8 文字未満は 400 (field="password")
            resShortPw <- sendRequest app Http.methodPost "/api/auth/register" [] (Just "{\"username\":\"alice\",\"password\":\"short77\"}")
            assertStatusCode 400 resShortPw
            shortPwErr <- decodeBody resShortPw :: IO ApiError
            shortPwErr.code `shouldBe` ErrValidation
            shortPwErr.field `shouldBe` Just "password"

            -- 2. 新規登録成功 (201 Created + Set-Cookie + Cache-Control: no-store)
            let regPayload :: LazyBS.ByteString
                regPayload = Aeson.encode $ Aeson.object
                    [ "username"     Aeson..= (" Alice_01 " :: Text)
                    , "display_name" Aeson..= ("アリス" :: Text)
                    , "password"     Aeson..= ("correct-horse-battery" :: Text)
                    ]
            resReg <- sendRequest app Http.methodPost "/api/auth/register" [] (Just regPayload)
            assertStatusCode 201 resReg
            assertNoStoreHeader resReg
            regBody <- decodeBody resReg :: IO AuthSessionResponse
            toText regBody.user.username `shouldBe` "alice_01"
            fmap toText regBody.user.displayName `shouldBe` Just "アリス"
            setCookieReg <- extractSetCookieHeader resReg
            setCookieReg `shouldSatisfy` Txt.isInfixOf "HttpOnly"
            setCookieReg `shouldSatisfy` Txt.isInfixOf "SameSite=Lax"
            setCookieReg `shouldSatisfy` Txt.isInfixOf "Path=/"
            setCookieReg `shouldSatisfy` (not . Txt.isInfixOf "Secure")
            cookie1 <- extractCookiePair setCookieReg

            -- 3. 重複ユーザー名 (大文字小文字違い) は 409 Conflict (field="username")
            resDup <- sendRequest app Http.methodPost "/api/auth/register" [] (Just "{\"username\":\"ALICE_01\",\"password\":\"another-valid-pass\"}")
            assertStatusCode 409 resDup
            dupErr <- decodeBody resDup :: IO ApiError
            dupErr.code `shouldBe` ErrConflict
            dupErr.field `shouldBe` Just "username"

            -- 4. GET /api/auth/me は有効 Cookie で 200 OK、未認証/不正 Cookie で 401
            resMe <- sendRequest app Http.methodGet "/api/auth/me" [(Http.hCookie, cookie1)] Nothing
            assertStatusCode 200 resMe
            assertNoStoreHeader resMe
            meBody <- decodeBody resMe :: IO AuthSessionResponse
            meBody `shouldBe` regBody

            resMeNoCookie <- sendRequest app Http.methodGet "/api/auth/me" [] Nothing
            assertStatusCode 401 resMeNoCookie

            resMeBadCookie <- sendRequest app Http.methodGet "/api/auth/me" [(Http.hCookie, "tamatsuzura_session=invalid-short")] Nothing
            assertStatusCode 401 resMeBadCookie

            -- 5. ログイン失敗 (パスワード不一致・未知ユーザー双方で同一形式の 401)
            resWrongPw <- sendRequest app Http.methodPost "/api/auth/login" [] (Just "{\"username\":\"alice_01\",\"password\":\"wrong-password\"}")
            assertStatusCode 401 resWrongPw
            wrongPwErr <- decodeBody resWrongPw :: IO ApiError

            resUnknownUser <- sendRequest app Http.methodPost "/api/auth/login" [] (Just "{\"username\":\"nobody_here\",\"password\":\"wrong-password\"}")
            assertStatusCode 401 resUnknownUser
            unknownErr <- decodeBody resUnknownUser :: IO ApiError
            wrongPwErr `shouldBe` unknownErr
            wrongPwErr.code `shouldBe` ErrUnauthorized

            -- 6. 再ログイン時は旧セッション Cookie が失効し新セッション・新 CSRF トークンが発行される
            resLogin <- sendRequest app Http.methodPost "/api/auth/login" [(Http.hCookie, cookie1)] (Just "{\"username\":\"ALICE_01\",\"password\":\"correct-horse-battery\"}")
            assertStatusCode 200 resLogin
            assertNoStoreHeader resLogin
            loginBody <- decodeBody resLogin :: IO AuthSessionResponse
            loginBody.user `shouldBe` regBody.user
            (loginBody.csrfToken /= regBody.csrfToken) `shouldBe` True
            setCookieLogin <- extractSetCookieHeader resLogin
            cookie2 <- extractCookiePair setCookieLogin
            (cookie2 /= cookie1) `shouldBe` True

            resOldMe <- sendRequest app Http.methodGet "/api/auth/me" [(Http.hCookie, cookie1)] Nothing
            assertStatusCode 401 resOldMe

            resNewMe <- sendRequest app Http.methodGet "/api/auth/me" [(Http.hCookie, cookie2)] Nothing
            assertStatusCode 200 resNewMe

            -- 7. ログアウトは CSRF 必須 (未添付 403)、正常時 204 + Cookie クリア + 以後 401
            resLogoutNoCsrf <- sendRequest app Http.methodPost "/api/auth/logout" [(Http.hCookie, cookie2)] Nothing
            assertStatusCode 403 resLogoutNoCsrf

            resLogoutNoAuth <- sendRequest app Http.methodPost "/api/auth/logout" [] Nothing
            assertStatusCode 401 resLogoutNoAuth

            resLogout <- sendRequest
                app
                Http.methodPost
                "/api/auth/logout"
                [ (Http.hCookie, cookie2)
                , (csrfHeaderName, TxtEnc.encodeUtf8 (toText loginBody.csrfToken))
                ]
                Nothing
            assertStatusCode 204 resLogout
            WaiTest.simpleBody resLogout `shouldBe` ""
            clearCookie <- extractSetCookieHeader resLogout
            clearCookie `shouldSatisfy` Txt.isInfixOf "Max-Age=0"
            clearCookie `shouldSatisfy` Txt.isInfixOf "tamatsuzura_session=;"

            resAfterLogout <- sendRequest app Http.methodGet "/api/auth/me" [(Http.hCookie, cookie2)] Nothing
            assertStatusCode 401 resAfterLogout

            -- 7b. HTTP/2 の複数 Cookie ヘッダ分割送信および非 UTF-8 兄弟 Cookie 混在時も
            --     ログイン・再登録の旧セッション失効および /me 検証が 400 にならず正しく機能する
            resLoginAgain <- sendRequest
                app
                Http.methodPost
                "/api/auth/login"
                [(Http.hCookie, "nonutf8=\xff\xfe")]
                (Just "{\"username\":\"alice_01\",\"password\":\"correct-horse-battery\"}")
            assertStatusCode 200 resLoginAgain
            cookie3 <- extractCookiePair =<< extractSetCookieHeader resLoginAgain

            resMeWithNonUtf8 <- sendRequest
                app
                Http.methodGet
                "/api/auth/me"
                [ (Http.hCookie, "nonutf8=\xff\xfe")
                , (Http.hCookie, cookie3)
                ]
                Nothing
            assertStatusCode 200 resMeWithNonUtf8

            resRotateSplit <- sendRequest
                app
                Http.methodPost
                "/api/auth/login"
                [ (Http.hCookie, "theme=dark")
                , (Http.hCookie, cookie3)
                ]
                (Just "{\"username\":\"alice_01\",\"password\":\"correct-horse-battery\"}")
            assertStatusCode 200 resRotateSplit
            cookie4 <- extractCookiePair =<< extractSetCookieHeader resRotateSplit

            resCookie3Revoked <- sendRequest app Http.methodGet "/api/auth/me" [(Http.hCookie, cookie3)] Nothing
            assertStatusCode 401 resCookie3Revoked

            -- ログイン済み状態からの別アカウント新規登録でも分割 Cookie 上の旧セッションが失効する
            resReRegSplit <- sendRequest
                app
                Http.methodPost
                "/api/auth/register"
                [ (Http.hCookie, "theme=dark")
                , (Http.hCookie, cookie4)
                ]
                (Just "{\"username\":\"alice_second\",\"password\":\"second-valid-pass\"}")
            assertStatusCode 201 resReRegSplit
            resCookie4Revoked <- sendRequest app Http.methodGet "/api/auth/me" [(Http.hCookie, cookie4)] Nothing
            assertStatusCode 401 resCookie4Revoked

            -- 8. 失効済みセッション (expires_at <= now) は GET /api/auth/me で 401 になる
            now <- getCurrentTime
            expiredTok <- mustRight (mkSessionToken (Txt.replicate 43 "z"))
            expiredCsrf <- mustRight (mkCsrfToken (Txt.replicate 43 "y"))
            let expiredCreated = addUTCTime (-7200) now
                expiredAt      = addUTCTime (-60) now
            -- sqlite-simple の withConnection には OsPath 版がないためテスト準備時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            uidRows <- SQLite.withConnection dbStr $ \ conn ->
                SQLite.query_ conn "SELECT id FROM users WHERE username = 'alice_01'" :: IO [SQLite.Only Text]
            case uidRows of
                [SQLite.Only rawUid] -> do
                    aliceId <- mustRight (Aeson.eitherDecode ("\"" <> LazyBS.fromStrict (TxtEnc.encodeUtf8 rawUid) <> "\""))
                    _ <- mustRight =<< withStorage (defaultStorageConfig dbPath) (\ h ->
                        createSessionWith h (hashSessionToken expiredTok) aliceId expiredCsrf expiredCreated expiredAt)
                    resExp <- sendRequest app Http.methodGet "/api/auth/me" [(Http.hCookie, sessionCookieName <> "=" <> BS8.replicate 43 'z')] Nothing
                    assertStatusCode 401 resExp
                _ -> expectationFailure "Expected alice_01 row in database"

    it "rejects registration with 403 Forbidden when RegistrationClosed policy is configured" $
        withFastTestApp defaultFastAuthConfig { registrationPolicy = RegistrationClosed } $ \ app _ -> do
            res <- sendRequest app Http.methodPost "/api/auth/register" [] (Just "{\"username\":\"closed_user\",\"password\":\"valid-password-123\"}")
            assertStatusCode 403 res
            err <- decodeBody res :: IO ApiError
            err.code `shouldBe` ErrForbidden

    it "sets Secure attribute on Set-Cookie according to CookieSecureMode and HTTPS/X-Forwarded-Proto" $
        withFastTestApp defaultFastAuthConfig { trustProxyHeaders = True } $ \ app _ -> do
            resProxyHttps <- sendRequest
                app
                Http.methodPost
                "/api/auth/register"
                [("X-Forwarded-Proto", "https")]
                (Just "{\"username\":\"https_user\",\"password\":\"valid-password-123\"}")
            assertStatusCode 201 resProxyHttps
            sc <- extractSetCookieHeader resProxyHttps
            sc `shouldSatisfy` Txt.isInfixOf "; Secure"

csrfAndIsolationSpec :: Spec
csrfAndIsolationSpec = describe "CSRF enforcement, Origin check, and multi-user API isolation" $ do
    it "enforces 401 unauthenticated, 403 missing/mismatched CSRF or bad Origin, and strict 404 owner isolation across all document endpoints" $
        withFastTestApp defaultFastAuthConfig $ \ app _ -> do
            -- 未認証アクセスは全文書・メタデータエンドポイントで 401
            resUnauthList <- sendRequest app Http.methodGet "/api/documents" [] Nothing
            assertStatusCode 401 resUnauthList
            resUnauthCreate <- sendRequest app Http.methodPost "/api/documents" [] (Just "{}")
            assertStatusCode 401 resUnauthCreate
            resUnauthTags <- sendRequest app Http.methodGet "/api/tags" [] Nothing
            assertStatusCode 401 resUnauthTags
            resUnauthFolders <- sendRequest app Http.methodGet "/api/folders" [] Nothing
            assertStatusCode 401 resUnauthFolders

            -- クロスオリジン Origin はログイン・登録を含め 403 で遮断
            resEvilReg <- sendRequest
                app
                Http.methodPost
                "/api/auth/register"
                [(Http.hHost, "app.example"), ("Origin", "https://evil.example")]
                (Just "{\"username\":\"evil_user\",\"password\":\"valid-password-123\"}")
            assertStatusCode 403 resEvilReg
            resEvilLogin <- sendRequest
                app
                Http.methodPost
                "/api/auth/login"
                [(Http.hHost, "app.example"), ("Origin", "https://evil.example")]
                (Just "{\"username\":\"evil_user\",\"password\":\"valid-password-123\"}")
            assertStatusCode 403 resEvilLogin

            -- Alice と Bob を登録
            alice <- registerUser app "alice" "password-alice-1"
            bob <- registerUser app "bob" "password-bob-1"

            -- 同一オリジン Origin + 正しい CSRF トークンで Alice が文書を作成できる
            resAliceCreate <- sendRequest
                app
                Http.methodPost
                "/api/documents"
                [ (Http.hCookie, alice.cookieHeader)
                , (csrfHeaderName, alice.csrfToken)
                , (Http.hHost, "app.example")
                , ("Origin", "http://app.example")
                ]
                (Just "{\"title\":\"Alice Secret\",\"body\":\"private\",\"folder\":\"alice/dir\",\"tags\":[\"secret\"]}")
            assertStatusCode 201 resAliceCreate
            aliceDoc <- decodeBody resAliceCreate :: IO Document
            let aliceDocPath :: ByteString
                aliceDocPath = "/api/documents/" <> TxtEnc.encodeUtf8 (toText aliceDoc.id)

            -- CSRF トークン未添付・他ユーザーの CSRF トークン・不正 Origin は全変更 API で 403
            let updateJson :: LazyBS.ByteString
                updateJson = "{\"title\":\"Edited\",\"body\":\"x\",\"format\":\"markdown\",\"folder\":null,\"tags\":[]}"

            resNoCsrfCreate <- sendRequest app Http.methodPost "/api/documents" [(Http.hCookie, alice.cookieHeader)] (Just "{}")
            assertStatusCode 403 resNoCsrfCreate

            resWrongCsrfCreate <- sendRequest app Http.methodPost "/api/documents" [(Http.hCookie, alice.cookieHeader), (csrfHeaderName, bob.csrfToken)] (Just "{}")
            assertStatusCode 403 resWrongCsrfCreate

            resNoCsrfPut <- sendRequest app Http.methodPut aliceDocPath [(Http.hCookie, alice.cookieHeader)] (Just updateJson)
            assertStatusCode 403 resNoCsrfPut

            resNoCsrfDel <- sendRequest app Http.methodDelete aliceDocPath [(Http.hCookie, alice.cookieHeader)] Nothing
            assertStatusCode 403 resNoCsrfDel

            resNoCsrfRestore <- sendRequest app Http.methodPost (aliceDocPath <> "/restore") [(Http.hCookie, alice.cookieHeader)] Nothing
            assertStatusCode 403 resNoCsrfRestore

            resNoCsrfPerm <- sendRequest app Http.methodDelete (aliceDocPath <> "/permanent") [(Http.hCookie, alice.cookieHeader)] Nothing
            assertStatusCode 403 resNoCsrfPerm

            resBadOriginDoc <- sendRequest
                app
                Http.methodPost
                "/api/documents"
                [ (Http.hCookie, alice.cookieHeader)
                , (csrfHeaderName, alice.csrfToken)
                , (Http.hHost, "app.example")
                , ("Origin", "https://attacker.example")
                ]
                (Just "{}")
            assertStatusCode 403 resBadOriginDoc

            -- Bob は自身の文書を作成でき、Alice の文書・タグ・フォルダは完全に不可視 (404)
            resBobCreate <- sendRequest
                app
                Http.methodPost
                "/api/documents"
                [(Http.hCookie, bob.cookieHeader), (csrfHeaderName, bob.csrfToken)]
                (Just "{\"title\":\"Bob Public\",\"body\":\"hi\",\"folder\":\"bob/dir\",\"tags\":[\"bobtag\"]}")
            assertStatusCode 201 resBobCreate
            bobDoc <- decodeBody resBobCreate :: IO Document

            resBobList <- sendRequest app Http.methodGet "/api/documents?status=all" [(Http.hCookie, bob.cookieHeader)] Nothing
            assertStatusCode 200 resBobList
            bobList <- decodeBody resBobList :: IO DocumentListResponse
            bobList.total `shouldBe` 1
            map (\ s -> s.id) bobList.documents `shouldBe` [bobDoc.id]

            resBobTags <- sendRequest app Http.methodGet "/api/tags" [(Http.hCookie, bob.cookieHeader)] Nothing
            assertStatusCode 200 resBobTags
            bobTags <- decodeBody resBobTags :: IO TagListResponse
            map toText bobTags.tags `shouldBe` ["bobtag"]

            resBobFolders <- sendRequest app Http.methodGet "/api/folders" [(Http.hCookie, bob.cookieHeader)] Nothing
            assertStatusCode 200 resBobFolders
            bobFolders <- decodeBody resBobFolders :: IO FolderListResponse
            map toText bobFolders.folders `shouldBe` ["bob/dir"]

            -- Bob が Alice の文書 ID へアクセス・変更を試みてもすべて 404 Not Found
            resBobGetAlice <- sendRequest app Http.methodGet aliceDocPath [(Http.hCookie, bob.cookieHeader)] Nothing
            assertStatusCode 404 resBobGetAlice

            resBobPutAlice <- sendRequest app Http.methodPut aliceDocPath [(Http.hCookie, bob.cookieHeader), (csrfHeaderName, bob.csrfToken)] (Just updateJson)
            assertStatusCode 404 resBobPutAlice

            resBobDelAlice <- sendRequest app Http.methodDelete aliceDocPath [(Http.hCookie, bob.cookieHeader), (csrfHeaderName, bob.csrfToken)] Nothing
            assertStatusCode 404 resBobDelAlice

            resBobRestoreAlice <- sendRequest app Http.methodPost (aliceDocPath <> "/restore") [(Http.hCookie, bob.cookieHeader), (csrfHeaderName, bob.csrfToken)] Nothing
            assertStatusCode 404 resBobRestoreAlice

            resBobPermAlice <- sendRequest app Http.methodDelete (aliceDocPath <> "/permanent") [(Http.hCookie, bob.cookieHeader), (csrfHeaderName, bob.csrfToken)] Nothing
            assertStatusCode 404 resBobPermAlice

            -- Alice がゴミ箱へ移動しても Bob の PUT は 409 ではなく 404 のまま (状態漏洩なし)
            resAliceTrash <- sendRequest app Http.methodDelete aliceDocPath [(Http.hCookie, alice.cookieHeader), (csrfHeaderName, alice.csrfToken)] Nothing
            assertStatusCode 200 resAliceTrash

            resBobPutAliceTrashed <- sendRequest app Http.methodPut aliceDocPath [(Http.hCookie, bob.cookieHeader), (csrfHeaderName, bob.csrfToken)] (Just updateJson)
            assertStatusCode 404 resBobPutAliceTrashed

walNonBlockingMeSpec :: Spec
walNonBlockingMeSpec = describe "CGI/WAL non-blocking read on authenticated GET /api/auth/me" $ do
    it "serves GET /api/auth/me with busy_timeout=0 during an active SQLite write lock" $
        withTempDbPath "wal-me.sqlite3" $ \ dbPath -> do
            let baseCfg :: StorageConfig
                baseCfg = defaultStorageConfig dbPath

                app :: Wai.Application
                app = mkApplicationWithAuth defaultFastAuthConfig baseCfg
            alice <- registerUser app "wal_alice" "valid-password-123"

            zeroTimeout <- mustRight (mkBusyTimeoutMs 0)
            let noWaitCfg :: StorageConfig
                noWaitCfg = StorageConfig
                    { target        = StorageFile dbPath
                    , busyTimeoutMs = zeroTimeout
                    }

                noWaitApp :: Wai.Application
                noWaitApp = mkApplicationWithAuth defaultFastAuthConfig noWaitCfg

            -- sqlite-simple の withConnection には OsPath 版がないためロック検証時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            lockAcquired <- newEmptyMVar
            releaseSignal <- newEmptyMVar
            lockDone <- newEmptyMVar
            _ <- forkIO $
                SQLite.withConnection dbStr
                    ( \ conn -> do
                        SQLite.execute_ conn "BEGIN IMMEDIATE"
                        putMVar lockAcquired (Right ())
                        takeMVar releaseSignal
                        _ <- try (SQLite.execute_ conn "ROLLBACK") :: IO (Either SQLite.SQLError ())
                        pure ()
                    )
                    `finally` ( do
                        _ <- tryPutMVar lockAcquired (Left ("Lock thread exited early" :: Text))
                        _ <- tryPutMVar lockDone ()
                        pure ()
                    )
            _ <- mustRight =<< takeMVar lockAcquired
            resMe <- sendRequest noWaitApp Http.methodGet "/api/auth/me" [(Http.hCookie, alice.cookieHeader)] Nothing
            putMVar releaseSignal ()
            takeMVar lockDone
            assertStatusCode 200 resMe
            meBody <- decodeBody resMe :: IO AuthSessionResponse
            meBody.user `shouldBe` alice.profile

defaultFastAuthConfig :: AuthConfig
defaultFastAuthConfig = defaultAuthConfig
    { passwordHashParams = fastTestPasswordHashParams
    , sessionTtl         = defaultSessionTtlSeconds
    }

withFastTestApp :: AuthConfig -> (Wai.Application -> OsPath -> IO a) -> IO a
withFastTestApp authCfg action =
    withTempDbPath "auth-api.sqlite3" $ \ dbPath ->
        action (mkApplicationWithAuth authCfg (defaultStorageConfig dbPath)) dbPath

registerUser :: Wai.Application -> Text -> Text -> IO TestSessionCreds
registerUser app uname pw = do
    let payload :: LazyBS.ByteString
        payload = Aeson.encode $ Aeson.object
            [ "username" Aeson..= uname
            , "password" Aeson..= pw
            ]
    res <- sendRequest app Http.methodPost "/api/auth/register" [] (Just payload)
    assertStatusCode 201 res
    body <- decodeBody res :: IO AuthSessionResponse
    setCookieVal <- extractSetCookieHeader res
    cookiePair <- extractCookiePair setCookieVal
    pure TestSessionCreds
        { cookieHeader = cookiePair
        , csrfToken    = TxtEnc.encodeUtf8 (toText body.csrfToken)
        , profile      = body.user
        }

-- | WaiTest.Session の暗黙 Cookie Jar によるヘッダ上書きを避けるため、
-- 1 リクエストごとに独立したセッションで実行して明示的なヘッダ挙動を厳密に検証する。
sendRequest ::
       Wai.Application
    -> Http.Method
    -> ByteString
    -> Http.RequestHeaders
    -> Maybe LazyBS.ByteString
    -> IO WaiTest.SResponse
sendRequest app method rawPath extraHeaders mBody =
    let baseHeaders :: Http.RequestHeaders
        baseHeaders = case mBody of
            Nothing -> [(Http.hAccept, "application/json")]
            Just _  ->
                [ (Http.hContentType, "application/json")
                , (Http.hAccept, "application/json")
                ]

        bodyBytes :: LazyBS.ByteString
        bodyBytes = fromMaybe "" mBody

        req :: Wai.Request
        req =
            (WaiTest.setPath WaiTest.defaultRequest rawPath)
                { Wai.requestMethod  = method
                , Wai.requestHeaders = baseHeaders <> extraHeaders
                }
    in  WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest req bodyBytes)) app

assertStatusCode :: Int -> WaiTest.SResponse -> IO ()
assertStatusCode expected res =
    Http.statusCode (WaiTest.simpleStatus res) `shouldBe` expected

assertNoStoreHeader :: WaiTest.SResponse -> IO ()
assertNoStoreHeader res =
    case lookup Http.hCacheControl (WaiTest.simpleHeaders res) of
        Just val | "no-store" `Txt.isInfixOf` Txt.toLower (TxtEnc.decodeUtf8Lenient val) -> pure ()
        other -> expectationFailure ("Expected Cache-Control: no-store, got: " <> show other)

extractSetCookieHeader :: WaiTest.SResponse -> IO Text
extractSetCookieHeader res =
    case lookup "Set-Cookie" (WaiTest.simpleHeaders res) of
        Just raw -> pure (TxtEnc.decodeUtf8Lenient raw)
        Nothing  -> do
            expectationFailure "Missing Set-Cookie header in response"
            fail "Unreachable after expectationFailure"

extractCookiePair :: Text -> IO ByteString
extractCookiePair setCookieVal =
    case Txt.splitOn ";" setCookieVal of
        pair : _ -> pure (TxtEnc.encodeUtf8 (Txt.strip pair))
        []       -> do
            expectationFailure ("Malformed Set-Cookie header: " <> Txt.unpack setCookieVal)
            fail "Unreachable after expectationFailure"

decodeBody :: Aeson.FromJSON a => WaiTest.SResponse -> IO a
decodeBody res =
    case Aeson.eitherDecode (WaiTest.simpleBody res) of
        Left err -> do
            expectationFailure ("Failed to decode JSON body: " <> err <> " (body=" <> show (WaiTest.simpleBody res) <> ")")
            fail "Unreachable after expectationFailure"
        Right val -> pure val

isForbidden :: Either ApiError () -> Bool
isForbidden (Left err) = err.code == ErrForbidden
isForbidden (Right ()) = False

withTempDbPath :: String -> (OsPath -> IO a) -> IO a
withTempDbPath relFileName action = do
    tmpRoot <- DirOS.getTemporaryDirectory
    rawUuid <- UUID4.nextRandom
    -- UUID 文字列およびテスト用ファイル名から OsPath を生成するため encodeFS を使用する
    dirPart <- OS.encodeFS ("tamatsuzura-auth-api-test-" <> UUID.toString rawUuid)
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
