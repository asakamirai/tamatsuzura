{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module AuthStorageSpec
    ( authStorageSpec
    ) where

import           Control.Concurrent                (forkIO, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import           Control.Exception                 (bracket, finally, try)
import           Data.Either                       (isLeft)
import           Data.Maybe                        (isJust)
import           Data.Text                         (Text)
import qualified Data.Text                         as Txt
import           Data.Time.Calendar                (fromGregorian)
import           Data.Time.Clock                   (UTCTime(..), addUTCTime, picosecondsToDiffTime)
import qualified Data.UUID                         as UUID
import qualified Data.UUID.V4                      as UUID4
import           Database.SQLite.Simple            (Only(..))
import qualified Database.SQLite.Simple            as SQLite
import qualified System.Directory.OsPath           as DirOS
import           System.OsPath                     (OsPath)
import qualified System.OsPath                     as OS
import           TamaTsuzura.Common.Auth
    ( UserProfile(..)
    , mkCsrfToken
    , mkDisplayName
    , mkPassword
    , mkUserId
    , mkUsername
    )
import           TamaTsuzura.Common.Text           (ToText(..))
import           TamaTsuzura.Server.Auth.Crypto
    ( fastTestPasswordHashParams
    , hashPasswordWithSalt
    , hashSessionToken
    , verifyPassword
    )
import           TamaTsuzura.Server.Storage
    ( StorageConfig(..)
    , StorageError(..)
    , StorageHandle
    , StorageTarget(..)
    , defaultStorageConfig
    , inMemoryStorageConfig
    , mkBusyTimeoutMs
    , withStorage
    )
import           TamaTsuzura.Server.Storage.Auth
    ( AuthenticatedSession(..)
    , StoredSession(..)
    , StoredUser(..)
    , createSession
    , createSessionWith
    , createUser
    , createUserWith
    , defaultSessionTtlSeconds
    , deleteSession
    , deleteSessionByHash
    , deleteUserSessions
    , getUserById
    , getUserByUsername
    , lookupValidSession
    , lookupValidSessionAt
    , maxSessionTtlSeconds
    , minSessionTtlSeconds
    , mkSessionTtlSeconds
    , purgeExpiredSessions
    , purgeExpiredSessionsAt
    , sessionTtlSecondsValue
    , touchSessionAt
    )
import           Test.Hspec                        (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

authStorageSpec :: Spec
authStorageSpec = describe "SQLite user and session persistence" $ do
    it "validates SessionTtlSeconds boundaries" $ do
        fmap sessionTtlSecondsValue (mkSessionTtlSeconds minSessionTtlSeconds) `shouldBe` Right minSessionTtlSeconds
        fmap sessionTtlSecondsValue (mkSessionTtlSeconds maxSessionTtlSeconds) `shouldBe` Right maxSessionTtlSeconds
        sessionTtlSecondsValue defaultSessionTtlSeconds `shouldBe` (7 * 24 * 60 * 60)
        mkSessionTtlSeconds (minSessionTtlSeconds - 1) `shouldSatisfy` isLeft
        mkSessionTtlSeconds (maxSessionTtlSeconds + 1) `shouldSatisfy` isLeft

    it "creates, retrieves, and persists users across reopens with case-insensitive username uniqueness" $
        withTempDbPath "auth-users.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            uid1 <- mustRight (mkUserId "user-1")
            uid2 <- mustRight (mkUserId "user-2")
            unameAlice <- mustRight (mkUsername "Alice_01")
            unameAliceLower <- mustRight (mkUsername "alice_01")
            unameBob <- mustRight (mkUsername "bob")
            dnameAlice <- mustRight (mkDisplayName "アリス")
            pw <- mustRight (mkPassword "password-12345")
            pwHash <- mustRight (hashPasswordWithSalt fastTestPasswordHashParams "saltsaltsaltsalt" pw)
            let t0 :: UTCTime
                t0 = utcAt 2026 11 20 8 0 0 123456

            createdAlice <- mustRight =<< withStorage cfg (\ h ->
                createUserWith h uid1 t0 unameAlice (Just dnameAlice) pwHash)
            createdAlice.profile `shouldBe` UserProfile
                { id          = uid1
                , username    = unameAliceLower
                , displayName = Just dnameAlice
                , createdAt   = t0
                }
            verifyPassword pw createdAlice.passwordHash `shouldBe` True

            -- 再オープン後に ID・ユーザー名双方で取得できる
            fetchedById <- mustRight =<< withStorage cfg (`getUserById` uid1)
            fetchedById `shouldBe` Just createdAlice
            fetchedByName <- mustRight =<< withStorage cfg (`getUserByUsername` unameAliceLower)
            fetchedByName `shouldBe` Just createdAlice

            -- 未存在ユーザーは Nothing
            missingById <- mustRight =<< withStorage cfg (`getUserById` uid2)
            missingById `shouldBe` Nothing
            missingByName <- mustRight =<< withStorage cfg (`getUserByUsername` unameBob)
            missingByName `shouldBe` Nothing

            -- 重複 ID と重複ユーザー名 (大文字小文字正規化後) はそれぞれ専用エラーで拒否する
            dupIdRes <- withStorage cfg (\ h -> createUserWith h uid1 t0 unameBob Nothing pwHash)
            dupIdRes `shouldBe` Left (UserIdAlreadyExists uid1)
            dupNameRes <- withStorage cfg (\ h -> createUserWith h uid2 t0 unameAliceLower Nothing pwHash)
            dupNameRes `shouldBe` Left (UsernameAlreadyExists unameAliceLower)

            -- display_name = Nothing かつ UUIDv4 自動発行の createUser も正常動作する
            createdBob <- mustRight =<< withStorage cfg (\ h -> createUser h unameBob Nothing pwHash)
            createdBob.profile.username `shouldBe` unameBob
            createdBob.profile.displayName `shouldBe` Nothing

    it "manages SQLite sessions across create, lookup, expiry boundary, touch, logout, user revocation, purge, and user cascade" $
        withTempDbPath "auth-sessions.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            uid <- mustRight (mkUserId "user-sess")
            missingUid <- mustRight (mkUserId "user-missing")
            uname <- mustRight (mkUsername "session_user")
            pw <- mustRight (mkPassword "password-12345")
            pwHash <- mustRight (hashPasswordWithSalt fastTestPasswordHashParams "1234567890abcdef" pw)
            let t0 :: UTCTime
                t0 = utcAt 2026 11 20 10 0 0 0
                tMid :: UTCTime
                tMid = utcAt 2026 11 20 10 30 0 0
                tExp :: UTCTime
                tExp = utcAt 2026 11 20 11 0 0 0
                tExtended :: UTCTime
                tExtended = utcAt 2026 11 20 12 0 0 0
                tAfter :: UTCTime
                tAfter = utcAt 2026 11 20 12 0 0 1

            user <- mustRight =<< withStorage cfg (\ h -> createUserWith h uid t0 uname Nothing pwHash)

            -- 未存在ユーザーへのセッション作成は UserNotFound
            csrf1 <- mustRight (mkCsrfToken (Txt.replicate 43 "a"))
            csrf2 <- mustRight (mkCsrfToken (Txt.replicate 43 "b"))
            (rawTok1, _) <- mustRight =<< withStorage cfg (\ h -> createSession h uid defaultSessionTtlSeconds)
            let tokHash1 = hashSessionToken rawTok1

            eMissingUser <- withStorage cfg (\ h -> createSessionWith h tokHash1 missingUid csrf1 t0 tExp)
            eMissingUser `shouldBe` Left (UserNotFound missingUid)

            -- 現在時刻で作成されたセッションが lookupValidSession で取得でき、deleteSession が冪等に動作する
            mAuthLive <- mustRight =<< withStorage cfg (`lookupValidSession` rawTok1)
            mAuthLive `shouldSatisfy` isJust
            () <- mustRight =<< withStorage cfg (`deleteSession` rawTok1)
            () <- mustRight =<< withStorage cfg (`deleteSession` rawTok1)
            mAuthAfterLogout <- mustRight =<< withStorage cfg (`lookupValidSession` rawTok1)
            mAuthAfterLogout `shouldBe` Nothing

            -- 決定的な有効期限境界テスト: expires_at > now は有効、expires_at == now は失効
            _ <- mustRight =<< withStorage cfg (\ h -> createSessionWith h tokHash1 uid csrf1 t0 tExp)
            mValid <- mustRight =<< withStorage cfg (\ h -> lookupValidSessionAt h tokHash1 tMid)
            case mValid of
                Nothing -> expectationFailure "Expected valid session before expiry"
                Just authSess -> do
                    authSess.user `shouldBe` user.profile
                    authSess.session.csrfToken `shouldBe` csrf1
                    authSess.session.expiresAt `shouldBe` tExp

            mAtBoundary <- mustRight =<< withStorage cfg (\ h -> lookupValidSessionAt h tokHash1 tExp)
            mAtBoundary `shouldBe` Nothing

            -- touchSessionAt で mNewExpiresAt = Nothing の場合は expires_at を維持し last_accessed_at のみ更新する
            let tTouchOnly :: UTCTime
                tTouchOnly = utcAt 2026 11 20 10 15 0 0
            touchedOnly <- mustRight =<< withStorage cfg (\ h -> touchSessionAt h tokHash1 tTouchOnly Nothing)
            touchedOnly `shouldBe` True
            mAfterTouchOnly <- mustRight =<< withStorage cfg (\ h -> lookupValidSessionAt h tokHash1 tTouchOnly)
            case mAfterTouchOnly of
                Nothing -> expectationFailure "Expected session to remain valid after touch-only update"
                Just authSess -> do
                    authSess.session.lastAccessedAt `shouldBe` tTouchOnly
                    authSess.session.expiresAt `shouldBe` tExp

            -- touchSessionAt で最終アクセス時刻とスライディング失効時刻を更新できる
            touched <- mustRight =<< withStorage cfg (\ h -> touchSessionAt h tokHash1 tMid (Just tExtended))
            touched `shouldBe` True
            mExtended <- mustRight =<< withStorage cfg (\ h -> lookupValidSessionAt h tokHash1 tExp)
            case mExtended of
                Nothing -> expectationFailure "Expected session to remain valid after sliding expiry extension"
                Just authSess -> do
                    authSess.session.lastAccessedAt `shouldBe` tMid
                    authSess.session.expiresAt `shouldBe` tExtended

            -- 失効済みセッションの touch は False
            touchedExpired <- mustRight =<< withStorage cfg (\ h -> touchSessionAt h tokHash1 tAfter Nothing)
            touchedExpired `shouldBe` False

            -- 失効済みセッション一括削除の厳密境界 (expires_at == now は削除、expires_at > now は残存)
            (rawTok2, _) <- mustRight =<< withStorage cfg (\ h -> createSession h uid defaultSessionTtlSeconds)
            let tokHash2 = hashSessionToken rawTok2
            () <- mustRight =<< withStorage cfg (`deleteSessionByHash` tokHash2)
            _ <- mustRight =<< withStorage cfg (\ h -> createSessionWith h tokHash2 uid csrf2 t0 (addUTCTime 7200 tExtended))
            purgedCount <- mustRight =<< withStorage cfg (`purgeExpiredSessionsAt` tExtended)
            purgedCount `shouldBe` 1
            mSurviving <- mustRight =<< withStorage cfg (\ h -> lookupValidSessionAt h tokHash2 tExtended)
            mSurviving `shouldSatisfy` isJust

            -- ユーザー単位の一括失効 (deleteUserSessions) と purgeExpiredSessions ラッパー
            revokedCount <- mustRight =<< withStorage cfg (`deleteUserSessions` uid)
            revokedCount `shouldBe` 1
            purgedNow <- mustRight =<< withStorage cfg purgeExpiredSessions
            purgedNow `shouldBe` 0

            -- ユーザー削除時の ON DELETE CASCADE でセッションも削除される
            (rawTok3, _) <- mustRight =<< withStorage cfg (\ h -> createSession h uid defaultSessionTtlSeconds)
            -- sqlite-simple の withConnection には OsPath 版がないため検証時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            SQLite.withConnection dbStr $ \ conn -> do
                SQLite.execute_ conn "PRAGMA foreign_keys = ON"
                SQLite.execute conn "DELETE FROM users WHERE id = ?" (Only (toText uid))
                sessCount <- SQLite.query_ conn "SELECT COUNT(*) FROM sessions" :: IO [Only Int]
                sessCount `shouldBe` [Only 0]
            mAfterCascade <- mustRight =<< withStorage cfg (`lookupValidSession` rawTok3)
            mAfterCascade `shouldBe` Nothing

    it "allows non-blocking WAL session lookups during active write locks (CGI read concurrency)" $
        withTempDbPath "auth-wal-read.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            uname <- mustRight (mkUsername "wal_reader")
            pw <- mustRight (mkPassword "password-12345")
            pwHash <- mustRight (hashPasswordWithSalt fastTestPasswordHashParams "1234567890abcdef" pw)
            user <- mustRight =<< withStorage cfg (\ h -> createUser h uname Nothing pwHash)
            (rawToken, _) <- mustRight =<< withStorage cfg (\ h -> createSession h user.profile.id defaultSessionTtlSeconds)

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
            zeroTimeout <- mustRight (mkBusyTimeoutMs 0)
            let noWaitCfg :: StorageConfig
                noWaitCfg = StorageConfig
                    { target        = StorageFile dbPath
                    , busyTimeoutMs = zeroTimeout
                    }
            eLookup <- withStorage noWaitCfg (`lookupValidSession` rawToken)
            putMVar releaseSignal ()
            takeMVar lockDone
            case eLookup of
                Right (Just authSess) -> authSess.user `shouldBe` user.profile
                other                 -> expectationFailure ("Expected non-blocking WAL session lookup, got: " <> show other)

    it "returns CorruptedData when stored user or session columns violate domain invariants" $
        withTempDbPath "auth-corrupt.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            uid <- mustRight (mkUserId "user-corrupt")
            uname <- mustRight (mkUsername "corrupt_user")
            pw <- mustRight (mkPassword "password-12345")
            pwHash <- mustRight (hashPasswordWithSalt fastTestPasswordHashParams "1234567890abcdef" pw)
            _ <- mustRight =<< withStorage cfg (\ h -> createUserWith h uid (utcAt 2026 11 20 8 0 0 0) uname Nothing pwHash)
            (rawToken, _) <- mustRight =<< withStorage cfg (\ h -> createSession h uid defaultSessionTtlSeconds)

            -- sqlite-simple の withConnection には OsPath 版がないため破損データ注入時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            SQLite.withConnection dbStr $ \ conn -> do
                -- users.username の CHECK 制約により非正規 (大文字混在) 挿入は DB レベルで拒否される
                eCheckViolation <-
                    try
                        ( SQLite.execute
                            conn
                            "UPDATE users SET username = 'Corrupt_User' WHERE id = ?"
                            (Only (toText uid))
                        ) :: IO (Either SQLite.SQLError ())
                eCheckViolation `shouldSatisfy` isLeft

                SQLite.execute conn "UPDATE users SET password_hash = 'not-a-phc-hash' WHERE id = ?" (Only (toText uid))
                SQLite.execute conn "UPDATE sessions SET csrf_token = 'bad' WHERE user_id = ?" (Only (toText uid))
            eUser <- withStorage cfg (`getUserById` uid)
            eUser `shouldSatisfy` isCorruptedData
            eSess <- withStorage cfg (`lookupValidSession` rawToken)
            eSess `shouldSatisfy` isCorruptedData

            -- CHECK 制約を回避して非正規 username が混入した場合でも、Haskell 側デコーダが CorruptedData で検出する
            withTempDbPath "auth-noncanonical-username.sqlite3" $ \ dbPath2 -> do
                let cfg2 :: StorageConfig
                    cfg2 = defaultStorageConfig dbPath2
                _ <- mustRight =<< withStorage cfg2 (\ h -> createUserWith h uid (utcAt 2026 11 20 8 0 0 0) uname Nothing pwHash)
                dbStr2 <- OS.decodeFS dbPath2
                SQLite.withConnection dbStr2 $ \ conn -> do
                    SQLite.execute_ conn "PRAGMA ignore_check_constraints = ON"
                    SQLite.execute conn "UPDATE users SET username = 'Corrupt_User' WHERE id = ?" (Only (toText uid))
                eNonCanon <- withStorage cfg2 (`getUserById` uid)
                eNonCanon `shouldSatisfy` isCorruptedData

    it "rejects legacy schema v1 databases with UnsupportedSchemaVersion" $
        withTempDbPath "legacy-v1.sqlite3" $ \ dbPath -> do
            -- sqlite-simple の withConnection には OsPath 版がないためテスト準備時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            SQLite.withConnection dbStr $ \ conn ->
                SQLite.execute_ conn "PRAGMA user_version = 1"
            anyUid <- mustRight (mkUserId "any")
            eRes <- withStorage (defaultStorageConfig dbPath) (`getUserById` anyUid)
            eRes `shouldBe` Left (UnsupportedSchemaVersion 1 2)

    it "rejects invalid session expiry timestamps on createSessionWith and touchSessionAt" $
        withInMemory $ \ storage -> do
            uid <- mustRight (mkUserId "user-ts")
            uname <- mustRight (mkUsername "time_user")
            pw <- mustRight (mkPassword "password-12345")
            pwHash <- mustRight (hashPasswordWithSalt fastTestPasswordHashParams "1234567890abcdef" pw)
            let t0 :: UTCTime
                t0 = utcAt 2026 11 20 8 0 0 0
            _ <- mustRight =<< createUserWith storage uid t0 uname Nothing pwHash
            (rawToken, _) <- mustRight =<< createSession storage uid defaultSessionTtlSeconds
            csrf <- mustRight (mkCsrfToken (Txt.replicate 43 "c"))
            eBadCreate <- createSessionWith storage (hashSessionToken rawToken) uid csrf t0 t0
            eBadCreate `shouldSatisfy` isCorruptedData
            eBadTouch <- touchSessionAt storage (hashSessionToken rawToken) t0 (Just t0)
            eBadTouch `shouldSatisfy` isCorruptedData

withInMemory :: (StorageHandle -> IO ()) -> IO ()
withInMemory testBody = do
    eRes <- withStorage inMemoryStorageConfig (fmap Right . testBody)
    _ <- mustRight eRes
    pure ()

withTempDbPath :: String -> (OsPath -> IO a) -> IO a
withTempDbPath relFileName action = do
    tmpRoot <- DirOS.getTemporaryDirectory
    rawUuid <- UUID4.nextRandom
    -- UUID 文字列および相対テストファイル名から OsPath を生成するため encodeFS を使用する
    dirPart <- OS.encodeFS ("tamatsuzura-auth-storage-test-" <> UUID.toString rawUuid)
    filePart <- OS.encodeFS relFileName
    let testDir :: OsPath
        testDir = tmpRoot OS.</> dirPart

        dbPath :: OsPath
        dbPath = testDir OS.</> filePart
    bracket
        (DirOS.createDirectoryIfMissing True testDir >> pure dbPath)
        (const (DirOS.removePathForcibly testDir))
        action

utcAt :: Integer -> Int -> Int -> Int -> Int -> Int -> Integer -> UTCTime
utcAt year month day hour minute second micros =
    let totalSeconds :: Integer
        totalSeconds = fromIntegral (hour * 3600 + minute * 60 + second)

        totalPicos :: Integer
        totalPicos = (totalSeconds * 1000000 + micros) * 1000000
    in  UTCTime (fromGregorian year month day) (picosecondsToDiffTime totalPicos)

isCorruptedData :: Either StorageError a -> Bool
isCorruptedData (Left CorruptedData{}) = True
isCorruptedData _                      = False

mustRight :: Show e => Either e a -> IO a
mustRight (Right val) = pure val
mustRight (Left err)  = fail ("Expected Right, got Left: " <> show err)
