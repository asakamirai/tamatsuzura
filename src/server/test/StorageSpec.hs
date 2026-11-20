{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module StorageSpec
    ( storageSpec
    ) where

import           Control.Concurrent                (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import           Control.Exception                 (SomeException, bracket, finally, throwIO, try)
import           Data.Maybe                        (isJust)
import           Data.Text                         (Text)
import qualified Data.Text                         as Txt
import           Data.Time.Calendar                (fromGregorian)
import           Data.Time.Clock                   (UTCTime(..), picosecondsToDiffTime)
import qualified Data.UUID                         as UUID
import qualified Data.UUID.V4                      as UUID4
import           Database.SQLite.Simple            (Only(..))
import qualified Database.SQLite.Simple            as SQLite
import qualified System.Directory.OsPath           as DirOS
import           System.OsPath                     (OsPath)
import qualified System.OsPath                     as OS
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , CreateDocumentRequest(..)
    , DocumentListQuery(..)
    , DocumentListResponse(..)
    , DocumentStatusFilter(..)
    , FolderListResponse(..)
    , SearchQuery
    , TagListResponse(..)
    , UpdateDocumentRequest(..)
    , defaultCreateDocumentRequest
    , defaultDocumentListQuery
    , mkListLimit
    , mkListOffset
    , mkSearchQuery
    )
import           TamaTsuzura.Common.Auth
    ( UserId
    , UserProfile(..)
    , mkPassword
    , mkUserId
    , mkUsername
    )
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody(..)
    , DocumentFormat(..)
    , DocumentSummary(..)
    , emptyDocumentBody
    , emptyDocumentTitle
    , emptyTagSet
    , mkDocumentId
    , mkDocumentTitle
    , mkFolderPath
    , mkTag
    , mkTagSetFromTexts
    , toDocumentSummary
    )
import           TamaTsuzura.Common.Text           (ToText(..))
import           TamaTsuzura.Common.Validation     (ValidationError(..), maxDocumentBodyBytes)
import           TamaTsuzura.Server.Auth.Crypto
    ( fastTestPasswordHashParams
    , hashPasswordWithSalt
    )
import           TamaTsuzura.Server.Storage
    ( SqlitePragmas(..)
    , StorageConfig(..)
    , StorageError(..)
    , StorageHandle
    , StorageTarget(..)
    , busyTimeoutMsValue
    , createDocument
    , createDocumentWith
    , defaultBusyTimeoutMs
    , defaultStorageConfig
    , getDocument
    , inMemoryStorageConfig
    , inspectStoragePragmas
    , listDocuments
    , listFolders
    , listTags
    , maxBusyTimeoutMs
    , mkBusyTimeoutMs
    , permanentDeleteDocument
    , restoreDocument
    , softDeleteDocument
    , softDeleteDocumentAt
    , storageErrorToApiError
    , updateDocument
    , updateDocumentAt
    , withStorage
    )
import           TamaTsuzura.Server.Storage.Auth   (StoredUser(..), createUserWith)
import           TamaTsuzura.Server.Storage.Schema
    ( asciiToLower
    , currentSchemaVersion
    , escapeLikePattern
    , formatSqliteUtcTime
    , initializeSchema
    , parseSqliteUtcTime
    , truncateToMicros
    , withImmediateTransaction
    )
import           Test.Hspec                        (Spec, describe, it, shouldBe, shouldSatisfy)

storageSpec :: Spec
storageSpec = do
    configAndUtilSpec
    schemaAndConcurrencySpec
    transactionRollbackSpec
    documentCrudSpec
    trashLifecycleSpec
    listQueryAndMetadataSpec
    ownerIsolationSpec
    corruptedDataSpec

configAndUtilSpec :: Spec
configAndUtilSpec = describe "Storage config and utilities" $ do
    it "validates BusyTimeoutMs boundaries" $ do
        fmap busyTimeoutMsValue (mkBusyTimeoutMs 0) `shouldBe` Right 0
        fmap busyTimeoutMsValue (mkBusyTimeoutMs 5000) `shouldBe` Right 5000
        fmap busyTimeoutMsValue (mkBusyTimeoutMs maxBusyTimeoutMs) `shouldBe` Right maxBusyTimeoutMs
        mkBusyTimeoutMs (-1) `shouldSatisfy` isLeft
        mkBusyTimeoutMs (maxBusyTimeoutMs + 1) `shouldSatisfy` isLeft
        busyTimeoutMsValue defaultBusyTimeoutMs `shouldBe` 5000

    it "formats and parses UTC timestamps with microsecond precision and monotonic lexical order" $ do
        let t0 :: UTCTime
            t0 = utcAt 2026 11 20 5 0 0 0

            t1 :: UTCTime
            t1 = utcAt 2026 11 20 5 0 0 5

            t2 :: UTCTime
            t2 = utcAt 2026 11 20 5 0 0 500000

            tEarlyYear :: UTCTime
            tEarlyYear = utcAt 999 1 2 3 4 5 6

            tWithPicos :: UTCTime
            tWithPicos = UTCTime (fromGregorian 2026 11 20) (picosecondsToDiffTime 123456789012)

            s0 :: Text
            s0 = formatSqliteUtcTime t0

            s1 :: Text
            s1 = formatSqliteUtcTime t1

            s2 :: Text
            s2 = formatSqliteUtcTime t2
        s0 `shouldBe` "2026-11-20T05:00:00.000000Z"
        s1 `shouldBe` "2026-11-20T05:00:00.000005Z"
        s2 `shouldBe` "2026-11-20T05:00:00.500000Z"
        formatSqliteUtcTime tEarlyYear `shouldBe` "0999-01-02T03:04:05.000006Z"
        (s0 < s1 && s1 < s2) `shouldBe` True
        parseSqliteUtcTime s0 `shouldBe` Right t0
        parseSqliteUtcTime s1 `shouldBe` Right t1
        parseSqliteUtcTime s2 `shouldBe` Right t2
        parseSqliteUtcTime (formatSqliteUtcTime tWithPicos) `shouldBe` Right (truncateToMicros tWithPicos)
        parseSqliteUtcTime "not-a-timestamp" `shouldSatisfy` isCorruptedData

    it "lowercases ASCII characters only to match SQLite built-in lower() and escapes LIKE metacharacters" $ do
        asciiToLower "AbC-123 第Ａ章 Éclair ＰＤＦ" `shouldBe` "abc-123 第Ａ章 Éclair ＰＤＦ"
        escapeLikePattern "hello" `shouldBe` "%hello%"
        escapeLikePattern "100%_a\\b" `shouldBe` "%100\\%\\_a\\\\b%"

    it "maps StorageError to ApiError without leaking internal details" $ do
        docId <- mustRight (mkDocumentId "doc-1")
        uid <- mustRight (mkUserId "user-1")
        uname <- mustRight (mkUsername "alice")
        (storageErrorToApiError (DocumentNotFound docId)).code `shouldBe` ErrNotFound
        (storageErrorToApiError (DocumentInTrash docId)).code `shouldBe` ErrConflict
        (storageErrorToApiError (DocumentNotTrashed docId)).code `shouldBe` ErrConflict
        (storageErrorToApiError (DocumentIdAlreadyExists docId)).code `shouldBe` ErrConflict
        let dupUserErr :: ApiError
            dupUserErr = storageErrorToApiError (UsernameAlreadyExists uname)
        dupUserErr.code `shouldBe` ErrConflict
        dupUserErr.field `shouldBe` Just "username"
        (storageErrorToApiError (UserIdAlreadyExists uid)).code `shouldBe` ErrConflict
        (storageErrorToApiError (UserNotFound uid)).code `shouldBe` ErrNotFound
        (storageErrorToApiError SessionNotFound).code `shouldBe` ErrUnauthorized
        (storageErrorToApiError (StorageValidationError (BodyTooLarge (maxDocumentBodyBytes + 1) maxDocumentBodyBytes))).code
            `shouldBe` ErrPayloadTooLarge
        let internalErr :: ApiError
            internalErr = storageErrorToApiError (SqliteOperationError "secret path /var/private.db")
        internalErr.code `shouldBe` ErrInternalServer
        internalErr.message `shouldBe` "Internal server error"
        internalErr.field `shouldBe` Nothing

schemaAndConcurrencySpec :: Spec
schemaAndConcurrencySpec = describe "SQLite schema initialization, WAL, and busy_timeout" $ do
    it "enables WAL, busy_timeout, foreign_keys, and user_version on file-backed DB (creating parent dirs)" $
        withTempDbPath "nested/data/tamatsuzura.sqlite3" $ \ dbPath -> do
            customTimeout <- mustRight (mkBusyTimeoutMs 3210)
            let cfg :: StorageConfig
                cfg = StorageConfig
                    { target        = StorageFile dbPath
                    , busyTimeoutMs = customTimeout
                    }
            pragmas <- mustRight =<< withStorage cfg inspectStoragePragmas
            pragmas.journalMode `shouldBe` "wal"
            pragmas.busyTimeoutMs `shouldBe` 3210
            pragmas.foreignKeys `shouldBe` True
            pragmas.userVersion `shouldBe` currentSchemaVersion

    it "initializes schema idempotently and persists documents across reopens" $
        withTempDbPath "persist.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            ownerId <- mustRight =<< withStorage cfg (\ h -> Right <$> seedUser h "user-persist" "persist_user")
            docId <- mustRight (mkDocumentId "persisted-doc")
            title <- mustRight (mkDocumentTitle "永続化テスト")
            folder <- mustRight (mkFolderPath "work/notes")
            tags <- mustRight (mkTagSetFromTexts ["tagA", "tagB"])
            let createdTime :: UTCTime
                createdTime = utcAt 2026 11 20 6 0 0 123456

                req :: CreateDocumentRequest
                req = CreateDocumentRequest
                    { title  = title
                    , body   = DocumentBody "# 内容"
                    , format = FormatMarkdown
                    , folder = Just folder
                    , tags   = tags
                    }
            created <- mustRight =<< withStorage cfg (\ h -> createDocumentWith h ownerId docId createdTime req)
            fetched <- mustRight =<< withStorage cfg (\ h -> getDocument h ownerId docId)
            fetched `shouldBe` created

    it "rejects databases with an unsupported schema user_version" $
        withTempDbPath "future.sqlite3" $ \ dbPath -> do
            -- sqlite-simple の withConnection には OsPath 版がないためテスト準備時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            SQLite.withConnection dbStr $ \ conn ->
                SQLite.execute_ conn "PRAGMA user_version = 99"
            eRes <- withStorage (defaultStorageConfig dbPath) inspectStoragePragmas
            eRes `shouldBe` Left (UnsupportedSchemaVersion 99 currentSchemaVersion)

    it "allows non-blocking WAL reads during active write locks while writers wait up to busy_timeout" $
        withTempDbPath "lock.sqlite3" $ \ dbPath -> do
            let baseCfg :: StorageConfig
                baseCfg = defaultStorageConfig dbPath
            ownerId <- mustRight =<< withStorage baseCfg (\ h -> Right <$> seedUser h "user-lock" "lock_user")
            seeded <- mustRight =<< withStorage baseCfg (\ h -> createDocument h ownerId defaultCreateDocumentRequest)
            -- sqlite-simple の withConnection には OsPath 版がないためテスト準備時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath

            -- busy_timeout = 2000ms の書き込み接続は短時間のロック解放を待って成功する
            lockAcquired <- newEmptyMVar
            lockReleased <- newEmptyMVar
            _ <- forkIO $
                SQLite.withConnection dbStr
                    ( \ conn -> do
                        SQLite.execute_ conn "BEGIN IMMEDIATE"
                        putMVar lockAcquired (Right ())
                        threadDelay 150000
                        SQLite.execute_ conn "COMMIT"
                    )
                    `finally` ( do
                        _ <- tryPutMVar lockAcquired (Left ("Lock thread exited early" :: Text))
                        _ <- tryPutMVar lockReleased ()
                        pure ()
                    )
            _ <- mustRight =<< takeMVar lockAcquired
            waitTimeout <- mustRight (mkBusyTimeoutMs 2000)
            let waitCfg :: StorageConfig
                waitCfg = StorageConfig
                    { target        = StorageFile dbPath
                    , busyTimeoutMs = waitTimeout
                    }
            waitedDoc <- mustRight =<< withStorage waitCfg (\ h -> createDocument h ownerId defaultCreateDocumentRequest)
            takeMVar lockReleased
            waitedDoc.title `shouldBe` emptyDocumentTitle

            -- 書き込みロック保持中でも、busy_timeout = 0ms の読み取りは WAL によりブロックせず即座に成功し、
            -- 一方で同じ busy_timeout = 0ms の書き込みは即座に SqliteOperationError となる
            lockAcquired2 <- newEmptyMVar
            releaseSignal <- newEmptyMVar
            lockDone2 <- newEmptyMVar
            _ <- forkIO $
                SQLite.withConnection dbStr
                    ( \ conn -> do
                        SQLite.execute_ conn "BEGIN IMMEDIATE"
                        putMVar lockAcquired2 (Right ())
                        takeMVar releaseSignal
                        _ <- try (SQLite.execute_ conn "ROLLBACK") :: IO (Either SQLite.SQLError ())
                        pure ()
                    )
                    `finally` ( do
                        _ <- tryPutMVar lockAcquired2 (Left ("Lock thread 2 exited early" :: Text))
                        _ <- tryPutMVar lockDone2 ()
                        pure ()
                    )
            _ <- mustRight =<< takeMVar lockAcquired2
            zeroTimeout <- mustRight (mkBusyTimeoutMs 0)
            let noWaitCfg :: StorageConfig
                noWaitCfg = StorageConfig
                    { target        = StorageFile dbPath
                    , busyTimeoutMs = zeroTimeout
                    }
            readDuringLock <- withStorage noWaitCfg (\ h -> getDocument h ownerId seeded.id)
            listDuringLock <- withStorage noWaitCfg (\ h -> listDocuments h ownerId defaultDocumentListQuery)
            writeDuringLock <- withStorage noWaitCfg (\ h -> createDocument h ownerId defaultCreateDocumentRequest)
            putMVar releaseSignal ()
            takeMVar lockDone2

            readDuringLock `shouldBe` Right seeded
            fmap (\ res -> res.total) listDuringLock `shouldBe` Right 2
            writeDuringLock `shouldSatisfy` isSqliteOperationError

transactionRollbackSpec :: Spec
transactionRollbackSpec = describe "Transaction rollback atomicity" $ do
    it "rolls back partial writes on Left result and on thrown exception without leaving an active transaction" $
        SQLite.withConnection ":memory:" $ \ conn -> do
            SQLite.execute_ conn "PRAGMA foreign_keys = ON"
            _ <- mustRight =<< initializeSchema conn
            docId <- mustRight (mkDocumentId "rollback-doc")
            let ts :: Text
                ts = formatSqliteUtcTime (utcAt 2026 11 20 10 0 0 0)
            SQLite.execute
                conn
                "INSERT INTO users (id, username, display_name, password_hash, created_at, updated_at)\
                \ VALUES ('user-rb', 'rb_user', NULL, '$argon2id$v=19$m=64,t=1,p=1$c2FsdHNhbHRzYWx0c2FsdA$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', ?, ?)"
                (ts, ts)

            let insertRow :: IO ()
                insertRow =
                    SQLite.execute
                        conn
                        "INSERT INTO documents (id, owner_id, title, body, format, folder, created_at, updated_at, deleted_at)\
                        \ VALUES (?, 'user-rb', 't', 'b', 'markdown', NULL, ?, ?, NULL)"
                        (toText docId, ts, ts)

            -- Left 返却時は途中までの書き込みがロールバックされる
            eLeft <- withImmediateTransaction conn $ do
                insertRow
                pure (Left (DocumentNotFound docId) :: Either StorageError ())
            eLeft `shouldBe` Left (DocumentNotFound docId)
            countAfterLeft <- SQLite.query_ conn "SELECT COUNT(*) FROM documents" :: IO [Only Int]
            countAfterLeft `shouldBe` [Only 0]

            -- 例外送出時もロールバックされ、接続上で後続のトランザクションが正常に開始できる
            eThrown <- try $ withImmediateTransaction conn $ do
                insertRow
                _ <- throwIO (DocumentNotFound docId)
                pure (Right () :: Either StorageError ())
            (eThrown :: Either SomeException (Either StorageError ())) `shouldSatisfy` isLeft
            countAfterEx <- SQLite.query_ conn "SELECT COUNT(*) FROM documents" :: IO [Only Int]
            countAfterEx `shouldBe` [Only 0]

            -- 後続トランザクションが正常にコミットできる (未終了トランザクション残留なし)
            eOk <- withImmediateTransaction conn $ do
                insertRow
                pure (Right ())
            eOk `shouldBe` Right ()
            countAfterCommit <- SQLite.query_ conn "SELECT COUNT(*) FROM documents" :: IO [Only Int]
            countAfterCommit `shouldBe` [Only 1]

documentCrudSpec :: Spec
documentCrudSpec = describe "Document CRUD operations" $ do
    it "creates a default blank Markdown document with UUIDv4 ID and retrieves it identically" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            created <- mustRight =<< createDocument storage ownerId defaultCreateDocumentRequest
            created.title `shouldBe` emptyDocumentTitle
            created.body `shouldBe` emptyDocumentBody
            created.format `shouldBe` FormatMarkdown
            created.folder `shouldBe` Nothing
            created.tags `shouldBe` emptyTagSet
            created.deletedAt `shouldBe` Nothing
            created.createdAt `shouldBe` created.updatedAt
            fetched <- mustRight =<< getDocument storage ownerId created.id
            fetched `shouldBe` created

    it "rejects creating a document for a non-existent owner with UserNotFound" $
        withInMemory $ \ storage -> do
            missingOwner <- mustRight (mkUserId "missing-owner")
            eRes <- createDocument storage missingOwner defaultCreateDocumentRequest
            eRes `shouldBe` Left (UserNotFound missingOwner)

    it "creates a full document and rejects duplicate IDs" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            docId <- mustRight (mkDocumentId "doc_001")
            title <- mustRight (mkDocumentTitle "仕様メモ")
            folder <- mustRight (mkFolderPath "projects/tama")
            tags <- mustRight (mkTagSetFromTexts ["haskell", "設計"])
            let t1 :: UTCTime
                t1 = utcAt 2026 11 20 8 0 0 0

                req :: CreateDocumentRequest
                req = CreateDocumentRequest
                    { title  = title
                    , body   = DocumentBody "本文テキスト"
                    , format = FormatPlainText
                    , folder = Just folder
                    , tags   = tags
                    }
            created <- mustRight =<< createDocumentWith storage ownerId docId t1 req
            created.id `shouldBe` docId
            created.format `shouldBe` FormatPlainText
            created.folder `shouldBe` Just folder
            created.tags `shouldBe` tags

            dupRes <- createDocumentWith storage ownerId docId t1 req
            dupRes `shouldBe` Left (DocumentIdAlreadyExists docId)

    it "rejects oversized document bodies (> 5 MiB) on create and update without modifying storage" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            let oversizedBody :: DocumentBody
                oversizedBody = DocumentBody (Txt.replicate (maxDocumentBodyBytes + 1) "a")

                badCreate :: CreateDocumentRequest
                badCreate = CreateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = oversizedBody
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }
            eCreate <- createDocument storage ownerId badCreate
            eCreate `shouldBe`
                Left (StorageValidationError (BodyTooLarge (maxDocumentBodyBytes + 1) maxDocumentBodyBytes))

            created <- mustRight =<< createDocument storage ownerId defaultCreateDocumentRequest
            let badUpdate :: UpdateDocumentRequest
                badUpdate = UpdateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = oversizedBody
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }
            eUpdate <- updateDocumentAt storage ownerId created.id (utcAt 2026 11 20 9 0 0 0) badUpdate
            eUpdate `shouldBe`
                Left (StorageValidationError (BodyTooLarge (maxDocumentBodyBytes + 1) maxDocumentBodyBytes))
            stillOriginal <- mustRight =<< getDocument storage ownerId created.id
            stillOriginal `shouldBe` created

    it "returns DocumentNotFound when getting or updating a non-existent document" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            missingId <- mustRight (mkDocumentId "missing-id")
            getDocument storage ownerId missingId `shouldReturnEq` Left (DocumentNotFound missingId)
            let req :: UpdateDocumentRequest
                req = UpdateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = emptyDocumentBody
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }
            updateDocumentAt storage ownerId missingId (utcAt 2026 11 20 9 0 0 0) req
                `shouldReturnEq` Left (DocumentNotFound missingId)

    it "updates title, body, format, folder (including moving to root), tags, and updatedAt while preserving createdAt" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            docId <- mustRight (mkDocumentId "doc-update")
            title1 <- mustRight (mkDocumentTitle "初版")
            title2 <- mustRight (mkDocumentTitle "改訂版")
            folder1 <- mustRight (mkFolderPath "drafts")
            tags1 <- mustRight (mkTagSetFromTexts ["old", "keep"])
            tags2 <- mustRight (mkTagSetFromTexts ["keep", "new"])
            let t1 :: UTCTime
                t1 = utcAt 2026 11 20 10 0 0 0

                t2 :: UTCTime
                t2 = utcAt 2026 11 20 11 30 0 500000
            _ <- mustRight =<< createDocumentWith storage ownerId docId t1 CreateDocumentRequest
                { title  = title1
                , body   = DocumentBody "before"
                , format = FormatMarkdown
                , folder = Just folder1
                , tags   = tags1
                }
            updated <- mustRight =<< updateDocumentAt storage ownerId docId t2 UpdateDocumentRequest
                { title  = title2
                , body   = DocumentBody "after"
                , format = FormatPlainText
                , folder = Nothing
                , tags   = tags2
                }
            updated.title `shouldBe` title2
            updated.body `shouldBe` DocumentBody "after"
            updated.format `shouldBe` FormatPlainText
            updated.folder `shouldBe` Nothing
            updated.tags `shouldBe` tags2
            updated.createdAt `shouldBe` t1
            updated.updatedAt `shouldBe` t2
            updated.deletedAt `shouldBe` Nothing

            fetched <- mustRight =<< getDocument storage ownerId docId
            fetched `shouldBe` updated

            -- getCurrentTime を使う updateDocument ラッパーの疎通検証
            title3 <- mustRight (mkDocumentTitle "現在時刻更新")
            updatedNow <- mustRight =<< updateDocument storage ownerId docId UpdateDocumentRequest
                { title  = title3
                , body   = DocumentBody "after-now"
                , format = FormatMarkdown
                , folder = Just folder1
                , tags   = emptyTagSet
                }
            updatedNow.title `shouldBe` title3
            updatedNow.createdAt `shouldBe` t1

trashLifecycleSpec :: Spec
trashLifecycleSpec = describe "Trash, restore, and permanent delete lifecycle" $ do
    it "supports idempotent soft-delete and restore while preserving updatedAt and allowing preview via getDocument" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            docId <- mustRight (mkDocumentId "doc-trash")
            let tCreate :: UTCTime
                tCreate = utcAt 2026 11 20 12 0 0 0

                tDelete1 :: UTCTime
                tDelete1 = utcAt 2026 11 20 13 0 0 0

                tDelete2 :: UTCTime
                tDelete2 = utcAt 2026 11 20 14 0 0 0
            created <- mustRight =<< createDocumentWith storage ownerId docId tCreate defaultCreateDocumentRequest

            -- ゴミ箱移動
            trashed1 <- mustRight =<< softDeleteDocumentAt storage ownerId docId tDelete1
            trashed1.deletedAt `shouldBe` Just tDelete1
            trashed1.updatedAt `shouldBe` tCreate

            -- ゴミ箱移動の冪等性 (既存の deleted_at を保持)
            trashed2 <- mustRight =<< softDeleteDocumentAt storage ownerId docId tDelete2
            trashed2 `shouldBe` trashed1

            -- ゴミ箱内文書も getDocument でプレビュー可能
            previewed <- mustRight =<< getDocument storage ownerId docId
            previewed `shouldBe` trashed1

            -- ゴミ箱内文書の更新は DocumentInTrash (409 Conflict)
            eUpdate <- updateDocumentAt storage ownerId docId tDelete2 UpdateDocumentRequest
                { title  = emptyDocumentTitle
                , body   = DocumentBody "cannot edit"
                , format = FormatMarkdown
                , folder = Nothing
                , tags   = emptyTagSet
                }
            eUpdate `shouldBe` Left (DocumentInTrash docId)

            -- 復元
            restored1 <- mustRight =<< restoreDocument storage ownerId docId
            restored1 `shouldBe` created

            -- 復元の冪等性
            restored2 <- mustRight =<< restoreDocument storage ownerId docId
            restored2 `shouldBe` created

            -- getCurrentTime を使う softDeleteDocument ラッパーの疎通検証
            trashedNow <- mustRight =<< softDeleteDocument storage ownerId docId
            trashedNow.deletedAt `shouldSatisfy` isJust
            trashedNow.updatedAt `shouldBe` tCreate

    it "rejects permanent deletion of active documents and deletes trashed documents including cascaded tags" $
        withTempDbPath "cascade.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            ownerId <- mustRight =<< withStorage cfg (\ h -> Right <$> seedUser h "user-1" "alice")
            docId <- mustRight (mkDocumentId "doc-perm")
            tags <- mustRight (mkTagSetFromTexts ["cleanup", "temp"])
            let tCreate :: UTCTime
                tCreate = utcAt 2026 11 20 12 0 0 0

                tDelete :: UTCTime
                tDelete = utcAt 2026 11 20 12 30 0 0
            _ <- mustRight =<< withStorage cfg (\ h ->
                createDocumentWith h ownerId docId tCreate CreateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = DocumentBody "bye"
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = tags
                    })

            -- 稼働中文書の完全削除は DocumentNotTrashed (409 Conflict)
            ePermActive <- withStorage cfg (\ h -> permanentDeleteDocument h ownerId docId)
            ePermActive `shouldBe` Left (DocumentNotTrashed docId)

            -- ゴミ箱移動後は完全削除できる
            _ <- mustRight =<< withStorage cfg (\ h -> softDeleteDocumentAt h ownerId docId tDelete)
            _ <- mustRight =<< withStorage cfg (\ h -> permanentDeleteDocument h ownerId docId)

            -- 文書自体も document_tags 行も消えていることを検証
            eGet <- withStorage cfg (\ h -> getDocument h ownerId docId)
            eGet `shouldBe` Left (DocumentNotFound docId)

            -- sqlite-simple の withConnection には OsPath 版がないため検証時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            tagCountRows <- SQLite.withConnection dbStr $ \ conn ->
                SQLite.query
                    conn
                    "SELECT COUNT(*) FROM document_tags WHERE document_id = ?"
                    (Only (toText docId))
                    :: IO [Only Int]
            tagCountRows `shouldBe` [Only 0]

    it "returns DocumentNotFound on soft-delete, restore, and permanent-delete for missing IDs" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            missingId <- mustRight (mkDocumentId "nope")
            softDeleteDocumentAt storage ownerId missingId (utcAt 2026 11 20 12 0 0 0)
                `shouldReturnEq` Left (DocumentNotFound missingId)
            restoreDocument storage ownerId missingId
                `shouldReturnEq` Left (DocumentNotFound missingId)
            permanentDeleteDocument storage ownerId missingId
                `shouldReturnEq` Left (DocumentNotFound missingId)

listQueryAndMetadataSpec :: Spec
listQueryAndMetadataSpec = describe "Document listing, filtering, search, pagination, and metadata lists" $ do
    it "returns empty results on an empty database" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            emptyList <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery
            emptyList `shouldBe` DocumentListResponse { documents = [], total = 0 }
            emptyTags <- mustRight =<< listTags storage ownerId
            emptyTags `shouldBe` TagListResponse { tags = [] }
            emptyFolders <- mustRight =<< listFolders storage ownerId
            emptyFolders `shouldBe` FolderListResponse { folders = [] }

    it "sorts by updated_at DESC then id ASC and filters by status, tag, folder/subfolders, search, combined filters, and pagination" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            d1 <- seedDoc storage ownerId "doc-a" (utcAt 2026 11 20 10 0 0 0) "Alpha Note" "Body with 100% match" (Just "work") ["haskell", "memo"]
            d2 <- seedDoc storage ownerId "doc-b" (utcAt 2026 11 20 12 0 0 0) "Beta Guide" "Subfolder _underscore_ text" (Just "work/notes") ["haskell"]
            d3 <- seedDoc storage ownerId "doc-c" (utcAt 2026 11 20 12 0 0 0) "Gamma Memo" "Path C:\\temp\\file" (Just "work/notes/deep") ["ops"]
            d4 <- seedDoc storage ownerId "doc-d" (utcAt 2026 11 20 11 0 0 0) "Similar Prefix" "Should not match work/" (Just "work-2/sub") ["memo"]
            d5 <- seedDoc storage ownerId "doc-e" (utcAt 2026 11 20 11 30 0 0) "Zero Suffix" "Boundary char 0 after /" (Just "work0/sub") ["memo"]
            d6 <- seedDoc storage ownerId "doc-f" (utcAt 2026 11 20 9 0 0 0) "Case Folder" "Case sensitive folder check" (Just "Work/notes") ["memo"]
            d7 <- seedDoc storage ownerId "doc-g" (utcAt 2026 11 20 15 0 0 0) "Trashed Alpha" "Deleted doc body" (Just "archive") ["trash-only", "haskell"]
            d8 <- seedDoc storage ownerId "doc-h" (utcAt 2026 11 20 14 0 0 0) "日本語メモ 第Ａ章" "ルート直下の本文 Éclair ＰＤＦ" Nothing ["日本語"]
            d7Trashed <- mustRight =<< softDeleteDocumentAt storage ownerId d7.id (utcAt 2026 11 20 16 0 0 0)

            -- デフォルト一覧: 稼働中のみ、updated_at DESC、同一時刻は id ASC (doc-b < doc-c)
            activeRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery
            activeRes.total `shouldBe` 7
            map (\ s -> s.id) activeRes.documents `shouldBe`
                [d8.id, d2.id, d3.id, d5.id, d4.id, d1.id, d6.id]
            activeRes.documents `shouldBe`
                map toDocumentSummary [d8, d2, d3, d5, d4, d1, d6]

            -- status = trashed / all
            trashedRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { status = StatusTrashed }
            trashedRes.total `shouldBe` 1
            trashedRes.documents `shouldBe` [toDocumentSummary d7Trashed]

            allRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { status = StatusAll }
            allRes.total `shouldBe` 8
            map (\ s -> s.id) allRes.documents `shouldBe`
                [d7Trashed.id, d8.id, d2.id, d3.id, d5.id, d4.id, d1.id, d6.id]

            -- tag フィルタ (ゴミ箱文書 d7 は active では除外)
            tagHaskell <- mustRight (mkTag "haskell")
            tagRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { tag = Just tagHaskell }
            tagRes.total `shouldBe` 2
            map (\ s -> s.id) tagRes.documents `shouldBe` [d2.id, d1.id]

            -- folder 完全一致 (includeSubfolders = False)
            folderWork <- mustRight (mkFolderPath "work")
            exactFolderRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery
                { folder            = Just folderWork
                , includeSubfolders = False
                }
            exactFolderRes.total `shouldBe` 1
            map (\ s -> s.id) exactFolderRes.documents `shouldBe` [d1.id]

            -- folder 配下含む (includeSubfolders = True): work-2, work0, Work/notes を誤一致させない
            subFolderRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery
                { folder            = Just folderWork
                , includeSubfolders = True
                }
            subFolderRes.total `shouldBe` 3
            map (\ s -> s.id) subFolderRes.documents `shouldBe` [d2.id, d3.id, d1.id]

            -- folder = Nothing で includeSubfolders = True の場合は全件対象
            noFolderSubRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery
                { folder            = Nothing
                , includeSubfolders = True
                }
            noFolderSubRes.total `shouldBe` 7

            -- 検索: ASCII case-insensitive (タイトル・本文)
            qAlpha <- mustSearchQuery "aLpHa"
            searchAlpha <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { search = Just qAlpha }
            map (\ s -> s.id) searchAlpha.documents `shouldBe` [d1.id]

            -- 検索: 日本語・全角英大文字・アクセント付き大文字の部分一致
            qJa <- mustSearchQuery "ルート直下"
            searchJa <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { search = Just qJa }
            map (\ s -> s.id) searchJa.documents `shouldBe` [d8.id]

            qFullWidth <- mustSearchQuery "第Ａ章"
            searchFullWidth <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { search = Just qFullWidth }
            map (\ s -> s.id) searchFullWidth.documents `shouldBe` [d8.id]

            qAccented <- mustSearchQuery "Éclair"
            searchAccented <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { search = Just qAccented }
            map (\ s -> s.id) searchAccented.documents `shouldBe` [d8.id]

            -- 検索: LIKE メタ文字 (%, _, \) がワイルドカード誤爆しない
            qPercent <- mustSearchQuery "100%"
            searchPercent <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { search = Just qPercent }
            map (\ s -> s.id) searchPercent.documents `shouldBe` [d1.id]

            qUnderscore <- mustSearchQuery "_underscore_"
            searchUnderscore <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { search = Just qUnderscore }
            map (\ s -> s.id) searchUnderscore.documents `shouldBe` [d2.id]

            qBackslash <- mustSearchQuery "c:\\temp\\"
            searchBackslash <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery { search = Just qBackslash }
            map (\ s -> s.id) searchBackslash.documents `shouldBe` [d3.id]

            -- 複合フィルタ (status + tag + folder + includeSubfolders + search の同時 AND 条件)
            qGuide <- mustSearchQuery "guide"
            combinedRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery
                { status            = StatusActive
                , tag               = Just tagHaskell
                , folder            = Just folderWork
                , includeSubfolders = True
                , search            = Just qGuide
                }
            combinedRes.total `shouldBe` 1
            map (\ s -> s.id) combinedRes.documents `shouldBe` [d2.id]

            -- ページネーション (limit / offset と total の分離、offset >= total の境界)
            lim2 <- mustRight (mkListLimit 2)
            off2 <- mustRight (mkListOffset 2)
            pageRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery
                { limit  = lim2
                , offset = off2
                }
            pageRes.total `shouldBe` 7
            map (\ s -> s.id) pageRes.documents `shouldBe` [d3.id, d5.id]

            offPastEnd <- mustRight (mkListOffset 100)
            pastEndRes <- mustRight =<< listDocuments storage ownerId defaultDocumentListQuery
                { limit  = lim2
                , offset = offPastEnd
                }
            pastEndRes.total `shouldBe` 7
            pastEndRes.documents `shouldBe` []

            -- 整理メタデータ一覧 (稼働中文書のみ・重複なし昇順・ルート除外)
            tagList <- mustRight =<< listTags storage ownerId
            map toText tagList.tags `shouldBe` ["haskell", "memo", "ops", "日本語"]

            folderList <- mustRight =<< listFolders storage ownerId
            map toText folderList.folders `shouldBe`
                ["Work/notes", "work", "work-2/sub", "work/notes", "work/notes/deep", "work0/sub"]

            -- ゴミ箱から復元するとゴミ箱専用だったタグ・フォルダが再び現れる
            _ <- mustRight =<< restoreDocument storage ownerId d7.id
            tagListAfterRestore <- mustRight =<< listTags storage ownerId
            map toText tagListAfterRestore.tags `shouldBe` ["haskell", "memo", "ops", "trash-only", "日本語"]
            folderListAfterRestore <- mustRight =<< listFolders storage ownerId
            map toText folderListAfterRestore.folders `shouldBe`
                ["Work/notes", "archive", "work", "work-2/sub", "work/notes", "work/notes/deep", "work0/sub"]

ownerIsolationSpec :: Spec
ownerIsolationSpec = describe "Multi-user document and metadata owner isolation" $ do
    it "isolates documents, tags, folders, and mutations strictly per owner and cascades on user deletion" $
        withTempDbPath "owner-isolation.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            (aliceId, bobId) <- mustRight =<< withStorage cfg (\ h -> do
                a <- seedUser h "user-alice" "alice"
                b <- seedUser h "user-bob" "bob"
                pure (Right (a, b)))

            aliceActive <- mustRight =<< withStorage cfg (\ h ->
                Right <$> seedDoc h aliceId "alice-active" (utcAt 2026 11 20 10 0 0 0) "Alice Active" "secret a" (Just "alice/private") ["alice-tag"])
            aliceTrashed <- mustRight =<< withStorage cfg (\ h -> do
                d <- seedDoc h aliceId "alice-trashed" (utcAt 2026 11 20 11 0 0 0) "Alice Trashed" "secret t" (Just "alice/trash") ["alice-trash-tag"]
                softDeleteDocumentAt h aliceId d.id (utcAt 2026 11 20 12 0 0 0))
            bobDoc <- mustRight =<< withStorage cfg (\ h ->
                Right <$> seedDoc h bobId "bob-doc" (utcAt 2026 11 20 13 0 0 0) "Bob Note" "bob body" (Just "bob/notes") ["bob-tag"])

            -- 一覧・タグ・フォルダは所有者ごとに完全分離される
            aliceList <- mustRight =<< withStorage cfg (\ h -> listDocuments h aliceId defaultDocumentListQuery { status = StatusAll })
            aliceList.total `shouldBe` 2
            map (\ s -> s.id) aliceList.documents `shouldBe` [aliceTrashed.id, aliceActive.id]

            bobList <- mustRight =<< withStorage cfg (\ h -> listDocuments h bobId defaultDocumentListQuery { status = StatusAll })
            bobList.total `shouldBe` 1
            map (\ s -> s.id) bobList.documents `shouldBe` [bobDoc.id]

            aliceTags <- mustRight =<< withStorage cfg (`listTags` aliceId)
            map toText aliceTags.tags `shouldBe` ["alice-tag"]
            bobTags <- mustRight =<< withStorage cfg (`listTags` bobId)
            map toText bobTags.tags `shouldBe` ["bob-tag"]

            aliceFolders <- mustRight =<< withStorage cfg (`listFolders` aliceId)
            map toText aliceFolders.folders `shouldBe` ["alice/private"]
            bobFolders <- mustRight =<< withStorage cfg (`listFolders` bobId)
            map toText bobFolders.folders `shouldBe` ["bob/notes"]

            -- Bob から Alice の文書 (稼働中/ゴミ箱双方) への全操作は存在・状態を漏らさず DocumentNotFound を返す
            let dummyUpdate :: UpdateDocumentRequest
                dummyUpdate = UpdateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = DocumentBody "hijack"
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }
            withStorage cfg (\ h -> getDocument h bobId aliceActive.id) `shouldReturnEq` Left (DocumentNotFound aliceActive.id)
            withStorage cfg (\ h -> getDocument h bobId aliceTrashed.id) `shouldReturnEq` Left (DocumentNotFound aliceTrashed.id)
            withStorage cfg (\ h -> updateDocumentAt h bobId aliceActive.id (utcAt 2026 11 20 14 0 0 0) dummyUpdate)
                `shouldReturnEq` Left (DocumentNotFound aliceActive.id)
            withStorage cfg (\ h -> updateDocumentAt h bobId aliceTrashed.id (utcAt 2026 11 20 14 0 0 0) dummyUpdate)
                `shouldReturnEq` Left (DocumentNotFound aliceTrashed.id)
            withStorage cfg (\ h -> softDeleteDocumentAt h bobId aliceActive.id (utcAt 2026 11 20 14 0 0 0))
                `shouldReturnEq` Left (DocumentNotFound aliceActive.id)
            withStorage cfg (\ h -> restoreDocument h bobId aliceTrashed.id)
                `shouldReturnEq` Left (DocumentNotFound aliceTrashed.id)
            withStorage cfg (\ h -> permanentDeleteDocument h bobId aliceActive.id)
                `shouldReturnEq` Left (DocumentNotFound aliceActive.id)
            withStorage cfg (\ h -> permanentDeleteDocument h bobId aliceTrashed.id)
                `shouldReturnEq` Left (DocumentNotFound aliceTrashed.id)

            -- Alice の文書が一切改変されていないこと
            aliceStillIntact <- mustRight =<< withStorage cfg (\ h -> getDocument h aliceId aliceActive.id)
            aliceStillIntact `shouldBe` aliceActive

            -- Alice ユーザー削除時は ON DELETE CASCADE で Alice の文書・タグのみが削除され Bob の文書は残る
            dbStr <- OS.decodeFS dbPath
            SQLite.withConnection dbStr $ \ conn -> do
                SQLite.execute_ conn "PRAGMA foreign_keys = ON"
                SQLite.execute conn "DELETE FROM users WHERE id = ?" (Only (toText aliceId))
                docCount <- SQLite.query_ conn "SELECT COUNT(*) FROM documents" :: IO [Only Int]
                tagCount <- SQLite.query_ conn "SELECT COUNT(*) FROM document_tags" :: IO [Only Int]
                docCount `shouldBe` [Only 1]
                tagCount `shouldBe` [Only 1]
            bobStillIntact <- mustRight =<< withStorage cfg (\ h -> getDocument h bobId bobDoc.id)
            bobStillIntact `shouldBe` bobDoc

corruptedDataSpec :: Spec
corruptedDataSpec = describe "Corrupted stored data detection" $ do
    it "returns CorruptedData instead of crashing when DB contains invalid timestamps, tags, or folder paths" $
        withTempDbPath "corrupted.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            ownerId <- mustRight =<< withStorage cfg (\ h -> Right <$> seedUser h "user-corrupt" "corrupt_owner")
            docId <- mustRight (mkDocumentId "corrupt-doc")
            _ <- mustRight =<< withStorage cfg (\ h ->
                createDocumentWith h ownerId docId (utcAt 2026 11 20 12 0 0 0) defaultCreateDocumentRequest)
            -- sqlite-simple の withConnection には OsPath 版がないため破損データ注入時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            SQLite.withConnection dbStr $ \ conn -> do
                SQLite.execute
                    conn
                    "UPDATE documents SET created_at = 'broken-timestamp', folder = '../escape' WHERE id = ?"
                    (Only (toText docId))
                SQLite.execute
                    conn
                    "INSERT INTO document_tags (document_id, tag) VALUES (?, '')"
                    (Only (toText docId))
            eDoc <- withStorage cfg (\ h -> getDocument h ownerId docId)
            eDoc `shouldSatisfy` isCorruptedData
            eTags <- withStorage cfg (`listTags` ownerId)
            eTags `shouldSatisfy` isCorruptedData
            eFolders <- withStorage cfg (`listFolders` ownerId)
            eFolders `shouldSatisfy` isCorruptedData

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
    dirPart <- OS.encodeFS ("tamatsuzura-storage-test-" <> UUID.toString rawUuid)
    filePart <- OS.encodeFS relFileName
    let testDir :: OsPath
        testDir = tmpRoot OS.</> dirPart

        dbPath :: OsPath
        dbPath = testDir OS.</> filePart
    bracket
        (DirOS.createDirectoryIfMissing True testDir >> pure dbPath)
        (const (DirOS.removePathForcibly testDir))
        action

seedUser :: StorageHandle -> Text -> Text -> IO UserId
seedUser storage rawUid rawUname = do
    uid <- mustRight (mkUserId rawUid)
    uname <- mustRight (mkUsername rawUname)
    pw <- mustRight (mkPassword "password-12345")
    pwHash <- mustRight (hashPasswordWithSalt fastTestPasswordHashParams "1234567890abcdef" pw)
    stored <- mustRight =<< createUserWith storage uid (utcAt 2026 11 20 7 0 0 0) uname Nothing pwHash
    pure stored.profile.id

seedDoc ::
       StorageHandle
    -> UserId
    -> Text
    -> UTCTime
    -> Text
    -> Text
    -> Maybe Text
    -> [Text]
    -> IO Document
seedDoc storage ownerId rawId ts rawTitle rawBody mRawFolder rawTags = do
    docId <- mustRight (mkDocumentId rawId)
    title <- mustRight (mkDocumentTitle rawTitle)
    mFolder <- traverse (mustRight . mkFolderPath) mRawFolder
    tags <- mustRight (mkTagSetFromTexts rawTags)
    mustRight =<< createDocumentWith storage ownerId docId ts CreateDocumentRequest
        { title  = title
        , body   = DocumentBody rawBody
        , format = FormatMarkdown
        , folder = mFolder
        , tags   = tags
        }

utcAt :: Integer -> Int -> Int -> Int -> Int -> Int -> Integer -> UTCTime
utcAt year month day hour minute second micros =
    let totalSeconds :: Integer
        totalSeconds = fromIntegral (hour * 3600 + minute * 60 + second)

        totalPicos :: Integer
        totalPicos = totalSeconds * 1000000000000 + micros * 1000000
    in  UTCTime (fromGregorian year month day) (picosecondsToDiffTime totalPicos)

mustRight :: Show e => Either e a -> IO a
mustRight (Right val) = pure val
mustRight (Left err)  = fail ("Expected Right, got Left: " <> show err)

mustSearchQuery :: Text -> IO SearchQuery
mustSearchQuery raw = do
    mQuery <- mustRight (mkSearchQuery raw)
    case mQuery of
        Just q  -> pure q
        Nothing -> fail ("Expected non-empty SearchQuery for: " <> Txt.unpack raw)

shouldReturnEq :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturnEq action expected = do
    actual <- action
    actual `shouldBe` expected

isLeft :: Either a b -> Bool
isLeft Left{}  = True
isLeft Right{} = False

isCorruptedData :: Either StorageError a -> Bool
isCorruptedData (Left CorruptedData{}) = True
isCorruptedData _                      = False

isSqliteOperationError :: Either StorageError a -> Bool
isSqliteOperationError (Left SqliteOperationError{}) = True
isSqliteOperationError _                             = False
