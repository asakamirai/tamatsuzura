{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module RevisionSpec
    ( revisionSpec
    ) where

import           Control.Concurrent                 (forkIO, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import           Control.Exception                  (bracket, finally, try)
import           Control.Monad                      (forM_)
import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Aeson                         as Aeson
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BS8
import qualified Data.ByteString.Lazy               as LazyBS
import           Data.Maybe                         (fromMaybe, isJust)
import           Data.Text                          (Text)
import qualified Data.Text                          as Txt
import qualified Data.Text.Encoding                 as TxtEnc
import           Data.Time.Calendar                 (fromGregorian)
import           Data.Time.Clock                    (UTCTime(..), secondsToDiffTime)
import qualified Data.UUID                          as UUID
import qualified Data.UUID.V4                       as UUID4
import           Database.SQLite.Simple             (Only(..))
import qualified Database.SQLite.Simple             as SQLite
import qualified Network.HTTP.Types                 as Http
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Test                   as WaiTest
import qualified System.Directory.OsPath            as DirOS
import           System.OsPath                      (OsPath)
import qualified System.OsPath                      as OS
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , CreateDocumentRequest(..)
    , UpdateDocumentRequest(..)
    , defaultListLimitValue
    , defaultListOffsetValue
    , listLimitValue
    , listOffsetValue
    , mkListLimit
    , mkListOffset
    )
import           TamaTsuzura.Common.Auth
    ( AuthSessionResponse(..)
    , UserId
    , UserProfile(..)
    , mkPassword
    , mkUserId
    , mkUsername
    )
import           TamaTsuzura.Common.Diff            (emptyUnifiedDiff, generateUnifiedDiff)
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody(..)
    , DocumentFormat(..)
    , emptyDocumentTitle
    , emptyTagSet
    , initialRevisionNumber
    , mkDocumentId
    , mkDocumentTitle
    , mkFolderPath
    , mkRevisionNumber
    , mkTagSetFromTexts
    , revisionNumberValue
    )
import           TamaTsuzura.Common.Revision
    ( RestoreRevisionRequest(..)
    , RevisionCompareResponse(..)
    , RevisionDetail(..)
    , RevisionListResponse(..)
    , RevisionSummary(..)
    , defaultRestoreRevisionRequest
    )
import           TamaTsuzura.Common.Text            (ToText(..))
import           TamaTsuzura.Server.Api
    ( RawRevisionListQuery(..)
    , RevisionListQuery(..)
    , defaultRevisionListQuery
    , emptyRawRevisionListQuery
    , parseCompareRevisionsQuery
    , parseRevisionListQuery
    , parseRevisionNumberParam
    )
import           TamaTsuzura.Server.App             (mkApplicationWithAuth)
import           TamaTsuzura.Server.Auth.Cookie     (AuthConfig(..), defaultAuthConfig)
import           TamaTsuzura.Server.Auth.Crypto
    ( fastTestPasswordHashParams
    , hashPasswordWithSalt
    )
import           TamaTsuzura.Server.Auth.Middleware (csrfHeaderName)
import           TamaTsuzura.Server.Storage
    ( StorageConfig(..)
    , StorageError(..)
    , StorageHandle
    , StorageTarget(..)
    , createDocumentWith
    , defaultStorageConfig
    , getDocument
    , getDocumentRevision
    , inMemoryStorageConfig
    , listDocumentRevisions
    , mkBusyTimeoutMs
    , permanentDeleteDocument
    , restoreDocumentRevisionAt
    , softDeleteDocumentAt
    , storageErrorToApiError
    , updateDocumentAt
    , withStorage
    )
import           TamaTsuzura.Server.Storage.Auth     (StoredUser(..), createUserWith)
import           TamaTsuzura.Server.Storage.Revision (computeRevisionContentHash)
import           TamaTsuzura.Server.Storage.Schema   (formatSqliteUtcTime)
import           Test.Hspec                         (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

data SessionHeaders = SessionHeaders
    { cookie :: !ByteString
    , csrf   :: !ByteString
    } deriving (Eq, Show)

revisionSpec :: Spec
revisionSpec = do
    revisionParamParserSpec
    revisionStorageSpec
    revisionApiSpec

revisionParamParserSpec :: Spec
revisionParamParserSpec = describe "Revision query and path parameter validation" $ do
    it "parses revision list pagination defaults and valid bounds" $ do
        parseRevisionListQuery emptyRawRevisionListQuery `shouldBe` Right defaultRevisionListQuery
        parsed <- mustRight $ parseRevisionListQuery RawRevisionListQuery
            { rawLimit  = Just "20"
            , rawOffset = Just "5"
            }
        listLimitValue parsed.limit `shouldBe` 20
        listOffsetValue parsed.offset `shouldBe` 5

    it "validates revision path parameters and reports exact field names" $ do
        r1 <- mustRight (parseRevisionNumberParam "1")
        revisionNumberValue r1 `shouldBe` 1
        r42 <- mustRight (parseRevisionNumberParam "42")
        revisionNumberValue r42 `shouldBe` 42

        assertFieldError "revision" (parseRevisionNumberParam "0")
        assertFieldError "revision" (parseRevisionNumberParam "-1")
        assertFieldError "revision" (parseRevisionNumberParam "abc")
        assertFieldError "revision" (parseRevisionNumberParam "99999999999999999999999")
        assertFieldError "limit" (parseRevisionListQuery emptyRawRevisionListQuery { rawLimit = Just "0" })
        assertFieldError "limit" (parseRevisionListQuery emptyRawRevisionListQuery { rawLimit = Just "201" })
        assertFieldError "offset" (parseRevisionListQuery emptyRawRevisionListQuery { rawOffset = Just "-1" })

    it "maps RevisionNotFound and RevisionConflict to structured ApiError" $ do
        docId <- mustRight (mkDocumentId "doc-rev")
        r1 <- mustRight (mkRevisionNumber 1)
        r2 <- mustRight (mkRevisionNumber 2)

        let notFoundErr :: ApiError
            notFoundErr = storageErrorToApiError (RevisionNotFound docId r2)
        notFoundErr.code `shouldBe` ErrNotFound
        notFoundErr.field `shouldBe` Nothing

        let conflictErr :: ApiError
            conflictErr = storageErrorToApiError (RevisionConflict docId r1 r2)
        conflictErr.code `shouldBe` ErrConflict
        conflictErr.field `shouldBe` Just "base_revision"

revisionStorageSpec :: Spec
revisionStorageSpec = describe "Revision history SQLite storage" $ do
    it "records initial revision on create, appends diffs on content changes, skips metadata-only edits, and reconstructs every revision" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            docId <- mustRight (mkDocumentId "doc-history")
            t1Title <- mustRight (mkDocumentTitle "初版タイトル")
            t2Title <- mustRight (mkDocumentTitle "初版タイトル")
            t3Title <- mustRight (mkDocumentTitle "改題後タイトル")
            folder1 <- mustRight (mkFolderPath "notes/v1")
            folder2 <- mustRight (mkFolderPath "notes/v2")
            tags1 <- mustRight (mkTagSetFromTexts ["draft"])
            tags2 <- mustRight (mkTagSetFromTexts ["published", "review"])

            let ts1 :: UTCTime
                ts1 = utcAt 2026 11 21 10 0 0

                ts2 :: UTCTime
                ts2 = utcAt 2026 11 21 10 5 0

                ts3 :: UTCTime
                ts3 = utcAt 2026 11 21 10 10 0

                ts4 :: UTCTime
                ts4 = utcAt 2026 11 21 10 15 0

                body1 :: DocumentBody
                body1 = DocumentBody "1行目\n2行目\n3行目"

                body2 :: DocumentBody
                body2 = DocumentBody "1行目\n2行目を改訂\n3行目\n4行目を追加\n"

            -- r1: 新規作成
            d1 <- mustRight =<< createDocumentWith storage ownerId docId ts1 CreateDocumentRequest
                { title  = t1Title
                , body   = body1
                , format = FormatMarkdown
                , folder = Just folder1
                , tags   = tags1
                }
            d1.revision `shouldBe` initialRevisionNumber

            -- r2: 本文変更 (base_revision = 1 一致)
            d2 <- mustRight =<< updateDocumentAt storage ownerId docId ts2 UpdateDocumentRequest
                { title        = t2Title
                , body         = body2
                , format       = FormatMarkdown
                , folder       = Just folder1
                , tags         = tags1
                , baseRevision = Just initialRevisionNumber
                }
            r2 <- mustRight (mkRevisionNumber 2)
            d2.revision `shouldBe` r2

            -- メタデータのみ変更 (folder / tags のみ): リビジョンは r2 のまま増えない
            d2Meta <- mustRight =<< updateDocumentAt storage ownerId docId ts3 UpdateDocumentRequest
                { title        = t2Title
                , body         = body2
                , format       = FormatMarkdown
                , folder       = Just folder2
                , tags         = tags2
                , baseRevision = Just r2
                }
            d2Meta.revision `shouldBe` r2
            d2Meta.folder `shouldBe` Just folder2
            d2Meta.tags `shouldBe` tags2

            -- base_revision 不一致時は RevisionConflict (DB は変更されない)
            eConflict <- updateDocumentAt storage ownerId docId ts4 UpdateDocumentRequest
                { title        = t3Title
                , body         = body2
                , format       = FormatPlainText
                , folder       = Just folder2
                , tags         = tags2
                , baseRevision = Just initialRevisionNumber
                }
            eConflict `shouldBe` Left (RevisionConflict docId initialRevisionNumber r2)

            -- r3: タイトルとフォーマットのみ変更 (本文は同一なので diff は空文字列)
            d3 <- mustRight =<< updateDocumentAt storage ownerId docId ts4 UpdateDocumentRequest
                { title        = t3Title
                , body         = body2
                , format       = FormatPlainText
                , folder       = Just folder2
                , tags         = tags2
                , baseRevision = Just r2
                }
            r3 <- mustRight (mkRevisionNumber 3)
            d3.revision `shouldBe` r3

            -- リビジョン一覧 (降順: r3, r2, r1) とページネーション
            allRevs <- mustRight =<< listDocumentRevisions storage ownerId docId defaultListLimitValue defaultListOffsetValue
            allRevs.total `shouldBe` 3
            map (\ s -> s.revision) allRevs.revisions `shouldBe` [r3, r2, initialRevisionNumber]
            case allRevs.revisions of
                [s3, s2, s1] -> do
                    s1.parentRevisionId `shouldBe` Nothing
                    s2.parentRevisionId `shouldBe` Just s1.id
                    s3.parentRevisionId `shouldBe` Just s2.id
                    s1.createdAt `shouldBe` ts1
                    s2.createdAt `shouldBe` ts2
                    s3.createdAt `shouldBe` ts4
                _ -> expectationFailure "Expected exactly 3 revision summaries"

            lim1 <- mustRight (mkListLimit 1)
            off1 <- mustRight (mkListOffset 1)
            pagedRevs <- mustRight =<< listDocumentRevisions storage ownerId docId lim1 off1
            pagedRevs.total `shouldBe` 3
            map (\ s -> s.revision) pagedRevs.revisions `shouldBe` [r2]

            -- 全リビジョンの再構成と diff / contentHash 検証
            det1 <- mustRight =<< getDocumentRevision storage ownerId docId initialRevisionNumber
            det1.title `shouldBe` t1Title
            det1.format `shouldBe` FormatMarkdown
            det1.body `shouldBe` body1
            det1.diff `shouldBe` generateUnifiedDiff "" (toText body1)
            h1 <- mustRight (computeRevisionContentHash t1Title FormatMarkdown body1)
            det1.contentHash `shouldBe` h1

            det2 <- mustRight =<< getDocumentRevision storage ownerId docId r2
            det2.title `shouldBe` t2Title
            det2.format `shouldBe` FormatMarkdown
            det2.body `shouldBe` body2
            det2.diff `shouldBe` generateUnifiedDiff (toText body1) (toText body2)

            det3 <- mustRight =<< getDocumentRevision storage ownerId docId r3
            det3.title `shouldBe` t3Title
            det3.format `shouldBe` FormatPlainText
            det3.body `shouldBe` body2
            det3.diff `shouldBe` emptyUnifiedDiff

            -- 存在しないリビジョンは RevisionNotFound
            r99 <- mustRight (mkRevisionNumber 99)
            eMissing <- getDocumentRevision storage ownerId docId r99
            eMissing `shouldBe` Left (RevisionNotFound docId r99)

    it "restores historical revisions as append-only new revisions while preserving folder/tags and skipping identical restores" $
        withInMemory $ \ storage -> do
            ownerId <- seedUser storage "user-1" "alice"
            docId <- mustRight (mkDocumentId "doc-restore")
            title1 <- mustRight (mkDocumentTitle "第1版")
            title2 <- mustRight (mkDocumentTitle "第2版")
            folderLatest <- mustRight (mkFolderPath "current/folder")
            tagsLatest <- mustRight (mkTagSetFromTexts ["keep-me"])

            let ts1 :: UTCTime
                ts1 = utcAt 2026 11 21 11 0 0

                ts2 :: UTCTime
                ts2 = utcAt 2026 11 21 11 10 0

                ts3 :: UTCTime
                ts3 = utcAt 2026 11 21 11 20 0

                ts4 :: UTCTime
                ts4 = utcAt 2026 11 21 11 30 0

            _ <- mustRight =<< createDocumentWith storage ownerId docId ts1 CreateDocumentRequest
                { title  = title1
                , body   = DocumentBody "v1 line 1\nv1 line 2\n"
                , format = FormatMarkdown
                , folder = Nothing
                , tags   = emptyTagSet
                }
            d2 <- mustRight =<< updateDocumentAt storage ownerId docId ts2 UpdateDocumentRequest
                { title        = title2
                , body         = DocumentBody "v2 line 1\nv2 line 2\n"
                , format       = FormatPlainText
                , folder       = Just folderLatest
                , tags         = tagsLatest
                , baseRevision = Nothing
                }
            r2 <- mustRight (mkRevisionNumber 2)
            r3 <- mustRight (mkRevisionNumber 3)
            d2.revision `shouldBe` r2

            -- base_revision 不一致時は RevisionConflict
            eBadBase <- restoreDocumentRevisionAt storage ownerId docId initialRevisionNumber ts3 RestoreRevisionRequest
                { baseRevision = Just initialRevisionNumber
                }
            eBadBase `shouldBe` Left (RevisionConflict docId initialRevisionNumber r2)

            -- r1 へ復元 -> r3 が追記され、title/format/body は r1、folder/tags は現行 (r2) を維持
            d3 <- mustRight =<< restoreDocumentRevisionAt storage ownerId docId initialRevisionNumber ts3 RestoreRevisionRequest
                { baseRevision = Just r2
                }
            d3.revision `shouldBe` r3
            d3.title `shouldBe` title1
            d3.format `shouldBe` FormatMarkdown
            d3.body `shouldBe` DocumentBody "v1 line 1\nv1 line 2\n"
            d3.folder `shouldBe` Just folderLatest
            d3.tags `shouldBe` tagsLatest
            d3.updatedAt `shouldBe` ts3

            -- 既に同一内容 (r1 と同じ contentHash) へ再復元しても重複リビジョンを増やさない
            d3Again <- mustRight =<< restoreDocumentRevisionAt storage ownerId docId initialRevisionNumber ts4 defaultRestoreRevisionRequest
            d3Again `shouldBe` d3

            -- 復元後も r1, r2, r3 すべてが正確に再構成できる
            det1 <- mustRight =<< getDocumentRevision storage ownerId docId initialRevisionNumber
            det2 <- mustRight =<< getDocumentRevision storage ownerId docId r2
            det3 <- mustRight =<< getDocumentRevision storage ownerId docId r3
            det1.body `shouldBe` DocumentBody "v1 line 1\nv1 line 2\n"
            det2.body `shouldBe` DocumentBody "v2 line 1\nv2 line 2\n"
            det3.body `shouldBe` DocumentBody "v1 line 1\nv1 line 2\n"
            det3.diff `shouldBe` generateUnifiedDiff "v2 line 1\nv2 line 2\n" "v1 line 1\nv1 line 2\n"

            -- ゴミ箱内文書の履歴閲覧は可能、復元は DocumentInTrash
            _ <- mustRight =<< softDeleteDocumentAt storage ownerId docId ts4
            revListTrashed <- mustRight =<< listDocumentRevisions storage ownerId docId defaultListLimitValue defaultListOffsetValue
            revListTrashed.total `shouldBe` 3
            eRestoreTrashed <- restoreDocumentRevisionAt storage ownerId docId r2 ts4 defaultRestoreRevisionRequest
            eRestoreTrashed `shouldBe` Left (DocumentInTrash docId)

    it "cascades deletion of multi-revision chains on permanentDeleteDocument and detects tampered diff/hash as CorruptedData" $
        withTempDbPath "rev-integrity.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            ownerId <- mustRight =<< withStorage cfg (\ h -> Right <$> seedUser h "user-1" "alice")
            docCascade <- mustRight (mkDocumentId "doc-cascade")
            docTamper <- mustRight (mkDocumentId "doc-tamper")
            let ts1 :: UTCTime
                ts1 = utcAt 2026 11 21 12 0 0

                ts2 :: UTCTime
                ts2 = utcAt 2026 11 21 12 5 0

                ts3 :: UTCTime
                ts3 = utcAt 2026 11 21 12 10 0

            -- 複数リビジョンチェーンを持つ文書の完全削除で外部キー制約違反が起きず全行消える
            _ <- mustRight =<< withStorage cfg (\ h -> do
                _ <- mustRight =<< createDocumentWith h ownerId docCascade ts1 CreateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = DocumentBody "line 1\n"
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }
                _ <- mustRight =<< updateDocumentAt h ownerId docCascade ts2 UpdateDocumentRequest
                    { title        = emptyDocumentTitle
                    , body         = DocumentBody "line 1\nline 2\n"
                    , format       = FormatMarkdown
                    , folder       = Nothing
                    , tags         = emptyTagSet
                    , baseRevision = Nothing
                    }
                _ <- mustRight =<< updateDocumentAt h ownerId docCascade ts3 UpdateDocumentRequest
                    { title        = emptyDocumentTitle
                    , body         = DocumentBody "line 1\nline 2\nline 3\n"
                    , format       = FormatMarkdown
                    , folder       = Nothing
                    , tags         = emptyTagSet
                    , baseRevision = Nothing
                    }
                _ <- mustRight =<< softDeleteDocumentAt h ownerId docCascade ts3
                permanentDeleteDocument h ownerId docCascade)

            -- sqlite-simple の withConnection には OsPath 版がないため検証時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            remainingRevs <- SQLite.withConnection dbStr $ \ conn ->
                SQLite.query
                    conn
                    "SELECT COUNT(*) FROM document_revisions WHERE document_id = ?"
                    (Only (toText docCascade))
                    :: IO [Only Int]
            remainingRevs `shouldBe` [Only 0]

            -- 改ざん検知テスト: diff が破損していると CorruptedData を返す
            _ <- mustRight =<< withStorage cfg (\ h -> do
                _ <- mustRight =<< createDocumentWith h ownerId docTamper ts1 CreateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = DocumentBody "original\n"
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }
                updateDocumentAt h ownerId docTamper ts2 UpdateDocumentRequest
                    { title        = emptyDocumentTitle
                    , body         = DocumentBody "modified\n"
                    , format       = FormatMarkdown
                    , folder       = Nothing
                    , tags         = emptyTagSet
                    , baseRevision = Nothing
                    })
            SQLite.withConnection dbStr $ \ conn ->
                SQLite.execute
                    conn
                    "UPDATE document_revisions SET diff = 'corrupted-diff' WHERE document_id = ? AND revision = 2"
                    (Only (toText docTamper))
            eCorruptedDiff <- withStorage cfg (\ h -> getDocumentRevision h ownerId docTamper initialRevisionNumber)
            eCorruptedDiff `shouldSatisfy` isCorruptedData

            -- 改ざん検知テスト: content_hash が不一致だと CorruptedData を返す
            r2 <- mustRight (mkRevisionNumber 2)
            SQLite.withConnection dbStr $ \ conn ->
                SQLite.execute
                    conn
                    "UPDATE document_revisions SET content_hash = ? WHERE document_id = ? AND revision = 2"
                    (Txt.replicate 64 "0", toText docTamper)
            eCorruptedHash <- withStorage cfg (\ h -> getDocumentRevision h ownerId docTamper r2)
            eCorruptedHash `shouldSatisfy` isCorruptedData

            -- 文書取得自体は正常 (documents 行は壊れていない)
            docStillReadable <- mustRight =<< withStorage cfg (\ h -> getDocument h ownerId docTamper)
            docStillReadable.revision `shouldBe` r2

    it "deletes deep (>1000) revision chains via permanentDeleteDocument and user deletion without hitting SQLite trigger depth limit" $
        withTempDbPath "deep-chain.sqlite3" $ \ dbPath -> do
            let cfg :: StorageConfig
                cfg = defaultStorageConfig dbPath
            (aliceId, bobId) <- mustRight =<< withStorage cfg (\ h -> do
                a <- seedUser h "user-deep-a" "deep_alice"
                b <- seedUser h "user-deep-b" "deep_bob"
                pure (Right (a, b)))
            docAlice <- mustRight (mkDocumentId "doc-deep-alice")
            docBob <- mustRight (mkDocumentId "doc-deep-bob")
            let ts :: UTCTime
                ts = utcAt 2026 11 21 13 0 0

                tsText :: Text
                tsText = formatSqliteUtcTime ts

                zeroHash :: Text
                zeroHash = Txt.replicate 64 "a"

            _ <- mustRight =<< withStorage cfg (\ h -> createDocumentWith h aliceId docAlice ts CreateDocumentRequest
                { title  = emptyDocumentTitle
                , body   = DocumentBody ""
                , format = FormatMarkdown
                , folder = Nothing
                , tags   = emptyTagSet
                })
            _ <- mustRight =<< withStorage cfg (\ h -> createDocumentWith h bobId docBob ts CreateDocumentRequest
                { title  = emptyDocumentTitle
                , body   = DocumentBody ""
                , format = FormatMarkdown
                , folder = Nothing
                , tags   = emptyTagSet
                })

            -- 各文書に 1,100 件 (SQLITE_MAX_TRIGGER_DEPTH = 1000 超) の parent_revision_id 連鎖を構築
            -- sqlite-simple の withConnection には OsPath 版がないためテストデータ生成時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            let seedDeepChain :: Text -> Text -> IO ()
                seedDeepChain rawDocId prefix =
                    SQLite.withConnection dbStr $ \ conn -> do
                        SQLite.execute_ conn "PRAGMA foreign_keys = ON"
                        SQLite.execute_ conn "BEGIN IMMEDIATE"
                        [Only r1Id] <- SQLite.query
                            conn
                            "SELECT id FROM document_revisions WHERE document_id = ? AND revision = 1"
                            (Only rawDocId)
                            :: IO [Only Text]
                        forM_ ([2 .. 1100] :: [Int]) $ \ revNum -> do
                            let revId :: Text
                                revId = prefix <> "-r" <> Txt.pack (show revNum)

                                parentId :: Text
                                parentId
                                    | revNum == 2 = r1Id
                                    | otherwise   = prefix <> "-r" <> Txt.pack (show (revNum - 1))
                            SQLite.execute
                                conn
                                "INSERT INTO document_revisions\
                                \ (id, document_id, revision, parent_revision_id, title, format, diff, content_hash, created_at)\
                                \ VALUES (?, ?, ?, ?, '', 'markdown', '', ?, ?)"
                                (revId, rawDocId, revNum, parentId, zeroHash, tsText)
                        SQLite.execute
                            conn
                            "UPDATE documents SET revision = 1100 WHERE id = ?"
                            (Only rawDocId)
                        SQLite.execute_ conn "COMMIT"

            seedDeepChain (toText docAlice) "alice"
            seedDeepChain (toText docBob) "bob"

            -- 1. Alice の 1,100 リビジョン文書をゴミ箱移動→完全削除
            _ <- mustRight =<< withStorage cfg (\ h -> softDeleteDocumentAt h aliceId docAlice ts)
            _ <- mustRight =<< withStorage cfg (\ h -> permanentDeleteDocument h aliceId docAlice)

            -- 2. Bob のユーザー削除による 1,100 リビジョン連鎖カスケード削除
            SQLite.withConnection dbStr $ \ conn -> do
                SQLite.execute_ conn "PRAGMA foreign_keys = ON"
                SQLite.execute conn "DELETE FROM users WHERE id = ?" (Only (toText bobId))
                revCount <- SQLite.query_ conn "SELECT COUNT(*) FROM document_revisions" :: IO [Only Int]
                revCount `shouldBe` [Only 0]

    it "allows non-blocking WAL revision reads during an active write lock and detects missing/out-of-sync revision chains" $
        withTempDbPath "rev-wal-corrupt.sqlite3" $ \ dbPath -> do
            let baseCfg :: StorageConfig
                baseCfg = defaultStorageConfig dbPath
            ownerId <- mustRight =<< withStorage baseCfg (\ h -> Right <$> seedUser h "user-wal" "wal_user")
            docId <- mustRight (mkDocumentId "doc-wal")
            let ts1 :: UTCTime
                ts1 = utcAt 2026 11 21 14 0 0

                ts2 :: UTCTime
                ts2 = utcAt 2026 11 21 14 5 0

                ts3 :: UTCTime
                ts3 = utcAt 2026 11 21 14 10 0

            _ <- mustRight =<< withStorage baseCfg (\ h -> createDocumentWith h ownerId docId ts1 CreateDocumentRequest
                { title  = emptyDocumentTitle
                , body   = DocumentBody "v1\n"
                , format = FormatMarkdown
                , folder = Nothing
                , tags   = emptyTagSet
                })
            _ <- mustRight =<< withStorage baseCfg (\ h -> updateDocumentAt h ownerId docId ts2 UpdateDocumentRequest
                { title        = emptyDocumentTitle
                , body         = DocumentBody "v2\n"
                , format       = FormatMarkdown
                , folder       = Nothing
                , tags         = emptyTagSet
                , baseRevision = Nothing
                })
            _ <- mustRight =<< withStorage baseCfg (\ h -> updateDocumentAt h ownerId docId ts3 UpdateDocumentRequest
                { title        = emptyDocumentTitle
                , body         = DocumentBody "v3\n"
                , format       = FormatMarkdown
                , folder       = Nothing
                , tags         = emptyTagSet
                , baseRevision = Nothing
                })

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
            eListDuringLock <- withStorage noWaitCfg (\ h -> listDocumentRevisions h ownerId docId defaultListLimitValue defaultListOffsetValue)
            eDetailDuringLock <- withStorage noWaitCfg (\ h -> getDocumentRevision h ownerId docId initialRevisionNumber)
            putMVar releaseSignal ()
            takeMVar lockDone

            fmap (\ res -> res.total) eListDuringLock `shouldBe` Right 3
            fmap (\ det -> det.body) eDetailDuringLock `shouldBe` Right (DocumentBody "v1\n")

            -- 中間リビジョン行 (r2) が欠損している破損 DB では CorruptedData を返す
            SQLite.withConnection dbStr $ \ conn -> do
                SQLite.execute_ conn "PRAGMA foreign_keys = OFF"
                SQLite.execute
                    conn
                    "DELETE FROM document_revisions WHERE document_id = ? AND revision = 2"
                    (Only (toText docId))
            eMissingMid <- withStorage baseCfg (\ h -> getDocumentRevision h ownerId docId initialRevisionNumber)
            eMissingMid `shouldSatisfy` isCorruptedData

            -- document_revisions.revision > documents.revision の不整合を CorruptedData として検出する
            r3 <- mustRight (mkRevisionNumber 3)
            SQLite.withConnection dbStr $ \ conn ->
                SQLite.execute
                    conn
                    "UPDATE documents SET revision = 1 WHERE id = ?"
                    (Only (toText docId))
            eExceedsCurrent <- withStorage baseCfg (\ h -> getDocumentRevision h ownerId docId r3)
            eExceedsCurrent `shouldSatisfy` isCorruptedData

revisionApiSpec :: Spec
revisionApiSpec = describe "Revision history HTTP REST API endpoints" $ do
    it "serves revision listing, detail reconstruction, append-only rollback, base_revision conflicts, CSRF, and owner isolation" $
        withTestApp $ \ app -> do
            -- WaiTest.Session は Cookie Jar を自動保持するため、2 ユーザーの登録は独立セッションで行う
            alice <- WaiTest.runSession (registerTestUser "rev_alice") app
            bob <- WaiTest.runSession (registerTestUser "rev_bob") app

            WaiTest.runSession
                ( do
                    let aliceRead :: Http.RequestHeaders
                        aliceRead = [(Http.hCookie, alice.cookie)]

                        aliceWrite :: Http.RequestHeaders
                        aliceWrite = [(Http.hCookie, alice.cookie), (csrfHeaderName, alice.csrf)]

                        bobRead :: Http.RequestHeaders
                        bobRead = [(Http.hCookie, bob.cookie)]

                        bobWrite :: Http.RequestHeaders
                        bobWrite = [(Http.hCookie, bob.cookie), (csrfHeaderName, bob.csrf)]

                    -- 1. Alice creates a document (r1)
                    let createPayload :: LazyBS.ByteString
                        createPayload = Aeson.encode $ Aeson.object
                            [ "title"  Aeson..= ("初版" :: Text)
                            , "body"   Aeson..= ("line 1\nline 2\n" :: Text)
                            , "format" Aeson..= ("markdown" :: Text)
                            , "folder" Aeson..= ("docs/spec" :: Text)
                            , "tags"   Aeson..= (["m4"] :: [Text])
                            ]
                    resCreate <- sendJsonRequest Http.methodPost "/api/documents" aliceWrite (Just createPayload)
                    WaiTest.assertStatus 201 resCreate
                    doc1 <- liftDecode resCreate :: WaiTest.Session Document
                    liftEq (revisionNumberValue doc1.revision) 1

                    let docPath :: ByteString
                        docPath = "/api/documents/" <> TxtEnc.encodeUtf8 (toText doc1.id)

                        revsPath :: ByteString
                        revsPath = docPath <> "/revisions"

                    -- 2. Unauthenticated -> 401, missing CSRF on restore -> 403
                    resUnauthList <- sendJsonRequest Http.methodGet revsPath [] Nothing
                    WaiTest.assertStatus 401 resUnauthList

                    resUnauthDetail <- sendJsonRequest Http.methodGet (revsPath <> "/1") [] Nothing
                    WaiTest.assertStatus 401 resUnauthDetail

                    resNoCsrfRestore <- sendJsonRequest Http.methodPost (revsPath <> "/1/restore") aliceRead (Just "{}")
                    WaiTest.assertStatus 403 resNoCsrfRestore

                    -- 3. Update content with matching base_revision -> r2
                    let updatePayload2 :: LazyBS.ByteString
                        updatePayload2 = Aeson.encode $ Aeson.object
                            [ "title"         Aeson..= ("第2版" :: Text)
                            , "body"          Aeson..= ("line 1\nline 2 edited\nline 3\n" :: Text)
                            , "format"        Aeson..= ("plain_text" :: Text)
                            , "folder"        Aeson..= ("docs/spec/v2" :: Text)
                            , "tags"          Aeson..= (["m4", "updated"] :: [Text])
                            , "base_revision" Aeson..= (1 :: Int)
                            ]
                    resUpd2 <- sendJsonRequest Http.methodPut docPath aliceWrite (Just updatePayload2)
                    WaiTest.assertStatus 200 resUpd2
                    doc2 <- liftDecode resUpd2 :: WaiTest.Session Document
                    liftEq (revisionNumberValue doc2.revision) 2

                    -- 4. Update with stale base_revision -> 409 Conflict with field "base_revision"
                    resStalePut <- sendJsonRequest Http.methodPut docPath aliceWrite (Just updatePayload2)
                    WaiTest.assertStatus 409 resStalePut
                    staleErr <- liftDecode resStalePut :: WaiTest.Session ApiError
                    liftEq staleErr.code ErrConflict
                    liftEq staleErr.field (Just "base_revision")

                    -- 5. Metadata-only update does not create r3
                    let metaOnlyPayload :: LazyBS.ByteString
                        metaOnlyPayload = Aeson.encode $ Aeson.object
                            [ "title"         Aeson..= ("第2版" :: Text)
                            , "body"          Aeson..= ("line 1\nline 2 edited\nline 3\n" :: Text)
                            , "format"        Aeson..= ("plain_text" :: Text)
                            , "folder"        Aeson..= ("docs/archive" :: Text)
                            , "tags"          Aeson..= (["archived"] :: [Text])
                            , "base_revision" Aeson..= (2 :: Int)
                            ]
                    resMetaOnly <- sendJsonRequest Http.methodPut docPath aliceWrite (Just metaOnlyPayload)
                    WaiTest.assertStatus 200 resMetaOnly
                    doc2Meta <- liftDecode resMetaOnly :: WaiTest.Session Document
                    liftEq (revisionNumberValue doc2Meta.revision) 2

                    -- 6. Bob cannot inspect or restore Alice's revisions (404)
                    resBobList <- sendJsonRequest Http.methodGet revsPath bobRead Nothing
                    WaiTest.assertStatus 404 resBobList
                    resBobDetail <- sendJsonRequest Http.methodGet (revsPath <> "/1") bobRead Nothing
                    WaiTest.assertStatus 404 resBobDetail
                    resBobRestore <- sendJsonRequest Http.methodPost (revsPath <> "/1/restore") bobWrite (Just "{}")
                    WaiTest.assertStatus 404 resBobRestore

                    -- 7. List revisions (GET /api/documents/{id}/revisions)
                    resList <- sendJsonRequest Http.methodGet (revsPath <> "?limit=10&offset=0") aliceRead Nothing
                    WaiTest.assertStatus 200 resList
                    revList <- liftDecode resList :: WaiTest.Session RevisionListResponse
                    liftEq revList.total 2
                    liftEq (map (\ s -> revisionNumberValue s.revision) revList.revisions) [2, 1]

                    -- 8. Get historical revision detail (GET /api/documents/{id}/revisions/1)
                    resDet1 <- sendJsonRequest Http.methodGet (revsPath <> "/1") aliceRead Nothing
                    WaiTest.assertStatus 200 resDet1
                    det1 <- liftDecode resDet1 :: WaiTest.Session RevisionDetail
                    liftEq (revisionNumberValue det1.revision) 1
                    liftEq (toText det1.title) "初版"
                    liftEq det1.format FormatMarkdown
                    liftEq (toText det1.body) "line 1\nline 2\n"

                    -- 9. Invalid document ID / query params / revision param -> 400, missing revision -> 404
                    resBadDocId <- sendJsonRequest Http.methodGet "/api/documents/bad!id/revisions" aliceRead Nothing
                    WaiTest.assertStatus 400 resBadDocId
                    badDocIdErr <- liftDecode resBadDocId :: WaiTest.Session ApiError
                    liftEq badDocIdErr.code ErrValidation
                    liftEq badDocIdErr.field (Just "id")

                    resBadLimit <- sendJsonRequest Http.methodGet (revsPath <> "?limit=0") aliceRead Nothing
                    WaiTest.assertStatus 400 resBadLimit
                    badLimitErr <- liftDecode resBadLimit :: WaiTest.Session ApiError
                    liftEq badLimitErr.code ErrValidation
                    liftEq badLimitErr.field (Just "limit")

                    resBadOffset <- sendJsonRequest Http.methodGet (revsPath <> "?offset=-1") aliceRead Nothing
                    WaiTest.assertStatus 400 resBadOffset
                    badOffsetErr <- liftDecode resBadOffset :: WaiTest.Session ApiError
                    liftEq badOffsetErr.code ErrValidation
                    liftEq badOffsetErr.field (Just "offset")

                    resBadRev <- sendJsonRequest Http.methodGet (revsPath <> "/0") aliceRead Nothing
                    WaiTest.assertStatus 400 resBadRev
                    badRevErr <- liftDecode resBadRev :: WaiTest.Session ApiError
                    liftEq badRevErr.code ErrValidation
                    liftEq badRevErr.field (Just "revision")

                    resMissingRev <- sendJsonRequest Http.methodGet (revsPath <> "/99") aliceRead Nothing
                    WaiTest.assertStatus 404 resMissingRev
                    missingRevErr <- liftDecode resMissingRev :: WaiTest.Session ApiError
                    liftEq missingRevErr.code ErrNotFound

                    -- 10. Restore validation (malformed JSON / empty body / base_revision=0 -> 400), stale base_revision -> 409,
                    --     matching base_revision -> r3 (preserving folder/tags), null base_revision & self-restore -> no duplicate revision
                    resMalformedRestore <- sendJsonRequest Http.methodPost (revsPath <> "/1/restore") aliceWrite (Just "{bad-json")
                    WaiTest.assertStatus 400 resMalformedRestore

                    resEmptyRestore <- sendJsonRequest Http.methodPost (revsPath <> "/1/restore") aliceWrite (Just "")
                    WaiTest.assertStatus 400 resEmptyRestore

                    resZeroBaseRestore <- sendJsonRequest Http.methodPost (revsPath <> "/1/restore") aliceWrite (Just "{\"base_revision\":0}")
                    WaiTest.assertStatus 400 resZeroBaseRestore

                    resBadBaseRestore <- sendJsonRequest Http.methodPost (revsPath <> "/1/restore") aliceWrite (Just "{\"base_revision\":1}")
                    WaiTest.assertStatus 409 resBadBaseRestore
                    badBaseErr <- liftDecode resBadBaseRestore :: WaiTest.Session ApiError
                    liftEq badBaseErr.code ErrConflict
                    liftEq badBaseErr.field (Just "base_revision")

                    resRestore1 <- sendJsonRequest Http.methodPost (revsPath <> "/1/restore") aliceWrite (Just "{\"base_revision\":2}")
                    WaiTest.assertStatus 200 resRestore1
                    doc3 <- liftDecode resRestore1 :: WaiTest.Session Document
                    liftEq (revisionNumberValue doc3.revision) 3
                    liftEq (toText doc3.title) "初版"
                    liftEq doc3.format FormatMarkdown
                    liftEq (toText doc3.body) "line 1\nline 2\n"
                    liftEq (fmap toText doc3.folder) (Just "docs/archive")
                    liftEq doc3.tags doc2Meta.tags

                    -- 11. Restoring same content (including base_revision: null and self-restore r3 -> r3) does not create r4
                    resRestoreSame <- sendJsonRequest Http.methodPost (revsPath <> "/1/restore") aliceWrite (Just "{\"base_revision\":null}")
                    WaiTest.assertStatus 200 resRestoreSame
                    doc3Same <- liftDecode resRestoreSame :: WaiTest.Session Document
                    liftEq doc3Same doc3

                    resRestoreSelf <- sendJsonRequest Http.methodPost (revsPath <> "/3/restore") aliceWrite (Just "{}")
                    WaiTest.assertStatus 200 resRestoreSelf
                    doc3Self <- liftDecode resRestoreSelf :: WaiTest.Session Document
                    liftEq doc3Self doc3

                    -- 12. Trashed document allows revision list & detail inspection (200) but rejects rollback (409)
                    resTrash <- sendJsonRequest Http.methodDelete docPath aliceWrite Nothing
                    WaiTest.assertStatus 200 resTrash
                    trashed <- liftDecode resTrash :: WaiTest.Session Document
                    liftSatisfy trashed.deletedAt isJust

                    resTrashedRevs <- sendJsonRequest Http.methodGet revsPath aliceRead Nothing
                    WaiTest.assertStatus 200 resTrashedRevs

                    resTrashedRevDetail <- sendJsonRequest Http.methodGet (revsPath <> "/1") aliceRead Nothing
                    WaiTest.assertStatus 200 resTrashedRevDetail
                    trashedRev1 <- liftDecode resTrashedRevDetail :: WaiTest.Session RevisionDetail
                    liftEq (toText trashedRev1.body) "line 1\nline 2\n"

                    resTrashedRollback <- sendJsonRequest Http.methodPost (revsPath <> "/2/restore") aliceWrite (Just "{}")
                    WaiTest.assertStatus 409 resTrashedRollback
                    trashedRollbackErr <- liftDecode resTrashedRollback :: WaiTest.Session ApiError
                    liftEq trashedRollbackErr.code ErrConflict
                )
                app

    it "returns masked 500 JSON ApiError when stored revision diff chain is corrupted" $
        withTempDbPath "rev-api-corrupt.sqlite3" $ \ dbPath -> do
            let app :: Wai.Application
                app = mkApplicationWithAuth
                    (defaultAuthConfig { passwordHashParams = fastTestPasswordHashParams })
                    (defaultStorageConfig dbPath)
            alice <- WaiTest.runSession (registerTestUser "corrupt_api_user") app
            let aliceRead :: Http.RequestHeaders
                aliceRead = [(Http.hCookie, alice.cookie)]

                aliceWrite :: Http.RequestHeaders
                aliceWrite = [(Http.hCookie, alice.cookie), (csrfHeaderName, alice.csrf)]

            doc1 <- WaiTest.runSession
                ( do
                    res1 <- sendJsonRequest Http.methodPost "/api/documents" aliceWrite (Just "{\"body\":\"v1\\n\"}")
                    WaiTest.assertStatus 201 res1
                    d1 <- liftDecode res1 :: WaiTest.Session Document
                    let p :: ByteString
                        p = "/api/documents/" <> TxtEnc.encodeUtf8 (toText d1.id)
                    res2 <- sendJsonRequest Http.methodPut p aliceWrite (Just "{\"title\":\"\",\"body\":\"v2\\n\",\"format\":\"markdown\",\"tags\":[]}")
                    WaiTest.assertStatus 200 res2
                    pure d1
                )
                app

            -- sqlite-simple の withConnection には OsPath 版がないため破損データ注入時のみ String へ変換する
            dbStr <- OS.decodeFS dbPath
            SQLite.withConnection dbStr $ \ conn ->
                SQLite.execute
                    conn
                    "UPDATE document_revisions SET diff = 'broken-diff' WHERE document_id = ? AND revision = 2"
                    (Only (toText doc1.id))

            WaiTest.runSession
                ( do
                    let rev1Path :: ByteString
                        rev1Path = "/api/documents/" <> TxtEnc.encodeUtf8 (toText doc1.id) <> "/revisions/1"
                    res500 <- sendJsonRequest Http.methodGet rev1Path aliceRead Nothing
                    WaiTest.assertStatus 500 res500
                    err500 <- liftDecode res500 :: WaiTest.Session ApiError
                    liftEq err500.code ErrInternalServer
                    liftEq err500.message "Internal server error"
                    liftEq err500.field Nothing
                )
                app

    it "compares any two revisions with metadata flags, handles reverse/same revision, validates params, and enforces owner isolation" $
        withTestApp $ \ app -> do
            assertFieldError "from" (parseCompareRevisionsQuery Nothing (Just "2"))
            assertFieldError "to"   (parseCompareRevisionsQuery (Just "1") Nothing)
            assertFieldError "from" (parseCompareRevisionsQuery (Just "0") (Just "2"))
            assertFieldError "to"   (parseCompareRevisionsQuery (Just "1") (Just "abc"))

            WaiTest.runSession
                ( do
                    alice <- registerTestUser "cmp_alice"
                    let aliceRead :: Http.RequestHeaders
                        aliceRead = [(Http.hCookie, alice.cookie)]

                        aliceWrite :: Http.RequestHeaders
                        aliceWrite = [(Http.hCookie, alice.cookie), (csrfHeaderName, alice.csrf)]

                    let payload1 :: LazyBS.ByteString
                        payload1 = Aeson.encode $ Aeson.object
                            [ "title"  Aeson..= ("初版タイトル" :: Text)
                            , "body"   Aeson..= ("line1\nline2\n" :: Text)
                            , "format" Aeson..= ("markdown" :: Text)
                            ]

                        payload2 :: LazyBS.ByteString
                        payload2 = Aeson.encode $ Aeson.object
                            [ "title"  Aeson..= ("初版タイトル" :: Text)
                            , "body"   Aeson..= ("line1\nline2-v2\n" :: Text)
                            , "format" Aeson..= ("markdown" :: Text)
                            , "tags"   Aeson..= ([] :: [Text])
                            ]

                        payload3 :: LazyBS.ByteString
                        payload3 = Aeson.encode $ Aeson.object
                            [ "title"  Aeson..= ("改訂タイトル" :: Text)
                            , "body"   Aeson..= ("line1\nline2-v3\nline3\n" :: Text)
                            , "format" Aeson..= ("plain_text" :: Text)
                            , "tags"   Aeson..= ([] :: [Text])
                            ]

                    resCreate <- sendJsonRequest Http.methodPost "/api/documents" aliceWrite (Just payload1)
                    WaiTest.assertStatus 201 resCreate
                    doc1 <- liftDecode resCreate :: WaiTest.Session Document
                    let docPath :: ByteString
                        docPath = "/api/documents/" <> TxtEnc.encodeUtf8 (toText doc1.id)

                        cmpBase :: ByteString
                        cmpBase = docPath <> "/revisions/compare"

                    resU2 <- sendJsonRequest Http.methodPut docPath aliceWrite (Just payload2)
                    WaiTest.assertStatus 200 resU2
                    resU3 <- sendJsonRequest Http.methodPut docPath aliceWrite (Just payload3)
                    WaiTest.assertStatus 200 resU3

                    -- 1. Forward comparison r1 -> r3
                    resFwd <- sendJsonRequest Http.methodGet (cmpBase <> "?from=1&to=3") aliceRead Nothing
                    WaiTest.assertStatus 200 resFwd
                    cmpFwd <- liftDecode resFwd :: WaiTest.Session RevisionCompareResponse
                    liftEq cmpFwd.documentId doc1.id
                    liftEq (revisionNumberValue cmpFwd.fromRevision.revision) 1
                    liftEq (revisionNumberValue cmpFwd.toRevision.revision) 3
                    liftEq cmpFwd.titleChanged True
                    liftEq cmpFwd.formatChanged True
                    liftEq cmpFwd.bodyDiff (generateUnifiedDiff "line1\nline2\n" "line1\nline2-v3\nline3\n")

                    -- 2. Reverse comparison r3 -> r1
                    resRev <- sendJsonRequest Http.methodGet (cmpBase <> "?from=3&to=1") aliceRead Nothing
                    WaiTest.assertStatus 200 resRev
                    cmpRev <- liftDecode resRev :: WaiTest.Session RevisionCompareResponse
                    liftEq cmpRev.bodyDiff (generateUnifiedDiff "line1\nline2-v3\nline3\n" "line1\nline2\n")

                    -- 3. Same revision r2 -> r2 yields empty diff and no metadata changes
                    resSame <- sendJsonRequest Http.methodGet (cmpBase <> "?from=2&to=2") aliceRead Nothing
                    WaiTest.assertStatus 200 resSame
                    cmpSame <- liftDecode resSame :: WaiTest.Session RevisionCompareResponse
                    liftEq cmpSame.titleChanged False
                    liftEq cmpSame.formatChanged False
                    liftEq cmpSame.bodyDiff emptyUnifiedDiff

                    -- 4. Missing query param returns 400
                    resBad <- sendJsonRequest Http.methodGet (cmpBase <> "?from=1") aliceRead Nothing
                    WaiTest.assertStatus 400 resBad

                    -- 5. Non-existent revision returns 404 (both to and from)
                    resNoRevTo <- sendJsonRequest Http.methodGet (cmpBase <> "?from=1&to=99") aliceRead Nothing
                    WaiTest.assertStatus 404 resNoRevTo
                    resNoRevFrom <- sendJsonRequest Http.methodGet (cmpBase <> "?from=99&to=1") aliceRead Nothing
                    WaiTest.assertStatus 404 resNoRevFrom

                    -- 6. Trashed document revisions remain comparable (200)
                    resTrash <- sendJsonRequest Http.methodDelete docPath aliceWrite Nothing
                    WaiTest.assertStatus 200 resTrash
                    resTrashedCmp <- sendJsonRequest Http.methodGet (cmpBase <> "?from=1&to=3") aliceRead Nothing
                    WaiTest.assertStatus 200 resTrashedCmp

                    -- 7. Another user cannot compare Alice's revisions (404)
                    bob <- registerTestUser "cmp_bob"
                    let bobRead :: Http.RequestHeaders
                        bobRead = [(Http.hCookie, bob.cookie)]
                    resBob <- sendJsonRequest Http.methodGet (cmpBase <> "?from=1&to=3") bobRead Nothing
                    WaiTest.assertStatus 404 resBob
                    pure cmpBase
                )
                app
                >>= \ cmpBasePath ->
                    -- 8. Unauthenticated request in a clean session returns 401
                    WaiTest.runSession
                        ( do
                            resUnauth <- sendJsonRequest Http.methodGet (cmpBasePath <> "?from=1&to=3") [] Nothing
                            WaiTest.assertStatus 401 resUnauth
                        )
                        app

seedUser :: StorageHandle -> Text -> Text -> IO UserId
seedUser storage rawUid rawUname = do
    uid <- mustRight (mkUserId rawUid)
    uname <- mustRight (mkUsername rawUname)
    pw <- mustRight (mkPassword "valid-password")
    pwHash <- mustRight (hashPasswordWithSalt fastTestPasswordHashParams "1234567890abcdef" pw)
    stored <- mustRight =<< createUserWith storage uid (utcAt 2026 11 20 0 0 0) uname Nothing pwHash
    pure stored.profile.id

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

liftDecode :: Aeson.FromJSON a => WaiTest.SResponse -> WaiTest.Session a
liftDecode res =
    case Aeson.eitherDecode (WaiTest.simpleBody res) of
        Left err  -> do
            liftIO (expectationFailure ("Failed to decode JSON body: " <> err <> " (body=" <> show (WaiTest.simpleBody res) <> ")"))
            liftIO (fail "Unreachable after expectationFailure")
        Right val -> pure val

liftEq :: (Eq a, Show a) => a -> a -> WaiTest.Session ()
liftEq actual expected = liftIO (actual `shouldBe` expected)

liftSatisfy :: Show a => a -> (a -> Bool) -> WaiTest.Session ()
liftSatisfy actual predicate = liftIO (actual `shouldSatisfy` predicate)

assertFieldError :: Show a => Text -> Either ApiError a -> IO ()
assertFieldError expectedField (Left err) = do
    err.code `shouldBe` ErrValidation
    err.field `shouldBe` Just expectedField
assertFieldError expectedField (Right val) =
    expectationFailure ("Expected validation error on field " <> Txt.unpack expectedField <> ", got Right: " <> show val)

isCorruptedData :: Either StorageError a -> Bool
isCorruptedData (Left CorruptedData{}) = True
isCorruptedData _                      = False

utcAt :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
utcAt y m d hh mm ss =
    UTCTime (fromGregorian y m d) (secondsToDiffTime (fromIntegral (hh * 3600 + mm * 60 + ss)))

withInMemory :: (StorageHandle -> IO ()) -> IO ()
withInMemory action = do
    eRes <- withStorage inMemoryStorageConfig (fmap Right . action)
    _ <- mustRight eRes
    pure ()

withTestApp :: (Wai.Application -> IO a) -> IO a
withTestApp action =
    withTempDbPath "rev-api.sqlite3" $ \ dbPath ->
        action (mkApplicationWithAuth fastAuthCfg (defaultStorageConfig dbPath))
  where
    fastAuthCfg :: AuthConfig
    fastAuthCfg = defaultAuthConfig { passwordHashParams = fastTestPasswordHashParams }

withTempDbPath :: String -> (OsPath -> IO a) -> IO a
withTempDbPath relFileName action = do
    tmpRoot <- DirOS.getTemporaryDirectory
    rawUuid <- UUID4.nextRandom
    -- UUID 文字列およびテスト用ファイル名から OsPath を生成するため encodeFS を使用する
    dirPart <- OS.encodeFS ("tamatsuzura-revision-test-" <> UUID.toString rawUuid)
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
