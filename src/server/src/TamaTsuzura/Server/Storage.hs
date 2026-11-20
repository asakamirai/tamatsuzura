{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.Storage
    ( -- * ストレージハンドルとライフサイクル
      StorageHandle
    , withStorage
    , inspectStoragePragmas
      -- * 文書 CRUD (所有者分離)
    , createDocument
    , createDocumentWith
    , getDocument
    , listDocuments
    , updateDocument
    , updateDocumentAt
      -- * 論理削除 (ゴミ箱)・復元・完全削除 (所有者分離)
    , softDeleteDocument
    , softDeleteDocumentAt
    , restoreDocument
    , permanentDeleteDocument
      -- * 整理メタデータ一覧 (所有者分離)
    , listTags
    , listFolders
      -- re-export
    , StorageTarget(..)
    , BusyTimeoutMs
    , maxBusyTimeoutMs
    , mkBusyTimeoutMs
    , defaultBusyTimeoutMs
    , busyTimeoutMsValue
    , StorageConfig(..)
    , defaultStorageConfig
    , inMemoryStorageConfig
    , SqlitePragmas(..)
    , StorageError(..)
    , storageErrorToApiError
    ) where

import           Control.Exception                 (bracket)
import           Control.Monad                     (forM_, unless)
import           Data.Bifunctor                    (first)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Txt
import           Data.Time.Clock                   (UTCTime, getCurrentTime)
import qualified Data.UUID                         as UUID
import qualified Data.UUID.V4                      as UUID4
import           Database.SQLite.Simple            (NamedParam(..), Only(..))
import qualified Database.SQLite.Simple            as SQLite
import qualified System.Directory.OsPath           as DirOS
import qualified System.OsPath                     as OS
import           TamaTsuzura.Common.Api
    ( CreateDocumentRequest(..)
    , DocumentListQuery(..)
    , DocumentListResponse(..)
    , DocumentStatusFilter(..)
    , FolderListResponse(..)
    , SearchQuery
    , TagListResponse(..)
    , UpdateDocumentRequest(..)
    , listLimitValue
    , listOffsetValue
    , validateCreateDocumentRequest
    , validateUpdateDocumentRequest
    )
import           TamaTsuzura.Common.Auth           (UserId)
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody(..)
    , DocumentFormat
    , DocumentId
    , DocumentSummary(..)
    , FolderPath
    , Tag
    , TagSet
    , emptyTagSet
    , mkDocumentId
    , mkDocumentTitle
    , mkFolderPath
    , mkTag
    , mkTagSetFromTexts
    , parseDocumentFormat
    , tagSetToList
    )
import           TamaTsuzura.Common.Text           (ToText(..))
import           TamaTsuzura.Common.Validation     (ValidationError)
import           TamaTsuzura.Server.Storage.Schema
    ( StorageHandle(..)
    , asciiToLower
    , catchSqliteErrors
    , configureConnection
    , escapeLikePattern
    , formatSqliteUtcTime
    , initializeSchema
    , inspectPragmas
    , parseSqliteUtcTime
    , truncateToMicros
    , withImmediateTransaction
    , withReadTransaction
    )
import           TamaTsuzura.Server.Storage.Types
    ( BusyTimeoutMs
    , SqlitePragmas(..)
    , StorageConfig(..)
    , StorageError(..)
    , StorageTarget(..)
    , busyTimeoutMsValue
    , defaultBusyTimeoutMs
    , defaultStorageConfig
    , inMemoryStorageConfig
    , maxBusyTimeoutMs
    , mkBusyTimeoutMs
    , storageErrorToApiError
    )

type DocumentRow = (Text, Text, Text, Text, Maybe Text, Text, Text, Maybe Text)

type SummaryRow = (Text, Text, Text, Maybe Text, Text, Text, Maybe Text)

-- | 設定に従って SQLite 接続を開き、PRAGMA 設定およびスキーマ初期化を行った上で
-- アクションを実行し、終了時に必ず接続を閉じる。
-- CGI の 1 リクエスト 1 プロセスモデルと Warp 常駐モデルの双方でリクエスト単位に呼び出す前提で設計されており、
-- 初期化済みデータベースに対する読み取り操作では書き込みロックを取得しない。
withStorage ::
       StorageConfig
    -> (StorageHandle -> IO (Either StorageError a))
    -> IO (Either StorageError a)
withStorage cfg action = catchSqliteErrors $ do
    dbTargetStr <- prepareTargetPath cfg.target
    bracket (SQLite.open dbTargetStr) SQLite.close $ \ conn -> do
        eConfigured <- configureConnection cfg conn
        case eConfigured of
            Left err -> pure (Left err)
            Right () -> do
                eInitialized <- initializeSchema conn
                case eInitialized of
                    Left err -> pure (Left err)
                    Right () -> action (StorageHandle conn)

-- | 接続中の SQLite における主要 PRAGMA 設定値を取得する。
inspectStoragePragmas :: StorageHandle -> IO (Either StorageError SqlitePragmas)
inspectStoragePragmas handle = inspectPragmas handle.connection

-- | 新しい UUIDv4 の 'DocumentId' と現在の UTC 時刻を用いて指定ユーザー所有の文書を新規作成する。
createDocument ::
       StorageHandle
    -> UserId
    -> CreateDocumentRequest
    -> IO (Either StorageError Document)
createDocument handle ownerId req = do
    rawUuid <- UUID4.nextRandom
    now <- getCurrentTime
    case mkDocumentId (UUID.toText rawUuid) of
        Left err    -> pure (Left (StorageValidationError err))
        Right docId -> createDocumentWith handle ownerId docId now req

-- | 明示的に指定した所有者 'UserId'・'DocumentId'・作成日時を用いて文書を新規作成する。
-- 所有者が存在しない場合は外部キーエラーではなく 'UserNotFound' を返し、
-- 決定的な単体テストや将来のインポート・マイグレーション用途を想定する。
createDocumentWith ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> UTCTime
    -> CreateDocumentRequest
    -> IO (Either StorageError Document)
createDocumentWith handle ownerId docId now req =
    case validateCreateDocumentRequest req of
        Left err -> pure (Left (StorageValidationError err))
        Right validatedReq ->
            catchSqliteErrors $
                withImmediateTransaction handle.connection $ do
                    ownerPresent <- ownerExists handle.connection ownerId
                    if not ownerPresent
                        then pure (Left (UserNotFound ownerId))
                        else do
                            exists <- documentExists handle.connection docId
                            if exists
                                then pure (Left (DocumentIdAlreadyExists docId))
                                else do
                                    let timestamp :: UTCTime
                                        timestamp = truncateToMicros now

                                        timestampText :: Text
                                        timestampText = formatSqliteUtcTime timestamp

                                        newDoc :: Document
                                        newDoc = Document
                                            { id        = docId
                                            , title     = validatedReq.title
                                            , body      = validatedReq.body
                                            , format    = validatedReq.format
                                            , folder    = validatedReq.folder
                                            , tags      = validatedReq.tags
                                            , createdAt = timestamp
                                            , updatedAt = timestamp
                                            , deletedAt = Nothing
                                            }
                                    SQLite.execute
                                        handle.connection
                                        "INSERT INTO documents\
                                        \ (id, owner_id, title, body, format, folder, created_at, updated_at, deleted_at)\
                                        \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, NULL)"
                                        ( toText newDoc.id
                                        , toText ownerId
                                        , toText newDoc.title
                                        , toText newDoc.body
                                        , toText newDoc.format
                                        , fmap toText newDoc.folder
                                        , timestampText
                                        , timestampText
                                        )
                                    insertDocumentTags handle.connection docId newDoc.tags
                                    pure (Right newDoc)

-- | 指定ユーザーが所有する文書 (本文・タグ含む) を ID で取得する。
-- 他ユーザーの文書 ID を指定した場合は存在有無を秘匿するため 'DocumentNotFound' を返す。
-- ゴミ箱内の文書も復元・完全削除前のプレビュー用途として取得可能とする。
getDocument ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> IO (Either StorageError Document)
getDocument handle ownerId docId =
    catchSqliteErrors $
        withReadTransaction handle.connection $
            fetchDocumentInTx handle.connection ownerId docId

-- | 指定ユーザーが所有する文書を対象に、フィルタ・検索・ページネーション条件に従って
-- 文書サマリ一覧と総一致件数を取得する。
-- 一覧取得時は本文列 (@body@) を読み込まず、タグは対象文書分を 1 クエリで一括取得して N+1 を回避する。
-- 検索条件 (@q@) は SQLite 標準の @lower()@ / @LIKE@ に準拠した ASCII 大文字小文字無視＋部分一致を行う。
listDocuments ::
       StorageHandle
    -> UserId
    -> DocumentListQuery
    -> IO (Either StorageError DocumentListResponse)
listDocuments handle ownerId query =
    catchSqliteErrors $
        withReadTransaction handle.connection $ do
            let (whereSql, filterParams) = buildListFilter ownerId query
                countSql :: SQLite.Query
                countSql = SQLite.Query ("SELECT COUNT(*) FROM documents d" <> whereSql)

                selectSql :: SQLite.Query
                selectSql = SQLite.Query
                    ( "SELECT d.id, d.title, d.format, d.folder, d.created_at, d.updated_at, d.deleted_at\
                      \ FROM documents d"
                        <> whereSql
                        <> " ORDER BY d.updated_at DESC, d.id ASC LIMIT :limit OFFSET :offset"
                    )

                selectParams :: [NamedParam]
                selectParams =
                    filterParams
                        <> [ ":limit"  := listLimitValue query.limit
                           , ":offset" := listOffsetValue query.offset
                           ]
            countRows <- SQLite.queryNamed handle.connection countSql filterParams :: IO [Only Int]
            case countRows of
                [Only totalCount] -> do
                    summaryRows <- SQLite.queryNamed handle.connection selectSql selectParams :: IO [SummaryRow]
                    case traverse decodeSummaryBase summaryRows of
                        Left err -> pure (Left err)
                        Right baseSummaries -> do
                            let docIds :: [DocumentId]
                                docIds = map (\ s -> s.id) baseSummaries
                            eTagMap <- fetchTagsByDocumentIds handle.connection docIds
                            pure $ case eTagMap of
                                Left err -> Left err
                                Right tagMap ->
                                    let summaries :: [DocumentSummary]
                                        summaries = map (attachTags tagMap) baseSummaries
                                    in  Right DocumentListResponse
                                            { documents = summaries
                                            , total     = totalCount
                                            }
                _ ->
                    pure (Left (CorruptedData "Unexpected row count from SELECT COUNT(*)"))
  where
    attachTags :: Map DocumentId TagSet -> DocumentSummary -> DocumentSummary
    attachTags tagMap summary = DocumentSummary
        { id        = summary.id
        , title     = summary.title
        , format    = summary.format
        , folder    = summary.folder
        , tags      = fromMaybe emptyTagSet (Map.lookup summary.id tagMap)
        , createdAt = summary.createdAt
        , updatedAt = summary.updatedAt
        , deletedAt = summary.deletedAt
        }

-- | 現在の UTC 時刻を用いて指定ユーザー所有の稼働中文書の内容・メタデータを全体置換する。
updateDocument ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> UpdateDocumentRequest
    -> IO (Either StorageError Document)
updateDocument handle ownerId docId req = do
    now <- getCurrentTime
    updateDocumentAt handle ownerId docId now req

-- | 明示的に指定した更新日時を用いて指定ユーザー所有の稼働中文書の内容・メタデータを全体置換する。
-- 他ユーザーの文書は 'DocumentNotFound' (HTTP 404)、本人所有かつゴミ箱内の文書は 'DocumentInTrash' (HTTP 409) とする。
updateDocumentAt ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> UTCTime
    -> UpdateDocumentRequest
    -> IO (Either StorageError Document)
updateDocumentAt handle ownerId docId now req =
    case validateUpdateDocumentRequest req of
        Left err -> pure (Left (StorageValidationError err))
        Right validatedReq ->
            catchSqliteErrors $
                withImmediateTransaction handle.connection $ do
                    rows <- SQLite.query
                        handle.connection
                        "SELECT created_at, deleted_at FROM documents WHERE id = ? AND owner_id = ?"
                        (toText docId, toText ownerId)
                        :: IO [(Text, Maybe Text)]
                    case rows of
                        []                        -> pure (Left (DocumentNotFound docId))
                        [(_, Just _)]             -> pure (Left (DocumentInTrash docId))
                        [(rawCreatedAt, Nothing)] ->
                            case parseSqliteUtcTime rawCreatedAt of
                                Left err        -> pure (Left err)
                                Right createdAt -> do
                                    let updatedAt :: UTCTime
                                        updatedAt = truncateToMicros now

                                        updatedDoc :: Document
                                        updatedDoc = Document
                                            { id        = docId
                                            , title     = validatedReq.title
                                            , body      = validatedReq.body
                                            , format    = validatedReq.format
                                            , folder    = validatedReq.folder
                                            , tags      = validatedReq.tags
                                            , createdAt = createdAt
                                            , updatedAt = updatedAt
                                            , deletedAt = Nothing
                                            }
                                    SQLite.execute
                                        handle.connection
                                        "UPDATE documents\
                                        \ SET title = ?, body = ?, format = ?, folder = ?, updated_at = ?\
                                        \ WHERE id = ? AND owner_id = ?"
                                        ( toText updatedDoc.title
                                        , toText updatedDoc.body
                                        , toText updatedDoc.format
                                        , fmap toText updatedDoc.folder
                                        , formatSqliteUtcTime updatedAt
                                        , toText docId
                                        , toText ownerId
                                        )
                                    SQLite.execute
                                        handle.connection
                                        "DELETE FROM document_tags WHERE document_id = ?"
                                        (Only (toText docId))
                                    insertDocumentTags handle.connection docId updatedDoc.tags
                                    pure (Right updatedDoc)
                        _                         ->
                            pure (Left (CorruptedData ("Duplicate primary key for document: " <> toText docId)))

-- | 現在の UTC 時刻を用いて指定ユーザー所有の文書を論理削除 (ゴミ箱へ移動) する。
softDeleteDocument ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> IO (Either StorageError Document)
softDeleteDocument handle ownerId docId = do
    now <- getCurrentTime
    softDeleteDocumentAt handle ownerId docId now

-- | 明示的に指定した削除日時を用いて指定ユーザー所有の文書を論理削除 (ゴミ箱へ移動) する。
-- 冪等性を保証し、既にゴミ箱内の文書については既存の @deleted_at@ を保持して返却する。
-- 復元時の並び順・最終編集日時を維持するため、@updated_at@ は変更しない。
softDeleteDocumentAt ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> UTCTime
    -> IO (Either StorageError Document)
softDeleteDocumentAt handle ownerId docId now =
    catchSqliteErrors $
        withImmediateTransaction handle.connection $ do
            eDoc <- fetchDocumentInTx handle.connection ownerId docId
            case eDoc of
                Left err -> pure (Left err)
                Right doc -> case doc.deletedAt of
                    Just _ -> pure (Right doc)
                    Nothing -> do
                        let deletedAt :: UTCTime
                            deletedAt = truncateToMicros now
                        SQLite.execute
                            handle.connection
                            "UPDATE documents SET deleted_at = ? WHERE id = ? AND owner_id = ?"
                            (formatSqliteUtcTime deletedAt, toText docId, toText ownerId)
                        pure (Right (setDocumentDeletedAt (Just deletedAt) doc))

-- | 指定ユーザー所有のゴミ箱内文書を稼働状態へ復元する (@deleted_at = NULL@)。
-- 冪等性を保証し、既に稼働中の文書についてはそのまま返却する。@updated_at@ は変更しない。
restoreDocument ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> IO (Either StorageError Document)
restoreDocument handle ownerId docId =
    catchSqliteErrors $
        withImmediateTransaction handle.connection $ do
            eDoc <- fetchDocumentInTx handle.connection ownerId docId
            case eDoc of
                Left err -> pure (Left err)
                Right doc -> case doc.deletedAt of
                    Nothing -> pure (Right doc)
                    Just _ -> do
                        SQLite.execute
                            handle.connection
                            "UPDATE documents SET deleted_at = NULL WHERE id = ? AND owner_id = ?"
                            (toText docId, toText ownerId)
                        pure (Right (setDocumentDeletedAt Nothing doc))

-- | 指定ユーザー所有のゴミ箱内文書をストレージから完全削除する。
-- 他ユーザーの文書は 'DocumentNotFound' (HTTP 404)、本人所有かつ稼働中の文書は誤削除防止のため
-- 'DocumentNotTrashed' (HTTP 409) として拒否する。
-- 関連タグ行は外部キーの @ON DELETE CASCADE@ により同時に削除される。
permanentDeleteDocument ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> IO (Either StorageError ())
permanentDeleteDocument handle ownerId docId =
    catchSqliteErrors $
        withImmediateTransaction handle.connection $ do
            rows <- SQLite.query
                handle.connection
                "SELECT deleted_at FROM documents WHERE id = ? AND owner_id = ?"
                (toText docId, toText ownerId)
                :: IO [Only (Maybe Text)]
            case rows of
                []             -> pure (Left (DocumentNotFound docId))
                [Only Nothing] -> pure (Left (DocumentNotTrashed docId))
                [Only Just{}]  -> do
                    SQLite.execute
                        handle.connection
                        "DELETE FROM documents WHERE id = ? AND owner_id = ?"
                        (toText docId, toText ownerId)
                    pure (Right ())
                _              -> pure (Left (CorruptedData ("Duplicate primary key for document: " <> toText docId)))

-- | 指定ユーザーの稼働中文書で使用されている重複なし・昇順ソート済みのタグ一覧を返す。
listTags :: StorageHandle -> UserId -> IO (Either StorageError TagListResponse)
listTags handle ownerId = catchSqliteErrors $ do
    rows <- SQLite.query
        handle.connection
        "SELECT DISTINCT dt.tag\
        \ FROM document_tags dt\
        \ INNER JOIN documents d ON d.id = dt.document_id\
        \ WHERE d.owner_id = ? AND d.deleted_at IS NULL\
        \ ORDER BY dt.tag ASC"
        (Only (toText ownerId))
        :: IO [Only Text]
    pure $ do
        tags <- traverse decodeTag rows
        Right TagListResponse
            { tags = tags
            }
  where
    decodeTag :: Only Text -> Either StorageError Tag
    decodeTag (Only rawTag) = mapValidationToCorrupted "tag" (mkTag rawTag)

-- | 指定ユーザーの稼働中文書で使用されている重複なし・昇順ソート済みのフォルダパス一覧を返す。
-- ルート直下 (@folder IS NULL@) の文書は結果に含めない。
listFolders :: StorageHandle -> UserId -> IO (Either StorageError FolderListResponse)
listFolders handle ownerId = catchSqliteErrors $ do
    rows <- SQLite.query
        handle.connection
        "SELECT DISTINCT folder\
        \ FROM documents\
        \ WHERE owner_id = ? AND deleted_at IS NULL AND folder IS NOT NULL\
        \ ORDER BY folder ASC"
        (Only (toText ownerId))
        :: IO [Only Text]
    pure $ do
        folders <- traverse decodeFolder rows
        Right FolderListResponse
            { folders = folders
            }
  where
    decodeFolder :: Only Text -> Either StorageError FolderPath
    decodeFolder (Only rawFolder) = mapValidationToCorrupted "folder" (mkFolderPath rawFolder)

prepareTargetPath :: StorageTarget -> IO String
prepareTargetPath StorageMemory      = pure ":memory:"
prepareTargetPath (StorageFile path) = do
    let parentDir :: OS.OsPath
        parentDir = OS.takeDirectory path
    unless (parentDir == mempty) $
        DirOS.createDirectoryIfMissing True parentDir
    -- sqlite-simple の open には OsPath 版が存在しないため、境界でのみ String へ変換する
    OS.decodeFS path

ownerExists :: SQLite.Connection -> UserId -> IO Bool
ownerExists conn ownerId = do
    rows <- SQLite.query
        conn
        "SELECT 1 FROM users WHERE id = ? LIMIT 1"
        (Only (toText ownerId))
        :: IO [Only Int]
    pure (not (null rows))

documentExists :: SQLite.Connection -> DocumentId -> IO Bool
documentExists conn docId = do
    rows <- SQLite.query
        conn
        "SELECT 1 FROM documents WHERE id = ? LIMIT 1"
        (Only (toText docId))
        :: IO [Only Int]
    pure (not (null rows))

insertDocumentTags :: SQLite.Connection -> DocumentId -> TagSet -> IO ()
insertDocumentTags conn docId tagSet =
    forM_ (tagSetToList tagSet) $ \ tag ->
        SQLite.execute
            conn
            "INSERT INTO document_tags (document_id, tag) VALUES (?, ?)"
            (toText docId, toText tag)

fetchDocumentInTx :: SQLite.Connection -> UserId -> DocumentId -> IO (Either StorageError Document)
fetchDocumentInTx conn ownerId docId = do
    rows <- SQLite.query
        conn
        "SELECT id, title, body, format, folder, created_at, updated_at, deleted_at\
        \ FROM documents\
        \ WHERE id = ? AND owner_id = ?"
        (toText docId, toText ownerId)
        :: IO [DocumentRow]
    case rows of
        []    -> pure (Left (DocumentNotFound docId))
        [row] -> do
            tagRows <- SQLite.query
                conn
                "SELECT tag FROM document_tags WHERE document_id = ? ORDER BY tag ASC"
                (Only (toText docId))
                :: IO [Only Text]
            let rawTags :: [Text]
                rawTags = map (\ (Only t) -> t) tagRows
            pure (decodeDocumentRow row rawTags)
        _     -> pure (Left (CorruptedData ("Duplicate primary key for document: " <> toText docId)))

fetchTagsByDocumentIds ::
       SQLite.Connection
    -> [DocumentId]
    -> IO (Either StorageError (Map DocumentId TagSet))
fetchTagsByDocumentIds _ [] = pure (Right Map.empty)
fetchTagsByDocumentIds conn docIds = do
    let placeholders :: Text
        placeholders = Txt.intercalate ", " (replicate (length docIds) "?")

        sql :: SQLite.Query
        sql = SQLite.Query
            ( "SELECT document_id, tag FROM document_tags WHERE document_id IN ("
                <> placeholders
                <> ") ORDER BY document_id ASC, tag ASC"
            )
    rows <- SQLite.query conn sql (map toText docIds) :: IO [(Text, Text)]
    pure $ do
        decodedPairs <- traverse decodePair rows
        let grouped :: Map DocumentId [Text]
            grouped = foldl' appendTag Map.empty decodedPairs
        traverse (mapValidationToCorrupted "tags" . mkTagSetFromTexts) grouped
  where
    decodePair :: (Text, Text) -> Either StorageError (DocumentId, Text)
    decodePair (rawDocId, rawTag) = do
        docId <- mapValidationToCorrupted "document_id" (mkDocumentId rawDocId)
        Right (docId, rawTag)

    appendTag :: Map DocumentId [Text] -> (DocumentId, Text) -> Map DocumentId [Text]
    appendTag acc (docId, rawTag) = Map.insertWith (<>) docId [rawTag] acc

buildListFilter :: UserId -> DocumentListQuery -> (Text, [NamedParam])
buildListFilter ownerId query =
    let (clauses, params) =
            mconcat
                [ ownerClause ownerId
                , statusClause query.status
                , tagClause query.tag
                , folderClause query.folder query.includeSubfolders
                , searchClause query.search
                ]
        whereSql :: Text
        whereSql = " WHERE " <> Txt.intercalate " AND " clauses
    in  (whereSql, params)
  where
    ownerClause :: UserId -> ([Text], [NamedParam])
    ownerClause uid = (["d.owner_id = :ownerId"], [":ownerId" := toText uid])

    statusClause :: DocumentStatusFilter -> ([Text], [NamedParam])
    statusClause StatusActive  = (["d.deleted_at IS NULL"], [])
    statusClause StatusTrashed = (["d.deleted_at IS NOT NULL"], [])
    statusClause StatusAll     = ([], [])

    tagClause :: Maybe Tag -> ([Text], [NamedParam])
    tagClause Nothing    = ([], [])
    tagClause (Just tag) =
        ( ["EXISTS (SELECT 1 FROM document_tags dt WHERE dt.document_id = d.id AND dt.tag = :tag)"]
        , [":tag" := toText tag]
        )

    folderClause :: Maybe FolderPath -> Bool -> ([Text], [NamedParam])
    folderClause Nothing   _     = ([], [])
    folderClause (Just fp) False =
        ( ["d.folder = :folder"]
        , [":folder" := toText fp]
        )
    folderClause (Just fp) True  =
        -- ASCII では '/' (0x2F) の直後が '0' (0x30) であるため、BINARY 照合順における
        -- [prefix <> "/", prefix <> "0") は大文字小文字を区別する前方一致 isFolderPrefixOf と同値になり、
        -- LIKE のワイルドカード問題や ASCII case-insensitive 挙動を避けつつ B-tree インデックスを利用できる。
        let prefix :: Text
            prefix = toText fp
        in  ( ["(d.folder = :folder OR (d.folder >= :folderLower AND d.folder < :folderUpper))"]
            , [ ":folder"      := prefix
              , ":folderLower" := (prefix <> "/")
              , ":folderUpper" := (prefix <> "0")
              ]
            )

    searchClause :: Maybe SearchQuery -> ([Text], [NamedParam])
    searchClause Nothing            = ([], [])
    searchClause (Just searchQuery) =
        ( ["(lower(d.title) LIKE :searchPattern ESCAPE '\\' OR lower(d.body) LIKE :searchPattern ESCAPE '\\')"]
        , [":searchPattern" := escapeLikePattern (asciiToLower (toText searchQuery))]
        )

decodeDocumentRow :: DocumentRow -> [Text] -> Either StorageError Document
decodeDocumentRow (rawId, rawTitle, rawBody, rawFormat, mRawFolder, rawCreatedAt, rawUpdatedAt, mRawDeletedAt) rawTags = do
    docId     <- mapValidationToCorrupted "id" (mkDocumentId rawId)
    title     <- mapValidationToCorrupted "title" (mkDocumentTitle rawTitle)
    format    <- decodeFormat rawFormat
    mFolder   <- traverse (mapValidationToCorrupted "folder" . mkFolderPath) mRawFolder
    tags      <- mapValidationToCorrupted "tags" (mkTagSetFromTexts rawTags)
    createdAt <- parseSqliteUtcTime rawCreatedAt
    updatedAt <- parseSqliteUtcTime rawUpdatedAt
    mDeleted  <- traverse parseSqliteUtcTime mRawDeletedAt
    Right Document
        { id        = docId
        , title     = title
        , body      = DocumentBody rawBody
        , format    = format
        , folder    = mFolder
        , tags      = tags
        , createdAt = createdAt
        , updatedAt = updatedAt
        , deletedAt = mDeleted
        }

decodeSummaryBase :: SummaryRow -> Either StorageError DocumentSummary
decodeSummaryBase (rawId, rawTitle, rawFormat, mRawFolder, rawCreatedAt, rawUpdatedAt, mRawDeletedAt) = do
    docId     <- mapValidationToCorrupted "id" (mkDocumentId rawId)
    title     <- mapValidationToCorrupted "title" (mkDocumentTitle rawTitle)
    format    <- decodeFormat rawFormat
    mFolder   <- traverse (mapValidationToCorrupted "folder" . mkFolderPath) mRawFolder
    createdAt <- parseSqliteUtcTime rawCreatedAt
    updatedAt <- parseSqliteUtcTime rawUpdatedAt
    mDeleted  <- traverse parseSqliteUtcTime mRawDeletedAt
    Right DocumentSummary
        { id        = docId
        , title     = title
        , format    = format
        , folder    = mFolder
        , tags      = emptyTagSet
        , createdAt = createdAt
        , updatedAt = updatedAt
        , deletedAt = mDeleted
        }

decodeFormat :: Text -> Either StorageError DocumentFormat
decodeFormat raw = mapValidationToCorrupted "format" (parseDocumentFormat raw)

mapValidationToCorrupted :: Text -> Either ValidationError a -> Either StorageError a
mapValidationToCorrupted columnName =
    first (\ err -> CorruptedData ("Invalid " <> columnName <> " in database: " <> toText err))

setDocumentDeletedAt :: Maybe UTCTime -> Document -> Document
setDocumentDeletedAt mDeletedAt doc = Document
    { id        = doc.id
    , title     = doc.title
    , body      = doc.body
    , format    = doc.format
    , folder    = doc.folder
    , tags      = doc.tags
    , createdAt = doc.createdAt
    , updatedAt = doc.updatedAt
    , deletedAt = mDeletedAt
    }
