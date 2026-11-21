{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.Storage.Revision
    ( -- * コンテンツハッシュ計算
      computeRevisionContentHash
      -- * トランザクション内リビジョン操作 (内部共有)
    , insertInitialRevisionInTx
    , appendRevisionInTx
    , reconstructRevisionDetailInTx
      -- * リビジョン履歴取得 (所有者分離)
    , listDocumentRevisions
    , getDocumentRevision
    ) where

import           Crypto.Hash                       (Digest, SHA256(..))
import qualified Crypto.Hash                       as Hash
import           Data.Bifunctor                    (first)
import           Data.ByteArray.Encoding           (Base(..))
import qualified Data.ByteArray.Encoding           as BAEnc
import           Data.ByteString                   (ByteString)
import           Data.Text                         (Text)
import qualified Data.Text                         as Txt
import qualified Data.Text.Encoding                as TxtEnc
import           Data.Time.Clock                   (UTCTime)
import qualified Data.UUID                         as UUID
import qualified Data.UUID.V4                      as UUID4
import           Database.SQLite.Simple            (Only(..))
import qualified Database.SQLite.Simple            as SQLite
import           TamaTsuzura.Common.Api
    ( ListLimit
    , ListOffset
    , listLimitValue
    , listOffsetValue
    )
import           TamaTsuzura.Common.Auth           (UserId)
import           TamaTsuzura.Common.Diff
    ( UnifiedDiff(..)
    , applyUnifiedDiffReverse
    , generateUnifiedDiff
    )
import           TamaTsuzura.Common.Document
    ( DocumentBody(..)
    , DocumentFormat
    , DocumentId
    , DocumentTitle
    , RevisionNumber
    , initialRevisionNumber
    , mkDocumentId
    , mkDocumentTitle
    , mkRevisionNumber
    , nextRevisionNumber
    , parseDocumentFormat
    , revisionNumberValue
    )
import           TamaTsuzura.Common.Revision
    ( ContentHash
    , RevisionDetail(..)
    , RevisionId
    , RevisionListResponse(..)
    , RevisionSummary(..)
    , mkContentHash
    , mkRevisionId
    , revisionContentCanonicalText
    )
import           TamaTsuzura.Common.Text           (ToText(..))
import           TamaTsuzura.Server.Storage.Schema
    ( StorageHandle(..)
    , catchSqliteErrors
    , formatSqliteUtcTime
    , mapValidationToCorrupted
    , parseSqliteUtcTime
    , withReadTransaction
    )
import           TamaTsuzura.Server.Storage.Types  (StorageError(..))

type RevisionSummaryRow = (Text, Text, Int, Maybe Text, Text, Text, Text, Text)

type RevisionDetailRow = (Text, Text, Int, Maybe Text, Text, Text, Text, Text, Text)

type CurrentDocMetaRow = (Text, Int)

-- | タイトル・フォーマット・本文の正規化表現から SHA-256 の 'ContentHash' を算出する。
-- SHA-256 の Base16 出力は常に 64 文字の小文字 16 進数であるが、部分関数を避けるため
-- 'mkContentHash' 経由で検証し 'Either' として返す。
computeRevisionContentHash ::
       DocumentTitle
    -> DocumentFormat
    -> DocumentBody
    -> Either StorageError ContentHash
computeRevisionContentHash title format body =
    let canonical :: Text
        canonical = revisionContentCanonicalText title format body

        digest :: Digest SHA256
        digest = Hash.hashWith SHA256 (TxtEnc.encodeUtf8 canonical)

        hexBytes :: ByteString
        hexBytes = BAEnc.convertToBase Base16 digest

        hexText :: Text
        hexText = TxtEnc.decodeLatin1 hexBytes
    in  first
            (\ err -> CorruptedData ("Generated invalid SHA-256 content hash: " <> toText err))
            (mkContentHash hexText)

-- | 文書新規作成トランザクション内で初期リビジョン (revision = 1) を @document_revisions@ へ挿入する。
-- 初期リビジョンの @parent_revision_id@ は @NULL@、@diff@ は空文書から初期本文への unified diff とする。
insertInitialRevisionInTx ::
       SQLite.Connection
    -> DocumentId
    -> DocumentTitle
    -> DocumentFormat
    -> DocumentBody
    -> UTCTime
    -> IO (Either StorageError RevisionId)
insertInitialRevisionInTx conn docId title format body createdAt =
    case computeRevisionContentHash title format body of
        Left err -> pure (Left err)
        Right contentHash -> do
            eRevId <- generateRevisionId
            case eRevId of
                Left err -> pure (Left err)
                Right revId -> do
                    let initialDiff :: UnifiedDiff
                        initialDiff = generateUnifiedDiff "" (toText body)
                    SQLite.execute
                        conn
                        "INSERT INTO document_revisions\
                        \ (id, document_id, revision, parent_revision_id, title, format, diff, content_hash, created_at)\
                        \ VALUES (?, ?, ?, NULL, ?, ?, ?, ?, ?)"
                        ( toText revId
                        , toText docId
                        , revisionNumberValue initialRevisionNumber
                        , toText title
                        , toText format
                        , toText initialDiff
                        , toText contentHash
                        , formatSqliteUtcTime createdAt
                        )
                    pure (Right revId)

-- | 文書更新・復元トランザクション内で次リビジョン (@currentRev + 1@) を追記する。
-- 親リビジョン (@currentRev@) の ID を @document_revisions@ から参照し、
-- 旧本文から新本文への unified diff と新内容の 'ContentHash' を保存する。
appendRevisionInTx ::
       SQLite.Connection
    -> DocumentId
    -> RevisionNumber
    -> DocumentBody
    -> DocumentTitle
    -> DocumentFormat
    -> DocumentBody
    -> UTCTime
    -> IO (Either StorageError (RevisionId, RevisionNumber))
appendRevisionInTx conn docId currentRev oldBody newTitle newFormat newBody createdAt =
    case computeRevisionContentHash newTitle newFormat newBody of
        Left err -> pure (Left err)
        Right contentHash -> do
            eParentId <- fetchRevisionIdInTx conn docId currentRev
            case eParentId of
                Left err -> pure (Left err)
                Right parentRevId -> do
                    eRevId <- generateRevisionId
                    case eRevId of
                        Left err -> pure (Left err)
                        Right revId -> do
                            let nextRev :: RevisionNumber
                                nextRev = nextRevisionNumber currentRev

                                bodyDiff :: UnifiedDiff
                                bodyDiff = generateUnifiedDiff (toText oldBody) (toText newBody)
                            SQLite.execute
                                conn
                                "INSERT INTO document_revisions\
                                \ (id, document_id, revision, parent_revision_id, title, format, diff, content_hash, created_at)\
                                \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                                ( toText revId
                                , toText docId
                                , revisionNumberValue nextRev
                                , toText parentRevId
                                , toText newTitle
                                , toText newFormat
                                , toText bodyDiff
                                , toText contentHash
                                , formatSqliteUtcTime createdAt
                                )
                            pure (Right (revId, nextRev))

-- | 指定ユーザーが所有する文書のリビジョン履歴サマリ一覧を降順 (新しい順) で取得する。
-- 他ユーザーの文書または存在しない文書は 'DocumentNotFound' を返す。
-- 本文・差分テキストを読み込まないため、モバイル環境でも高速に応答できる。
listDocumentRevisions ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> ListLimit
    -> ListOffset
    -> IO (Either StorageError RevisionListResponse)
listDocumentRevisions handle ownerId docId limit offset =
    catchSqliteErrors $
        withReadTransaction handle.connection $ do
            owned <- documentOwnedByInTx handle.connection ownerId docId
            if not owned
                then pure (Left (DocumentNotFound docId))
                else do
                    countRows <- SQLite.query
                        handle.connection
                        "SELECT COUNT(*) FROM document_revisions WHERE document_id = ?"
                        (Only (toText docId))
                        :: IO [Only Int]
                    case countRows of
                        [Only totalCount] -> do
                            rows <- SQLite.query
                                handle.connection
                                "SELECT id, document_id, revision, parent_revision_id, title, format, content_hash, created_at\
                                \ FROM document_revisions\
                                \ WHERE document_id = ?\
                                \ ORDER BY revision DESC\
                                \ LIMIT ? OFFSET ?"
                                (toText docId, listLimitValue limit, listOffsetValue offset)
                                :: IO [RevisionSummaryRow]
                            pure $ do
                                summaries <- traverse decodeRevisionSummaryRow rows
                                Right RevisionListResponse
                                    { revisions = summaries
                                    , total     = totalCount
                                    }
                        _ ->
                            pure (Left (CorruptedData "Unexpected row count from SELECT COUNT(*) on document_revisions"))

-- | 指定ユーザーが所有する文書の特定リビジョン詳細 (再構成済み本文と unified diff) を取得する。
-- 最新本文から対象リビジョンまで保存済み unified diff を逆適用して本文を再構成し、
-- 保存済み 'ContentHash' と照合することで再構成の完全性を検証する。
getDocumentRevision ::
       StorageHandle
    -> UserId
    -> DocumentId
    -> RevisionNumber
    -> IO (Either StorageError RevisionDetail)
getDocumentRevision handle ownerId docId targetRev =
    catchSqliteErrors $
        withReadTransaction handle.connection $ do
            eMeta <- fetchCurrentDocMetaInTx handle.connection ownerId docId
            case eMeta of
                Left err -> pure (Left err)
                Right (currentBody, currentRev) ->
                    reconstructRevisionDetailInTx handle.connection docId currentBody currentRev targetRev

-- | トランザクション内で指定リビジョン番号の 'RevisionDetail' を再構成・整合性検証する。
-- 復元操作 ('restoreDocumentRevisionAt') と詳細取得 ('getDocumentRevision') の双方から共有される。
reconstructRevisionDetailInTx ::
       SQLite.Connection
    -> DocumentId
    -> DocumentBody
    -> RevisionNumber
    -> RevisionNumber
    -> IO (Either StorageError RevisionDetail)
reconstructRevisionDetailInTx conn docId currentBody currentRev targetRev = do
    rows <- SQLite.query
        conn
        "SELECT id, document_id, revision, parent_revision_id, title, format, diff, content_hash, created_at\
        \ FROM document_revisions\
        \ WHERE document_id = ? AND revision = ?"
        (toText docId, revisionNumberValue targetRev)
        :: IO [RevisionDetailRow]
    case rows of
        [] ->
            pure (Left (RevisionNotFound docId targetRev))
        [row] ->
            case decodeRevisionRowBase row of
                Left err -> pure (Left err)
                Right (summary, storedDiff) ->
                    if targetRev > currentRev
                        then pure $ Left $ CorruptedData
                            ( "Stored revision "
                                <> toText targetRev
                                <> " exceeds document current revision "
                                <> toText currentRev
                                <> " for document: "
                                <> toText docId
                            )
                        else do
                            eReconstructedBody <-
                                if targetRev == currentRev
                                    then pure (Right currentBody)
                                    else rewindBodyToRevisionInTx conn docId currentBody currentRev targetRev
                            pure $ do
                                body <- eReconstructedBody
                                computedHash <- computeRevisionContentHash summary.title summary.format body
                                if computedHash /= summary.contentHash
                                    then Left $ CorruptedData
                                        ( "Reconstructed revision content hash mismatch for "
                                            <> toText docId
                                            <> "@r"
                                            <> toText targetRev
                                        )
                                    else Right RevisionDetail
                                        { id               = summary.id
                                        , documentId       = summary.documentId
                                        , revision         = summary.revision
                                        , parentRevisionId = summary.parentRevisionId
                                        , title            = summary.title
                                        , body             = body
                                        , format           = summary.format
                                        , diff             = storedDiff
                                        , contentHash      = summary.contentHash
                                        , createdAt        = summary.createdAt
                                        }
        _ ->
            pure (Left (CorruptedData ("Duplicate revision " <> toText targetRev <> " for document: " <> toText docId)))

generateRevisionId :: IO (Either StorageError RevisionId)
generateRevisionId =
    first
        (\ err -> CorruptedData ("Generated invalid revision UUID: " <> toText err))
        . mkRevisionId
        . UUID.toText
        <$> UUID4.nextRandom

documentOwnedByInTx :: SQLite.Connection -> UserId -> DocumentId -> IO Bool
documentOwnedByInTx conn ownerId docId = do
    rows <- SQLite.query
        conn
        "SELECT 1 FROM documents WHERE id = ? AND owner_id = ? LIMIT 1"
        (toText docId, toText ownerId)
        :: IO [Only Int]
    pure (not (null rows))

fetchCurrentDocMetaInTx ::
       SQLite.Connection
    -> UserId
    -> DocumentId
    -> IO (Either StorageError (DocumentBody, RevisionNumber))
fetchCurrentDocMetaInTx conn ownerId docId = do
    rows <- SQLite.query
        conn
        "SELECT body, revision FROM documents WHERE id = ? AND owner_id = ?"
        (toText docId, toText ownerId)
        :: IO [CurrentDocMetaRow]
    pure $ case rows of
        [] ->
            Left (DocumentNotFound docId)
        [(rawBody, rawRev)] -> do
            rev <- mapValidationToCorrupted "revision" (mkRevisionNumber rawRev)
            Right (DocumentBody rawBody, rev)
        _ ->
            Left (CorruptedData ("Duplicate primary key for document: " <> toText docId))

fetchRevisionIdInTx ::
       SQLite.Connection
    -> DocumentId
    -> RevisionNumber
    -> IO (Either StorageError RevisionId)
fetchRevisionIdInTx conn docId rev = do
    rows <- SQLite.query
        conn
        "SELECT id FROM document_revisions WHERE document_id = ? AND revision = ?"
        (toText docId, revisionNumberValue rev)
        :: IO [Only Text]
    pure $ case rows of
        [Only rawRevId] ->
            mapValidationToCorrupted "revision_id" (mkRevisionId rawRevId)
        [] ->
            Left (CorruptedData ("Missing parent revision " <> toText rev <> " for document: " <> toText docId))
        _ ->
            Left (CorruptedData ("Duplicate revision " <> toText rev <> " for document: " <> toText docId))

rewindBodyToRevisionInTx ::
       SQLite.Connection
    -> DocumentId
    -> DocumentBody
    -> RevisionNumber
    -> RevisionNumber
    -> IO (Either StorageError DocumentBody)
rewindBodyToRevisionInTx conn docId currentBody currentRev targetRev
    | targetRev >= currentRev =
        pure $ Left $ CorruptedData
            ( "Invalid rewind range for "
                <> toText docId
                <> ": target r"
                <> toText targetRev
                <> " is not older than current r"
                <> toText currentRev
            )
    | otherwise = do
        diffRows <- SQLite.query
            conn
            "SELECT revision, diff\
            \ FROM document_revisions\
            \ WHERE document_id = ? AND revision > ? AND revision <= ?\
            \ ORDER BY revision DESC"
            (toText docId, revisionNumberValue targetRev, revisionNumberValue currentRev)
            :: IO [(Int, Text)]
        let expectedRevNums :: [Int]
            expectedRevNums = [revisionNumberValue currentRev, revisionNumberValue currentRev - 1 .. revisionNumberValue targetRev + 1]
        pure (applyChain (toText currentBody) expectedRevNums diffRows)
  where
    applyChain :: Text -> [Int] -> [(Int, Text)] -> Either StorageError DocumentBody
    applyChain !accBody [] [] = Right (DocumentBody accBody)
    applyChain !accBody (expectedRev : restExpected) ((actualRev, rawDiff) : restRows)
        | actualRev /= expectedRev =
            Left $ CorruptedData
                ( "Missing or out-of-order revision in diff chain for "
                    <> toText docId
                    <> ": expected r"
                    <> Txt.pack (show expectedRev)
                    <> ", got r"
                    <> Txt.pack (show actualRev)
                )
        | otherwise =
            case applyUnifiedDiffReverse accBody (UnifiedDiff rawDiff) of
                Left diffErr ->
                    Left $ CorruptedData
                        ( "Failed to reverse-apply diff for "
                            <> toText docId
                            <> "@r"
                            <> Txt.pack (show actualRev)
                            <> ": "
                            <> toText diffErr
                        )
                Right prevBody ->
                    applyChain prevBody restExpected restRows
    applyChain _ _ _ =
        Left (CorruptedData ("Incomplete revision diff chain for document: " <> toText docId))

decodeRevisionSummaryRow :: RevisionSummaryRow -> Either StorageError RevisionSummary
decodeRevisionSummaryRow (rawId, rawDocId, rawRev, mRawParentId, rawTitle, rawFormat, rawHash, rawCreatedAt) = do
    revId     <- mapValidationToCorrupted "revision_id" (mkRevisionId rawId)
    docId     <- mapValidationToCorrupted "document_id" (mkDocumentId rawDocId)
    rev       <- mapValidationToCorrupted "revision" (mkRevisionNumber rawRev)
    mParentId <- traverse (mapValidationToCorrupted "parent_revision_id" . mkRevisionId) mRawParentId
    title     <- mapValidationToCorrupted "title" (mkDocumentTitle rawTitle)
    format    <- mapValidationToCorrupted "format" (parseDocumentFormat rawFormat)
    hashVal   <- mapValidationToCorrupted "content_hash" (mkContentHash rawHash)
    createdAt <- parseSqliteUtcTime rawCreatedAt
    Right RevisionSummary
        { id               = revId
        , documentId       = docId
        , revision         = rev
        , parentRevisionId = mParentId
        , title            = title
        , format           = format
        , contentHash      = hashVal
        , createdAt        = createdAt
        }

decodeRevisionRowBase :: RevisionDetailRow -> Either StorageError (RevisionSummary, UnifiedDiff)
decodeRevisionRowBase (rawId, rawDocId, rawRev, mRawParentId, rawTitle, rawFormat, rawDiff, rawHash, rawCreatedAt) = do
    summary <- decodeRevisionSummaryRow (rawId, rawDocId, rawRev, mRawParentId, rawTitle, rawFormat, rawHash, rawCreatedAt)
    Right (summary, UnifiedDiff rawDiff)
