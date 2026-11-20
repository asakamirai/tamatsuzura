{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.Storage.Schema
    ( -- * ストレージハンドル (内部共有)
      StorageHandle(..)
      -- * スキーマバージョンと初期化
    , currentSchemaVersion
    , configureConnection
    , initializeSchema
    , inspectPragmas
      -- * トランザクション・例外境界
    , withImmediateTransaction
    , withReadTransaction
    , catchSqliteErrors
      -- * SQLite 値変換ユーティリティ
    , truncateToMicros
    , formatSqliteUtcTime
    , parseSqliteUtcTime
    , asciiToLower
    , escapeLikePattern
    ) where

import           Control.Exception                (Handler(..), IOException, catches, mask, onException, try)
import           Control.Monad                    (when)
import qualified Data.Char                        as Char
import           Data.Text                        (Text)
import qualified Data.Text                        as Txt
import           Data.Time.Clock
    ( UTCTime(..)
    , diffTimeToPicoseconds
    , picosecondsToDiffTime
    )
import           Data.Time.Format                 (defaultTimeLocale, formatTime, parseTimeM)
import           Database.SQLite.Simple           (Only(..))
import qualified Database.SQLite.Simple           as SQLite
import           TamaTsuzura.Server.Storage.Types
    ( SqlitePragmas(..)
    , StorageConfig(..)
    , StorageError(..)
    , StorageTarget(..)
    , busyTimeoutMsValue
    )

-- | 初期化済み SQLite 接続を内包するストレージハンドル。
-- 公開モジュール ('TamaTsuzura.Server.Storage' 等) ではコンストラクタを隠蔽し、
-- PRAGMA 設定・スキーマ初期化済みの状態だけを型で保証する。
--
-- __スレッド安全性に関する前提__: 1 つの 'StorageHandle' は排他機構を持たない単一の SQLite 接続に対応する。
-- 複数スレッドから同一ハンドルを同時に操作するとトランザクションが交錯するため、
-- Warp 常駐サーバでもリクエスト単位で 'withStorage' を呼び出すか、外部で直列化して利用すること。
newtype StorageHandle = StorageHandle
    { connection :: SQLite.Connection
      -- ^ 初期化済み SQLite 接続
    }

-- | 本バイナリが期待・サポートする SQLite スキーマバージョン (@PRAGMA user_version@)。
-- v2 で M3 認証向けの @users@ / @sessions@ テーブルおよび関連インデックスを追加した。
currentSchemaVersion :: Int
currentSchemaVersion = 2

-- | 新規に開いた SQLite 接続へ CGI / Warp 運用で必須の PRAGMA 設定を適用する。
-- ロック競合時に後続 PRAGMA 自体が即座に @SQLITE_BUSY@ とならないよう、
-- 必ず @busy_timeout@ を最初に設定する。
-- また WAL モードが既に有効なファイル DB に対して毎回 @PRAGMA journal_mode = WAL@ を
-- 再発行して不要な書き込みロック競合を起こさないよう、現在値を確認してから切替える。
configureConnection :: StorageConfig -> SQLite.Connection -> IO (Either StorageError ())
configureConnection cfg conn = catchSqliteErrors $ do
    -- SQLite の PRAGMA 構文はバインドパラメータを受け付けないため、
    -- BusyTimeoutMs のスマートコンストラクタで範囲検証済みの整数値のみを埋め込む。
    let timeoutQuery :: SQLite.Query
        timeoutQuery = SQLite.Query ("PRAGMA busy_timeout = " <> Txt.pack (show (busyTimeoutMsValue cfg.busyTimeoutMs)))
    SQLite.execute_ conn timeoutQuery
    SQLite.execute_ conn "PRAGMA foreign_keys = ON"
    case cfg.target of
        StorageFile{} -> do
            eCurrentMode <- readSingleTextPragma conn "PRAGMA journal_mode"
            case eCurrentMode of
                Left err -> pure (Left err)
                Right currentMode -> do
                    when (Txt.toLower currentMode /= "wal") $ do
                        _ <- SQLite.query_ conn "PRAGMA journal_mode = WAL" :: IO [Only Text]
                        pure ()
                    SQLite.execute_ conn "PRAGMA synchronous = NORMAL"
                    pure (Right ())
        StorageMemory ->
            pure (Right ())

-- | データベーススキーマを冪等に初期化する。
-- 読み取り専用リクエストが不要な書き込みロックを取らないよう、まずトランザクション外で
-- @PRAGMA user_version@ を確認する (ダブルチェックロッキング)。
-- 未初期化 (@0@) の場合のみ @BEGIN IMMEDIATE@ トランザクションへ入り再確認した上で
-- テーブル・インデックスを作成する。対応していないスキーマバージョン (@0@ および 'currentSchemaVersion' 以外)
-- が設定されている場合はデータ不整合・破壊を防ぐため 'UnsupportedSchemaVersion' を返す。
initializeSchema :: SQLite.Connection -> IO (Either StorageError ())
initializeSchema conn = catchSqliteErrors $ do
    eVersion <- readSingleIntPragma conn "PRAGMA user_version"
    case eVersion of
        Left err -> pure (Left err)
        Right version
            | version == currentSchemaVersion ->
                pure (Right ())
            | version /= 0 ->
                pure (Left (UnsupportedSchemaVersion version currentSchemaVersion))
            | otherwise ->
                withImmediateTransaction conn $ do
                    eRechecked <- readSingleIntPragma conn "PRAGMA user_version"
                    case eRechecked of
                        Left err -> pure (Left err)
                        Right rechecked
                            | rechecked == 0 -> do
                                applyInitialSchema conn
                                pure (Right ())
                            | rechecked == currentSchemaVersion ->
                                pure (Right ())
                            | otherwise ->
                                pure (Left (UnsupportedSchemaVersion rechecked currentSchemaVersion))

-- | 接続中の SQLite における主要 PRAGMA 設定値を読み取る。
inspectPragmas :: SQLite.Connection -> IO (Either StorageError SqlitePragmas)
inspectPragmas conn = catchSqliteErrors $ do
    eJournal <- readSingleTextPragma conn "PRAGMA journal_mode"
    eTimeout <- readSingleIntPragma conn "PRAGMA busy_timeout"
    eForeign <- readSingleIntPragma conn "PRAGMA foreign_keys"
    eVersion <- readSingleIntPragma conn "PRAGMA user_version"
    pure $ do
        journal    <- eJournal
        timeout    <- eTimeout
        foreignVal <- eForeign
        version    <- eVersion
        Right SqlitePragmas
            { journalMode   = Txt.toLower journal
            , busyTimeoutMs = timeout
            , foreignKeys   = foreignVal /= 0
            , userVersion   = version
            }

-- | 書き込みを伴う操作向けに @BEGIN IMMEDIATE@ トランザクションで実行する。
-- SQLite の @DEFERRED@ トランザクションでは読み取りから書き込みへの昇格時に
-- @busy_timeout@ を待たず即時 @SQLITE_BUSY@ となる場合があるため、CGI 並行書き込みでは
-- 常に @BEGIN IMMEDIATE@ を用いる。アクションが 'Left' または例外で終了した場合、
-- および @COMMIT@ 失敗時は確実にロールバックを試みる。
withImmediateTransaction ::
       SQLite.Connection
    -> IO (Either StorageError a)
    -> IO (Either StorageError a)
withImmediateTransaction conn action =
    mask $ \ restore -> do
        SQLite.execute_ conn "BEGIN IMMEDIATE"
        eRes <- restore action `onException` rollbackQuietly conn
        case eRes of
            Right val -> do
                SQLite.execute_ conn "COMMIT" `onException` rollbackQuietly conn
                pure (Right val)
            Left err  -> do
                rollbackQuietly conn
                pure (Left err)

-- | 複数クエリ間で一貫したスナップショットを得るための読み取り専用トランザクション。
-- 書き込みロックを取得せず読み取りスナップショットを固定し、終了時に必ずトランザクションを閉じる。
withReadTransaction ::
       SQLite.Connection
    -> IO (Either StorageError a)
    -> IO (Either StorageError a)
withReadTransaction conn action =
    mask $ \ restore -> do
        SQLite.execute_ conn "BEGIN"
        eRes <- restore action `onException` rollbackQuietly conn
        case eRes of
            Right val -> do
                SQLite.execute_ conn "COMMIT" `onException` rollbackQuietly conn
                pure (Right val)
            Left err  -> do
                rollbackQuietly conn
                pure (Left err)

-- | SQLite ドライバおよびファイル I/O の同期例外を捕捉し、'SqliteOperationError' へ変換する。
-- 非同期例外は捕捉せずそのまま伝播させる。
catchSqliteErrors :: IO (Either StorageError a) -> IO (Either StorageError a)
catchSqliteErrors action =
    catches
        action
        [ Handler handleSqlError
        , Handler handleFormatError
        , Handler handleResultError
        , Handler handleIoException
        ]
  where
    handleSqlError :: SQLite.SQLError -> IO (Either StorageError a)
    handleSqlError err = pure (Left (SqliteOperationError (Txt.pack (show err))))

    handleFormatError :: SQLite.FormatError -> IO (Either StorageError a)
    handleFormatError err = pure (Left (SqliteOperationError (Txt.pack (show err))))

    handleResultError :: SQLite.ResultError -> IO (Either StorageError a)
    handleResultError err = pure (Left (SqliteOperationError (Txt.pack (show err))))

    handleIoException :: IOException -> IO (Either StorageError a)
    handleIoException err = pure (Left (SqliteOperationError (Txt.pack (show err))))

-- | 'UTCTime' をマイクロ秒精度へ切り捨てる。
-- SQLite 保存形式 (6 桁マイクロ秒固定) とメモリ上の戻り値精度を一致させ、
-- 保存直後のエンティティと再取得結果が厳密同値になる不変条件を保つ。
truncateToMicros :: UTCTime -> UTCTime
truncateToMicros utc =
    let micros :: Integer
        micros = diffTimeToPicoseconds utc.utctDayTime `div` 1000000
    in  UTCTime utc.utctDay (picosecondsToDiffTime (micros * 1000000))

-- | 'UTCTime' を SQLite の @TEXT@ 比較で時系列順と辞書順が完全に一致する
-- 固定長 ISO 8601 UTC 文字列 (@YYYY-MM-DDTHH:MM:SS.ffffffZ@) へ変換する。
formatSqliteUtcTime :: UTCTime -> Text
formatSqliteUtcTime utc =
    let truncated :: UTCTime
        truncated = truncateToMicros utc

        dateAndSec :: Text
        dateAndSec = Txt.pack (formatTime defaultTimeLocale "%04Y-%m-%dT%H:%M:%S." truncated)

        microsPart :: Text
        microsPart = Txt.take 6 (Txt.pack (formatTime defaultTimeLocale "%q" truncated))
    in  dateAndSec <> microsPart <> "Z"

-- | SQLite に保存された ISO 8601 UTC 文字列から 'UTCTime' を復元する。
parseSqliteUtcTime :: Text -> Either StorageError UTCTime
parseSqliteUtcTime raw =
    case parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (Txt.unpack raw) of
        Just utc -> Right utc
        Nothing  -> Left (CorruptedData ("Invalid UTC timestamp in database: " <> raw))

-- | ASCII 大文字 (@A@〜@Z@) のみを小文字へ変換する。
-- SQLite 標準の @lower()@ は ASCII 文字のみを小文字化するため、全角英大文字 (@Ａ@ 等) や
-- アクセント付き大文字 (@É@ 等) を検索クエリ側だけ小文字化して不一致になるバグを防ぐ。
asciiToLower :: Text -> Text
asciiToLower = Txt.map toAsciiLowerChar
  where
    toAsciiLowerChar :: Char -> Char
    toAsciiLowerChar ch
        | Char.isAsciiUpper ch = Char.toLower ch
        | otherwise            = ch

-- | 部分一致検索の入力テキストを SQLite の @LIKE ... ESCAPE '\'@ 用パターンへ安全に変換する。
-- 入力中の @%@・@_@・@\@ をすべてエスケープし、前後にワイルドカード @%@ を付与する。
escapeLikePattern :: Text -> Text
escapeLikePattern raw = "%" <> Txt.concatMap escapeChar raw <> "%"
  where
    escapeChar :: Char -> Text
    escapeChar '\\' = "\\\\"
    escapeChar '%'  = "\\%"
    escapeChar '_'  = "\\_"
    escapeChar ch   = Txt.singleton ch

applyInitialSchema :: SQLite.Connection -> IO ()
applyInitialSchema conn = do
    SQLite.execute_ conn createDocumentsTableSql
    SQLite.execute_ conn createDocumentTagsTableSql
    SQLite.execute_ conn createUsersTableSql
    SQLite.execute_ conn createSessionsTableSql
    SQLite.execute_ conn createDeletedUpdatedIndexSql
    SQLite.execute_ conn createFolderIndexSql
    SQLite.execute_ conn createTagIndexSql
    SQLite.execute_ conn createSessionsUserIndexSql
    SQLite.execute_ conn createSessionsExpiresIndexSql
    SQLite.execute_ conn (SQLite.Query ("PRAGMA user_version = " <> Txt.pack (show currentSchemaVersion)))

createDocumentsTableSql :: SQLite.Query
createDocumentsTableSql =
    "CREATE TABLE IF NOT EXISTS documents (\
    \ id TEXT PRIMARY KEY NOT NULL,\
    \ title TEXT NOT NULL,\
    \ body TEXT NOT NULL,\
    \ format TEXT NOT NULL CHECK (format IN ('markdown', 'plain_text')),\
    \ folder TEXT,\
    \ created_at TEXT NOT NULL,\
    \ updated_at TEXT NOT NULL,\
    \ deleted_at TEXT\
    \)"

createDocumentTagsTableSql :: SQLite.Query
createDocumentTagsTableSql =
    "CREATE TABLE IF NOT EXISTS document_tags (\
    \ document_id TEXT NOT NULL,\
    \ tag TEXT NOT NULL,\
    \ PRIMARY KEY (document_id, tag),\
    \ FOREIGN KEY (document_id) REFERENCES documents(id) ON DELETE CASCADE\
    \)"

createUsersTableSql :: SQLite.Query
createUsersTableSql =
    "CREATE TABLE IF NOT EXISTS users (\
    \ id TEXT PRIMARY KEY NOT NULL,\
    \ username TEXT NOT NULL UNIQUE CHECK (username = lower(username) AND username = trim(username)),\
    \ display_name TEXT,\
    \ password_hash TEXT NOT NULL,\
    \ created_at TEXT NOT NULL,\
    \ updated_at TEXT NOT NULL\
    \)"

createSessionsTableSql :: SQLite.Query
createSessionsTableSql =
    "CREATE TABLE IF NOT EXISTS sessions (\
    \ token_hash TEXT PRIMARY KEY NOT NULL,\
    \ user_id TEXT NOT NULL,\
    \ csrf_token TEXT NOT NULL,\
    \ created_at TEXT NOT NULL,\
    \ expires_at TEXT NOT NULL,\
    \ last_accessed_at TEXT NOT NULL,\
    \ FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE\
    \)"

createDeletedUpdatedIndexSql :: SQLite.Query
createDeletedUpdatedIndexSql =
    "CREATE INDEX IF NOT EXISTS idx_documents_deleted_updated\
    \ ON documents (deleted_at, updated_at DESC, id ASC)"

createFolderIndexSql :: SQLite.Query
createFolderIndexSql =
    "CREATE INDEX IF NOT EXISTS idx_documents_folder\
    \ ON documents (folder) WHERE folder IS NOT NULL"

createTagIndexSql :: SQLite.Query
createTagIndexSql =
    "CREATE INDEX IF NOT EXISTS idx_document_tags_tag\
    \ ON document_tags (tag, document_id)"

createSessionsUserIndexSql :: SQLite.Query
createSessionsUserIndexSql =
    "CREATE INDEX IF NOT EXISTS idx_sessions_user_id\
    \ ON sessions (user_id)"

createSessionsExpiresIndexSql :: SQLite.Query
createSessionsExpiresIndexSql =
    "CREATE INDEX IF NOT EXISTS idx_sessions_expires_at\
    \ ON sessions (expires_at)"

readSingleIntPragma :: SQLite.Connection -> SQLite.Query -> IO (Either StorageError Int)
readSingleIntPragma conn pragmaSql = do
    rows <- SQLite.query_ conn pragmaSql :: IO [Only Int]
    pure $ case rows of
        [Only val] -> Right val
        _          -> Left (CorruptedData ("Unexpected result row count for pragma: " <> SQLite.fromQuery pragmaSql))

readSingleTextPragma :: SQLite.Connection -> SQLite.Query -> IO (Either StorageError Text)
readSingleTextPragma conn pragmaSql = do
    rows <- SQLite.query_ conn pragmaSql :: IO [Only Text]
    pure $ case rows of
        [Only val] -> Right val
        _          -> Left (CorruptedData ("Unexpected result row count for pragma: " <> SQLite.fromQuery pragmaSql))

rollbackQuietly :: SQLite.Connection -> IO ()
rollbackQuietly conn = do
    -- SQLite が既に自動ロールバック済みの場合に ROLLBACK の失敗で元の例外を隠蔽しないよう握りつぶす
    _ <- try (SQLite.execute_ conn "ROLLBACK") :: IO (Either SQLite.SQLError ())
    pure ()
