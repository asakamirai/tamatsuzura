{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.Storage.Types
    ( -- * ストレージ接続設定
      StorageTarget(..)
    , BusyTimeoutMs
    , maxBusyTimeoutMs
    , mkBusyTimeoutMs
    , defaultBusyTimeoutMs
    , busyTimeoutMsValue
    , StorageConfig(..)
    , defaultStorageConfig
    , inMemoryStorageConfig
      -- * SQLite ランタイム設定診断
    , SqlitePragmas(..)
      -- * ストレージエラー型
    , StorageError(..)
    , storageErrorToApiError
    ) where

import           Control.Exception             (Exception)
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import           System.OsPath                 (OsPath)
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , validationErrorToApiError
    )
import           TamaTsuzura.Common.Document   (DocumentId)
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation (ValidationError)

-- | SQLite データベースの接続先種別。
-- 本番・CGI・Warp ではエンコーディング安全な 'OsPath' でファイルパスを保持し、
-- 単体テスト用途に限りプロセス内限定のインメモリ DB を選択できるようにする。
data StorageTarget
    = StorageFile !OsPath
      -- ^ ファイルベース SQLite データベース (WAL モード適用対象)
    | StorageMemory
      -- ^ インメモリ SQLite データベース (@:memory:@、接続単位で揮発)
    deriving (Eq, Show)

-- | SQLite の @PRAGMA busy_timeout@ に設定するロック待機時間 (ミリ秒、0〜'maxBusyTimeoutMs')。
-- 負数や極端に長い待機時間による CGI プロセス滞留を防ぐためコンストラクタを非公開とする。
newtype BusyTimeoutMs = BusyTimeoutMs
    { unWrap :: Int
    } deriving (Eq, Ord)

instance ToText BusyTimeoutMs where
    toText timeout = Txt.pack (show timeout.unWrap) <> "ms"

instance Show BusyTimeoutMs where
    show = Txt.unpack . toText

-- | @busy_timeout@ の許容上限 (5 分 = 300,000 ms)。
maxBusyTimeoutMs :: Int
maxBusyTimeoutMs = 300000

-- | 0〜'maxBusyTimeoutMs' の範囲を検証して 'BusyTimeoutMs' を構築する。
-- @0@ はロック待機を行わず即座にエラーとする設定として許可する。
mkBusyTimeoutMs :: Int -> Either Text BusyTimeoutMs
mkBusyTimeoutMs ms
    | ms < 0 || ms > maxBusyTimeoutMs =
        Left
            ( "Busy timeout must be between 0 and "
                <> Txt.pack (show maxBusyTimeoutMs)
                <> " ms, got "
                <> Txt.pack (show ms)
            )
    | otherwise = Right (BusyTimeoutMs ms)

-- | CGI 並行アクセス時のロック競合を吸収するための既定待機時間 (5,000 ms)。
defaultBusyTimeoutMs :: BusyTimeoutMs
defaultBusyTimeoutMs = BusyTimeoutMs 5000

-- | 検証済み 'BusyTimeoutMs' から生のミリ秒値を取り出す。
busyTimeoutMsValue :: BusyTimeoutMs -> Int
busyTimeoutMsValue timeout = timeout.unWrap

-- | SQLite 永続化層の接続設定。
data StorageConfig = StorageConfig
    { target        :: !StorageTarget
      -- ^ SQLite データベースの接続先 (ファイルまたはインメモリ)
    , busyTimeoutMs :: !BusyTimeoutMs
      -- ^ ロック競合時の最大待機時間 (ミリ秒)
    } deriving (Eq, Show)

-- | 指定した 'OsPath' を対象とし、既定の 'defaultBusyTimeoutMs' を用いるストレージ設定を生成する。
defaultStorageConfig :: OsPath -> StorageConfig
defaultStorageConfig path = StorageConfig
    { target        = StorageFile path
    , busyTimeoutMs = defaultBusyTimeoutMs
    }

-- | テスト用のインメモリ SQLite ストレージ設定を生成する。
inMemoryStorageConfig :: StorageConfig
inMemoryStorageConfig = StorageConfig
    { target        = StorageMemory
    , busyTimeoutMs = defaultBusyTimeoutMs
    }

-- | 接続中の SQLite に対して適用されている主要 PRAGMA 設定のスナップショット。
-- 運用診断および WAL / @busy_timeout@ / 外部キー / スキーマバージョンの自動テスト検証に用いる。
data SqlitePragmas = SqlitePragmas
    { journalMode   :: !Text
      -- ^ 現在の @PRAGMA journal_mode@ 値 (ファイル DB では @"wal"@)
    , busyTimeoutMs :: !Int
      -- ^ 現在の @PRAGMA busy_timeout@ 値 (ミリ秒)
    , foreignKeys   :: !Bool
      -- ^ 現在の @PRAGMA foreign_keys@ 有効状態
    , userVersion   :: !Int
      -- ^ 現在の @PRAGMA user_version@ (スキーマバージョン)
    } deriving (Eq, Show)

-- | SQLite 永続化層で発生しうる失敗理由。
-- 例外を無秩序に送出せず、呼び出し側 (HTTP API 層など) が状態遷移・エラーコードへ
-- 機械的にマッピングできるよう ADT として表現する。
data StorageError
    = DocumentNotFound !DocumentId
      -- ^ 指定された文書が存在しない (HTTP 404 相当)
    | DocumentInTrash !DocumentId
      -- ^ ゴミ箱内の文書に対して編集などの稼働中限定操作を試みた (HTTP 409 相当)
    | DocumentNotTrashed !DocumentId
      -- ^ 稼働中の文書に対して完全削除を試みた (HTTP 409 相当)
    | DocumentIdAlreadyExists !DocumentId
      -- ^ 既に存在する文書 ID での新規作成を試みた (HTTP 409 相当)
    | StorageValidationError !ValidationError
      -- ^ 永続化境界で検出したドメイン制約違反 (本文サイズ上限超過など)
    | UnsupportedSchemaVersion !Int !Int
      -- ^ 本バイナリが対応していないスキーマバージョンを検出した (検出値, 対応バージョン)
    | CorruptedData !Text
      -- ^ 永続化データがドメイン不変条件または期待形式を満たしていない
    | SqliteOperationError !Text
      -- ^ SQLite 接続・ロックタイムアウト・I/O 等のインフラ層エラー
    deriving (Eq)

instance ToText StorageError where
    toText (DocumentNotFound docId) =
        "Document not found: " <> toText docId
    toText (DocumentInTrash docId) =
        "Document is in trash and must be restored before editing: " <> toText docId
    toText (DocumentNotTrashed docId) =
        "Document must be moved to trash before permanent deletion: " <> toText docId
    toText (DocumentIdAlreadyExists docId) =
        "Document ID already exists: " <> toText docId
    toText (StorageValidationError err) =
        toText err
    toText (UnsupportedSchemaVersion actual supported) =
        "Unsupported database schema version: "
            <> Txt.pack (show actual)
            <> " (supported version: "
            <> Txt.pack (show supported)
            <> ")"
    toText (CorruptedData detail) =
        "Corrupted storage data: " <> detail
    toText (SqliteOperationError detail) =
        "SQLite operation failed: " <> detail

instance Show StorageError where
    show = Txt.unpack . toText

instance Exception StorageError

-- | 'StorageError' をクライアント返却用の 'ApiError' へ変換する。
-- インフラ詳細 ('SqliteOperationError' / 'CorruptedData' / 'UnsupportedSchemaVersion') は
-- クライアントへ内部情報を漏洩させないよう汎用メッセージにマスクする。
storageErrorToApiError :: StorageError -> ApiError
storageErrorToApiError err@DocumentNotFound{}        = ApiError
    { code    = ErrNotFound
    , message = toText err
    , field   = Nothing
    }
storageErrorToApiError err@DocumentInTrash{}         = ApiError
    { code    = ErrConflict
    , message = toText err
    , field   = Nothing
    }
storageErrorToApiError err@DocumentNotTrashed{}      = ApiError
    { code    = ErrConflict
    , message = toText err
    , field   = Nothing
    }
storageErrorToApiError err@DocumentIdAlreadyExists{} = ApiError
    { code    = ErrConflict
    , message = toText err
    , field   = Nothing
    }
storageErrorToApiError (StorageValidationError err)  =
    validationErrorToApiError err
storageErrorToApiError UnsupportedSchemaVersion{}    = ApiError
    { code    = ErrInternalServer
    , message = "Internal server error"
    , field   = Nothing
    }
storageErrorToApiError CorruptedData{}               = ApiError
    { code    = ErrInternalServer
    , message = "Internal server error"
    , field   = Nothing
    }
storageErrorToApiError SqliteOperationError{}        = ApiError
    { code    = ErrInternalServer
    , message = "Internal server error"
    , field   = Nothing
    }
