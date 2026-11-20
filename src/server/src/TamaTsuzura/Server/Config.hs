{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.Config
    ( -- * ポート設定
      ServerPort
    , mkServerPort
    , serverPortValue
    , defaultServerPort
    , parsePortArgs
      -- * 環境変数・既定値定数
    , envDbPath
    , envDbBusyTimeoutMs
    , envPort
    , envScriptName
    , envStaticDir
    , defaultDbRelativePath
      -- * Warp / CGI ランタイム設定
    , RawWarpOptions(..)
    , emptyRawWarpOptions
    , WarpConfig(..)
    , CgiConfig(..)
    , parseWarpArgs
    , resolveStorageConfig
    , resolveWarpConfig
    , resolveCgiConfig
    , loadWarpConfig
    , loadCgiConfig
    ) where

import           Control.Exception                (IOException, try)
import qualified Data.Char                        as Char
import           Data.Text                        (Text)
import qualified Data.Text                        as Txt
import qualified System.Environment               as Env
import qualified System.OsPath                    as OS
import           TamaTsuzura.Common.Text          (ToText(..))
import           TamaTsuzura.Server.Storage.Types
    ( BusyTimeoutMs
    , StorageConfig(..)
    , StorageTarget(..)
    , defaultBusyTimeoutMs
    , maxBusyTimeoutMs
    , mkBusyTimeoutMs
    )
import           Text.Read                        (readMaybe)

-- | HTTP サーバの待受ポート番号 (1〜65535)。
-- 不正なポート番号の構築を型レベルで防ぐためコンストラクタは非公開とし、'mkServerPort' を経由する。
newtype ServerPort = ServerPort
    { unWrap :: Int
    } deriving (Eq, Ord)

instance ToText ServerPort where
    toText port = Txt.pack (show port.unWrap)

instance Show ServerPort where
    show = Txt.unpack . toText

-- | 1〜65535 の範囲を検証して 'ServerPort' を構築するスマートコンストラクタ。
mkServerPort :: Int -> Either Text ServerPort
mkServerPort port | port >= 1 && port <= 65535 = Right (ServerPort port)
                  | otherwise                  = Left ("Invalid port number: " <> Txt.pack (show port))

-- | 検証済み 'ServerPort' から生の 'Int' ポート番号を取り出す。
serverPortValue :: ServerPort -> Int
serverPortValue port = port.unWrap

-- | 開発用 Warp サーバの既定ポート番号 (8080)。
defaultServerPort :: ServerPort
defaultServerPort = ServerPort 8080

-- | SQLite データベースファイルパスを指定する環境変数名。
envDbPath :: String
envDbPath = "TAMATSUZURA_DB_PATH"

-- | SQLite @PRAGMA busy_timeout@ (ミリ秒) を指定する環境変数名。
envDbBusyTimeoutMs :: String
envDbBusyTimeoutMs = "TAMATSUZURA_DB_BUSY_TIMEOUT_MS"

-- | Warp サーバの待受ポートを指定する環境変数名。
envPort :: String
envPort = "PORT"

-- | CGI/1.1 のスクリプトマウントパスを示す標準環境変数名。
envScriptName :: String
envScriptName = "SCRIPT_NAME"

-- | Warp 開発サーバの静的アセット配信ディレクトリを指定する環境変数名。
envStaticDir :: String
envStaticDir = "TAMATSUZURA_STATIC_DIR"

-- | @TAMATSUZURA_DB_PATH@ および @--db-path@ がともに未指定のときに使用する既定の相対 DB パス。
defaultDbRelativePath :: String
defaultDbRelativePath = "data/tamatsuzura.sqlite3"

-- | Warp エントリポイントのコマンドライン引数から解釈した生オプション。
data RawWarpOptions = RawWarpOptions
    { rawPort          :: !(Maybe String)
      -- ^ @--port@ 引数の生文字列
    , rawDbPath        :: !(Maybe String)
      -- ^ @--db-path@ 引数の生文字列
    , rawBusyTimeoutMs :: !(Maybe String)
      -- ^ @--busy-timeout-ms@ 引数の生文字列
    , rawStaticDir     :: !(Maybe String)
      -- ^ @--static-dir@ 引数の生文字列
    } deriving (Eq, Show)

-- | 全項目未指定の 'RawWarpOptions'。
emptyRawWarpOptions :: RawWarpOptions
emptyRawWarpOptions = RawWarpOptions
    { rawPort          = Nothing
    , rawDbPath        = Nothing
    , rawBusyTimeoutMs = Nothing
    , rawStaticDir     = Nothing
    }

-- | Warp スタンドアロンサーバの検証済み起動設定。
data WarpConfig = WarpConfig
    { port      :: !ServerPort
      -- ^ 待受ポート番号
    , storage   :: !StorageConfig
      -- ^ SQLite ストレージ設定
    , staticDir :: !(Maybe OS.OsPath)
      -- ^ 静的アセット配信ルートディレクトリ (未指定時は API 専用)
    } deriving (Eq, Show)

-- | CGI エントリポイントの検証済み実行設定。
data CgiConfig = CgiConfig
    { scriptName :: !(Maybe Text)
      -- ^ CGI @SCRIPT_NAME@ の値 (例: @"/api"@ または @"/cgi-bin/tamatsuzura.cgi"@)
    , storage    :: !StorageConfig
      -- ^ SQLite ストレージ設定
    } deriving (Eq, Show)

-- | コマンドライン引数から待受ポートのみを解釈する簡易パーサ。
-- ポート単体の検証と既存インターフェース互換のために提供する。
parsePortArgs :: [String] -> Either Text ServerPort
parsePortArgs []              = Right defaultServerPort
parsePortArgs ["--port", raw] = parsePortString raw
parsePortArgs _               = Left "Usage: tamatsuzura-server [--port PORT]"

-- | Warp サーバの全コマンドライン引数を検証・解釈する。
-- 重複フラグ・値欠落・未知フラグは利用法メッセージ付きのエラーとして拒否する。
parseWarpArgs :: [String] -> Either Text RawWarpOptions
parseWarpArgs = go emptyRawWarpOptions
  where
    usageMsg :: Text
    usageMsg = "Usage: tamatsuzura-server [--port PORT] [--db-path PATH] [--busy-timeout-ms MS] [--static-dir DIR]"

    go :: RawWarpOptions -> [String] -> Either Text RawWarpOptions
    go opts [] = Right opts
    go opts ("--port" : val : rest) =
        case opts.rawPort of
            Just _  -> Left "Duplicate option: --port"
            Nothing -> go opts { rawPort = Just val } rest
    go opts ("--db-path" : val : rest) =
        case opts.rawDbPath of
            Just _  -> Left "Duplicate option: --db-path"
            Nothing -> go opts { rawDbPath = Just val } rest
    go opts ("--busy-timeout-ms" : val : rest) =
        case opts.rawBusyTimeoutMs of
            Just _  -> Left "Duplicate option: --busy-timeout-ms"
            Nothing -> go opts { rawBusyTimeoutMs = Just val } rest
    go opts ("--static-dir" : val : rest) =
        case opts.rawStaticDir of
            Just _  -> Left "Duplicate option: --static-dir"
            Nothing -> go opts { rawStaticDir = Just val } rest
    go _ _ = Left usageMsg

-- | 生の DB パス文字列と @busy_timeout@ 文字列から 'StorageConfig' を構築する。
-- DB パス未指定時は 'defaultDbRelativePath' を使用し、空文字列の明示指定は設定ミスとして拒否する。
resolveStorageConfig ::
       Maybe String
    -> Maybe String
    -> IO (Either Text StorageConfig)
resolveStorageConfig mRawDbPath mRawTimeout =
    case resolveTimeout mRawTimeout of
        Left err -> pure (Left err)
        Right timeout ->
            case resolvePathString mRawDbPath of
                Left err -> pure (Left err)
                Right rawPath -> do
                    -- 環境変数・CLI 引数は base が String で返すため、境界で OsPath へエンコードする
                    eOsPath <- try (OS.encodeFS rawPath) :: IO (Either IOException OS.OsPath)
                    pure $ case eOsPath of
                        Left ioErr   -> Left ("Invalid database path: " <> Txt.pack (show ioErr))
                        Right osPath -> Right StorageConfig
                            { target        = StorageFile osPath
                            , busyTimeoutMs = timeout
                            }
  where
    resolvePathString :: Maybe String -> Either Text String
    resolvePathString Nothing    = Right defaultDbRelativePath
    resolvePathString (Just raw)
        | Txt.null (Txt.strip (Txt.pack raw)) = Left "Database path must not be empty"
        | otherwise                           = Right raw

    resolveTimeout :: Maybe String -> Either Text BusyTimeoutMs
    resolveTimeout Nothing    = Right defaultBusyTimeoutMs
    resolveTimeout (Just raw) = parseBusyTimeoutString raw

-- | CLI 引数と環境変数一覧から Warp サーバ設定を解決する。
-- 同一項目が双方にある場合は CLI 引数を優先し、未指定項目は安全な既定値を用いる。
resolveWarpConfig ::
       [String]
    -> [(String, String)]
    -> IO (Either Text WarpConfig)
resolveWarpConfig args envList =
    case parseWarpArgs args of
        Left err -> pure (Left err)
        Right rawOpts ->
            case resolvePort rawOpts.rawPort (lookup envPort envList) of
                Left err -> pure (Left err)
                Right port -> do
                    let mDbPath :: Maybe String
                        mDbPath = firstJust rawOpts.rawDbPath (lookup envDbPath envList)

                        mTimeout :: Maybe String
                        mTimeout = firstJust rawOpts.rawBusyTimeoutMs (lookup envDbBusyTimeoutMs envList)

                        mStaticDir :: Maybe String
                        mStaticDir = firstJust rawOpts.rawStaticDir (lookup envStaticDir envList)
                    eStorage <- resolveStorageConfig mDbPath mTimeout
                    case eStorage of
                        Left err         -> pure (Left err)
                        Right storageCfg -> do
                            eStatic <- resolveStaticDir mStaticDir
                            pure $ fmap
                                (\ mDir -> WarpConfig
                                    { port      = port
                                    , storage   = storageCfg
                                    , staticDir = mDir
                                    }
                                )
                                eStatic
  where
    resolvePort :: Maybe String -> Maybe String -> Either Text ServerPort
    resolvePort mCli mEnv = case firstJust mCli mEnv of
        Nothing  -> Right defaultServerPort
        Just raw -> parsePortString raw

    resolveStaticDir :: Maybe String -> IO (Either Text (Maybe OS.OsPath))
    resolveStaticDir Nothing = pure (Right Nothing)
    resolveStaticDir (Just raw)
        | Txt.null (Txt.strip (Txt.pack raw)) = pure (Left "Static directory path must not be empty")
        | otherwise = do
            -- 環境変数・CLI 引数は base が String で返すため、境界で OsPath へエンコードする
            eOsPath <- try (OS.encodeFS raw) :: IO (Either IOException OS.OsPath)
            pure $ case eOsPath of
                Left ioErr   -> Left ("Invalid static directory path: " <> Txt.pack (show ioErr))
                Right osPath -> Right (Just osPath)

-- | CGI 環境変数一覧から CGI 実行設定を解決する。
resolveCgiConfig :: [(String, String)] -> IO (Either Text CgiConfig)
resolveCgiConfig envList = do
    let mDbPath :: Maybe String
        mDbPath = lookup envDbPath envList

        mTimeout :: Maybe String
        mTimeout = lookup envDbBusyTimeoutMs envList

        mScriptName :: Maybe Text
        mScriptName = fmap Txt.pack (lookup envScriptName envList)
    eStorage <- resolveStorageConfig mDbPath mTimeout
    pure $ fmap (\ storageCfg -> CgiConfig { scriptName = mScriptName, storage = storageCfg }) eStorage

-- | プロセスの CLI 引数および環境変数から Warp サーバ設定を読み込む。
loadWarpConfig :: IO (Either Text WarpConfig)
loadWarpConfig = do
    args <- Env.getArgs
    envList <- Env.getEnvironment
    resolveWarpConfig args envList

-- | プロセスの環境変数から CGI 実行設定を読み込む。
loadCgiConfig :: IO (Either Text CgiConfig)
loadCgiConfig = Env.getEnvironment >>= resolveCgiConfig

parsePortString :: String -> Either Text ServerPort
parsePortString raw =
    case parseStrictInteger raw of
        Nothing -> Left ("Invalid port number: " <> Txt.pack raw)
        Just n
            | n < 1 || n > 65535 -> Left ("Invalid port number: " <> Txt.pack raw)
            | otherwise          -> mkServerPort (fromInteger n)

parseBusyTimeoutString :: String -> Either Text BusyTimeoutMs
parseBusyTimeoutString raw =
    case parseStrictInteger raw of
        Nothing -> Left ("Invalid busy timeout value: " <> Txt.pack raw)
        Just n
            | n < 0 || n > toInteger maxBusyTimeoutMs ->
                Left
                    ( "Busy timeout must be between 0 and "
                        <> Txt.pack (show maxBusyTimeoutMs)
                        <> " ms, got "
                        <> Txt.pack raw
                    )
            | otherwise -> mkBusyTimeoutMs (fromInteger n)

-- | 空白や @+@ 符号・8進/16進表記を許さず、10進整数リテラル (@-?[0-9]+@) のみを厳格に解釈する。
parseStrictInteger :: String -> Maybe Integer
parseStrictInteger raw = case raw of
    ""               -> Nothing
    '-' : digits
        | not (null digits) && all Char.isDigit digits -> readMaybe raw
        | otherwise                                    -> Nothing
    digits
        | all Char.isDigit digits -> readMaybe raw
        | otherwise               -> Nothing

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing  y = y
