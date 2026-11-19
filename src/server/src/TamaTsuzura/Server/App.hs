{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Server.App
    ( ServerPort
    , mkServerPort
    , serverPortValue
    , defaultServerPort
    , parsePortArgs
    , application
    ) where

import qualified Data.Aeson             as Aeson
import           Data.Text              (Text)
import qualified Data.Text              as Txt
import qualified Network.HTTP.Types     as Http
import qualified Network.Wai            as Wai
import           TamaTsuzura.Common.Api (ApiError(..), ApiErrorCode(..), healthOk)
import           Text.Read              (readMaybe)

-- | HTTP サーバの待受ポート番号 (1〜65535)。
-- 不正なポート番号の構築を型レベルで防ぐためコンストラクタは非公開とし、'mkServerPort' を経由する。
newtype ServerPort = ServerPort
    { unWrap :: Int
    } deriving (Show, Eq, Ord)

-- | 1〜65535 の範囲を検証して 'ServerPort' を構築するスマートコンストラクタ。
mkServerPort :: Int -> Either Text ServerPort
mkServerPort port | port >= 1 && port <= 65535 = Right (ServerPort port)
                  | otherwise                  = Left ("Invalid port number: " <> Txt.pack (show port))

-- | 検証済み 'ServerPort' から生の 'Int' ポート番号を取り出す。
serverPortValue :: ServerPort -> Int
serverPortValue (ServerPort port) = port

-- | 開発用 Warp サーバの既定ポート番号 (8080)。
defaultServerPort :: ServerPort
defaultServerPort = ServerPort 8080

-- | コマンドライン引数から待受ポートを解釈する。
-- 未指定時は 'defaultServerPort' を返し、不正な引数や範囲外のポートは 'Left' で通知する。
parsePortArgs :: [String] -> Either Text ServerPort
parsePortArgs []              = Right defaultServerPort
parsePortArgs ["--port", raw] = parsePortValue raw
parsePortArgs _               = Left "Usage: tamatsuzura-server [--port PORT]"

parsePortValue :: String -> Either Text ServerPort
parsePortValue raw = case readMaybe raw of
    Just port -> mkServerPort port
    Nothing   -> Left ("Invalid port number: " <> Txt.pack raw)

-- | CGI と Warp の双方で共有される完全ステートレスな WAI アプリケーション。
application :: Wai.Application
application req respond = case (Wai.requestMethod req, Wai.pathInfo req) of
    ("GET", ["api", "health"]) ->
        respond $ Wai.responseLBS
            Http.status200
            [(Http.hContentType, "application/json; charset=utf-8")]
            (Aeson.encode healthOk)
    _                          ->
        respond $ Wai.responseLBS
            Http.status404
            [(Http.hContentType, "application/json; charset=utf-8")]
            (Aeson.encode notFoundError)
  where
    notFoundError :: ApiError
    notFoundError = ApiError
        { code    = ErrNotFound
        , message = "Endpoint not found"
        , field   = Nothing
        }
