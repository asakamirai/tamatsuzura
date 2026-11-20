{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import qualified Data.Text.IO              as TxtIO
import qualified Network.Wai.Handler.CGI   as CGI
import qualified System.IO                 as Sys
import           TamaTsuzura.Server.App    (configErrorApplication, mkCgiApplication)
import           TamaTsuzura.Server.Config (loadCgiConfig)

-- | レンタルサーバ等の CGI 環境向けエントリポイント。
-- 1 リクエスト 1 プロセスで起動され、環境変数からストレージ設定を読み込んだ上で
-- 共通の WAI アプリケーションへ委譲する。
main :: IO ()
main = do
    eCfg <- loadCgiConfig
    case eCfg of
        Left errMsg -> do
            TxtIO.hPutStrLn Sys.stderr ("[tamatsuzura-cgi] Configuration error: " <> errMsg)
            CGI.run configErrorApplication
        Right cfg   ->
            CGI.run (mkCgiApplication cfg)
