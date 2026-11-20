{-# LANGUAGE OverloadedRecordDot #-}

module Main
    ( main
    ) where

import qualified Data.Text.IO              as TxtIO
import qualified Network.Wai.Handler.Warp  as Warp
import qualified System.Exit               as Exit
import qualified System.IO                 as Sys
import           TamaTsuzura.Server.App    (mkApplication)
import           TamaTsuzura.Server.Config (WarpConfig(..), loadWarpConfig, serverPortValue)

-- | ローカル開発およびスタンドアロン運用向けの Warp HTTP サーバエントリポイント。
main :: IO ()
main = do
    eCfg <- loadWarpConfig
    case eCfg of
        Left errMsg -> do
            TxtIO.hPutStrLn Sys.stderr errMsg
            Exit.exitFailure
        Right cfg   ->
            Warp.run (serverPortValue cfg.port) (mkApplication cfg.storage)
