module Main
    ( main
    ) where

import qualified Data.Text.IO             as Txt
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Environment       as Env
import qualified System.Exit              as Exit
import qualified System.IO                as Sys
import           TamaTsuzura.Server.App   (application, parsePortArgs, serverPortValue)

-- | ローカル開発およびスタンドアロン運用向けの Warp HTTP サーバエントリポイント。
main :: IO ()
main = do
    args <- Env.getArgs
    case parsePortArgs args of
        Left errMsg -> do
            Txt.hPutStrLn Sys.stderr errMsg
            Exit.exitFailure
        Right port  ->
            Warp.run (serverPortValue port) application
