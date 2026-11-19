module Main
    ( main
    ) where

import qualified Network.Wai.Handler.CGI as CGI
import           TamaTsuzura.Server.App  (application)

-- | レンタルサーバ等の CGI 環境向けエントリポイント。
-- 1 リクエスト 1 プロセスで起動され、共通の WAI 'application' へ委譲する。
main :: IO ()
main = CGI.run application
