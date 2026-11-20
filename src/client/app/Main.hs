{-# LANGUAGE CPP #-}

module Main
    ( main
    ) where

#ifdef WASM
import           Miso                                 (defaultEvents, startApp)
#endif
import           TamaTsuzura.Client.Api               (defaultApiClient, mkApiBaseUrl)
import           TamaTsuzura.Client.App               (ClientEnv(..), mkClientComponent)
import           TamaTsuzura.Client.ExtensionRegistry (bundledExtensions)
import           TamaTsuzura.Client.Ffi               (readConfiguredApiBaseUrl)
import           TamaTsuzura.Client.I18n              (jaMessages)
import           TamaTsuzura.Client.View              (viewClient)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-- | GHC Wasm (Miso) クライアントのエントリポイント。
-- ブラウザ実行時 (Wasm) は HTML メタタグ等から API ベース URL を読み取り、
-- 同梱拡張レジストリを注入して Miso アプリケーションをマウントする。
main :: IO ()
main = do
    rawBaseUrl <- readConfiguredApiBaseUrl
    let env :: ClientEnv
        env = ClientEnv
            { apiClient  = defaultApiClient (mkApiBaseUrl rawBaseUrl)
            , extensions = bundledExtensions
            , messages   = jaMessages
            }
#ifdef WASM
    startApp defaultEvents (mkClientComponent env viewClient)
#else
    -- ネイティブバイナリはビルド疎通・テスト専用のため DOM 起動を行わず安全に終了する
    seq (mkClientComponent env viewClient) (pure ())
#endif
