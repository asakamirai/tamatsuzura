{-# LANGUAGE CPP #-}

module Main
    ( main
    ) where

import           TamaTsuzura.Client.App               (ClientState, initialClientState)
import           TamaTsuzura.Client.ExtensionRegistry (bundledExtensions)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-- | GHC Wasm クライアントの最小エントリポイント (M0 骨格)。
-- M2 で Miso コンポーネント起動および CodeMirror 6 JSFFI 連携へ拡張する。
main :: IO ()
main = do
    let state :: ClientState
        state = initialClientState bundledExtensions
    seq state (pure ())
