module TamaTsuzura.Client.ExtensionRegistry
    ( bundledExtensions
    ) where

import           TamaTsuzura.ExtensionApi (Extension)
import           TamaTsuzura.Extensions   (allBundledExtensions)

-- | M5 の同梱拡張をクライアントへ供給する結線層 (Composition Root)。
-- 'src/client/' 内で 'tamatsuzura-extensions' への参照を本モジュールに限定することで、
-- 将来の外部 Wasm 動的ロード移行時の差し替え範囲を最小化する。
bundledExtensions :: [Extension]
bundledExtensions = allBundledExtensions
