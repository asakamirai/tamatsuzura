-- | TamaTsuzura に同梱される標準 Haskell 拡張機能の一覧カタログ。
module TamaTsuzura.Extensions
    ( allBundledExtensions
    ) where

import           TamaTsuzura.ExtensionApi                  (Extension)
import           TamaTsuzura.Extensions.MarkdownFormatter  (markdownFormatterExtension)
import           TamaTsuzura.Extensions.TextTransform      (textTransformExtension)
import           TamaTsuzura.Extensions.TrimTrailingSpaces (trimTrailingSpacesExtension)

-- | M5 でクライアントへ同梱提供するすべての標準拡張一覧。
allBundledExtensions :: [Extension]
allBundledExtensions =
    [ trimTrailingSpacesExtension
    , markdownFormatterExtension
    , textTransformExtension
    ]
