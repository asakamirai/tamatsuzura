{-# LANGUAGE OverloadedStrings #-}

module TamaTsuzura.Extensions.TrimTrailingSpaces
    ( trimTrailingSpacesExtension
    , trimTrailingSpaces
    ) where

import           Data.Text                   (Text)
import qualified Data.Text                   as Txt
import           TamaTsuzura.Common.Document (DocumentBody(..))
import           TamaTsuzura.Common.Text     (ToText(..))
import           TamaTsuzura.ExtensionApi    (Extension(..), ExtensionId(..), ExtensionMetadata(..), ExtensionName(..))

-- | 各行の末尾にある水平空白 (半角スペース・タブ) を除去する同梱サンプル拡張。
trimTrailingSpacesExtension :: Extension
trimTrailingSpacesExtension = Extension
    { metadata        = ExtensionMetadata
        { extensionId = ExtensionId "trim-trailing-spaces"
        , name        = ExtensionName "行末空白の除去"
        , description = "各行の末尾にある不要な空白文字を除去します。"
        }
    , transformBuffer = trimTrailingSpaces
    }

-- | 本文各行の末尾から水平空白を除去する純粋関数 (LF 改行前提)。
-- 末尾改行の有無を保持するため、'Txt.lines' ではなく 'Txt.splitOn' を用いる。
trimTrailingSpaces :: DocumentBody -> DocumentBody
trimTrailingSpaces body =
    DocumentBody . Txt.intercalate "\n" . map trimLine . Txt.splitOn "\n" $ toText body
  where
    trimLine :: Text -> Text
    trimLine = Txt.dropWhileEnd isHorizontalSpace

    isHorizontalSpace :: Char -> Bool
    isHorizontalSpace ch = ch == ' ' || ch == '\t'
