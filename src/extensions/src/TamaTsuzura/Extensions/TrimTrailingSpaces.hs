{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | 行末の不要な水平空白および過剰な連続空行を整理する同梱拡張。
module TamaTsuzura.Extensions.TrimTrailingSpaces
    ( trimTrailingSpacesExtension
    , trimTrailingSpaces
    , normalizeBlankLines
    ) where

import           Data.Text                       (Text)
import qualified Data.Text                       as Txt
import           TamaTsuzura.Common.Document     (DocumentBody(..))
import           TamaTsuzura.ExtensionApi
    ( CommandId(..)
    , CommandName(..)
    , Extension(..)
    , ExtensionCategory(..)
    , ExtensionCommand(..)
    , ExtensionContext(..)
    , ExtensionId(..)
    , ExtensionMetadata(..)
    , ExtensionName(..)
    , ExtensionResult(..)
    , defaultExtensionVersion
    )
import           TamaTsuzura.Extensions.Internal (mapLinesPreservingEnd)

-- | 各行の末尾空白除去および連続空行整理を提供する同梱拡張。
trimTrailingSpacesExtension :: Extension
trimTrailingSpacesExtension = Extension
    { metadata = ExtensionMetadata
        { extensionId    = ExtensionId "trim-trailing-spaces"
        , name           = ExtensionName "空白・改行の整形"
        , description    = "各行末の不要な空白や過剰な連続空行を整理します。"
        , version        = defaultExtensionVersion
        , category       = CategoryFormatting
        , defaultEnabled = True
        }
    , commands =
        [ ExtensionCommand
            { commandId    = CommandId "trim"
            , label        = CommandName "行末空白の除去"
            , description  = "各行の末尾にある半角スペースとタブを除去します。"
            , markdownOnly = False
            , runCommand   = \ ctx ->
                let next :: DocumentBody
                    next = trimTrailingSpaces ctx.body
                in  Right ExtensionResult
                        { nextBody = next
                        , summary  = Just "行末の不要な空白を除去しました。"
                        }
            }
        , ExtensionCommand
            { commandId    = CommandId "normalize-blank-lines"
            , label        = CommandName "連続空行の整理"
            , description  = "2 行以上連続する空行を 1 行へ圧縮し、末尾の余分な空行を整理します。"
            , markdownOnly = False
            , runCommand   = \ ctx ->
                let next :: DocumentBody
                    next = normalizeBlankLines ctx.body
                in  Right ExtensionResult
                        { nextBody = next
                        , summary  = Just "連続する空行を整理しました。"
                        }
            }
        ]
    , transformBuffer = trimTrailingSpaces
    }

-- | 本文各行の末尾から水平空白を除去する純粋関数 (CRLF は LF へ正規化する)。
-- 末尾改行の有無を保持するため、行リストと末尾改行フラグを分離して処理する。
trimTrailingSpaces :: DocumentBody -> DocumentBody
trimTrailingSpaces = mapLinesPreservingEnd (map trimLine)
  where
    trimLine :: Text -> Text
    trimLine = Txt.dropWhileEnd isHorizontalSpace

    isHorizontalSpace :: Char -> Bool
    isHorizontalSpace ch = ch == ' ' || ch == '\t'

-- | 2 行以上連続する空行を最大 1 行へ圧縮し、末尾の連続空行を取り除く純粋関数 (CRLF は LF へ正規化する)。
-- 空文書は空のまま保持し、非空文書では元の末尾改行の有無を尊重する。
normalizeBlankLines :: DocumentBody -> DocumentBody
normalizeBlankLines = mapLinesPreservingEnd (dropTrailingBlanks . collapseBlanks False)
  where
    isBlankLine :: Text -> Bool
    isBlankLine = Txt.null . Txt.strip

    collapseBlanks :: Bool -> [Text] -> [Text]
    collapseBlanks _         []            = []
    collapseBlanks prevBlank (line : rest)
        | isBlankLine line =
            if prevBlank
                then collapseBlanks True rest
                else "" : collapseBlanks True rest
        | otherwise =
            line : collapseBlanks False rest

    dropTrailingBlanks :: [Text] -> [Text]
    dropTrailingBlanks = reverse . dropWhile isBlankLine . reverse
