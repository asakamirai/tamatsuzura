{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | 行ソート・重複行除去・全角英数半角化などの汎用テキスト変換を提供する同梱拡張。
-- 既定では無効 (@defaultEnabled = False@) とし、拡張管理 UI からの有効化・実行フローを検証可能にする。
module TamaTsuzura.Extensions.TextTransform
    ( textTransformExtension
    , sortDocumentLines
    , dedupeDocumentLines
    , normalizeFullwidthAlnum
    ) where

import qualified Data.Char                       as Char
import           Data.List                       (sort)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Txt
import           TamaTsuzura.Common.Document     (DocumentBody(..))
import           TamaTsuzura.Common.Text         (ToText(..))
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

-- | 汎用テキスト変換コマンド群を提供する同梱拡張 (オプトイン既定無効)。
textTransformExtension :: Extension
textTransformExtension = Extension
    { metadata = ExtensionMetadata
        { extensionId    = ExtensionId "text-transform"
        , name           = ExtensionName "テキスト変換"
        , description    = "行の昇順ソート・重複行の除去・全角英数の半角統一を行います。"
        , version        = defaultExtensionVersion
        , category       = CategoryTextTransform
        , defaultEnabled = False
        }
    , commands =
        [ ExtensionCommand
            { commandId    = CommandId "sort-lines"
            , label        = CommandName "行を昇順ソート"
            , description  = "本文の全行を辞書順 (昇順) に並び替えます。"
            , markdownOnly = False
            , runCommand   = \ ctx ->
                Right ExtensionResult
                    { nextBody = sortDocumentLines ctx.body
                    , summary  = Just "行を昇順ソートしました。"
                    }
            }
        , ExtensionCommand
            { commandId    = CommandId "dedupe-lines"
            , label        = CommandName "重複行を除去"
            , description  = "初出順を保ったまま重複する行を除去します。"
            , markdownOnly = False
            , runCommand   = \ ctx ->
                Right ExtensionResult
                    { nextBody = dedupeDocumentLines ctx.body
                    , summary  = Just "重複する行を除去しました。"
                    }
            }
        , ExtensionCommand
            { commandId    = CommandId "normalize-fullwidth-alnum"
            , label        = CommandName "全角英数を半角へ統一"
            , description  = "全角の英数字 (０-９, Ａ-Ｚ, ａ-ｚ) と全角スペースを半角へ変換します。"
            , markdownOnly = False
            , runCommand   = \ ctx ->
                Right ExtensionResult
                    { nextBody = normalizeFullwidthAlnum ctx.body
                    , summary  = Just "全角英数字を半角へ統一しました。"
                    }
            }
        ]
    , transformBuffer = normalizeFullwidthAlnum
    }

-- | 本文の各行を昇順に並び替える (CRLF は LF へ正規化し、末尾改行の有無を保持)。
sortDocumentLines :: DocumentBody -> DocumentBody
sortDocumentLines = mapLinesPreservingEnd sort

-- | 本文から重複行を初出順を保って除去する (CRLF は LF へ正規化し、末尾改行の有無を保持)。
dedupeDocumentLines :: DocumentBody -> DocumentBody
dedupeDocumentLines = mapLinesPreservingEnd (go Set.empty)
  where
    go :: Set Text -> [Text] -> [Text]
    go _    []            = []
    go seen (line : rest)
        | Set.member line seen = go seen rest
        | otherwise            = line : go (Set.insert line seen) rest

-- | 全角英数字 (@０-９@, @Ａ-Ｚ@, @ａ-ｚ@) および全角スペース (@　@) を半角へ変換する。
normalizeFullwidthAlnum :: DocumentBody -> DocumentBody
normalizeFullwidthAlnum body = DocumentBody (Txt.map convertChar (toText body))
  where
    convertChar :: Char -> Char
    convertChar ch
        | ch == '\x3000'      = ' '
        | isFullwidthAlnum ch = Char.chr (Char.ord ch - 0xFEE0)
        | otherwise           = ch

    isFullwidthAlnum :: Char -> Bool
    isFullwidthAlnum ch =
        (ch >= '\xFF10' && ch <= '\xFF19')       -- ０-９
            || (ch >= '\xFF21' && ch <= '\xFF3A') -- Ａ-Ｚ
            || (ch >= '\xFF41' && ch <= '\xFF5A') -- ａ-ｚ
