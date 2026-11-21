{-# LANGUAGE OverloadedStrings #-}

-- | 同梱拡張モジュール間で共有する非公開の改行正規化・行分割ヘルパ。
module TamaTsuzura.Extensions.Internal
    ( normalizeLineEndings
    , splitLinesPreservingEnd
    , joinLinesPreservingEnd
    , mapLinesPreservingEnd
    ) where

import           Data.Text                   (Text)
import qualified Data.Text                   as Txt
import           TamaTsuzura.Common.Document (DocumentBody(..))
import           TamaTsuzura.Common.Text     (ToText(..))

-- | CRLF (@\r\n@) および単独 CR (@\r@) を LF (@\n@) へ統一する。
-- インポート文書等で混入した CRLF による行末処理の不整合や改行混在を防ぐ。
normalizeLineEndings :: Text -> Text
normalizeLineEndings =
    Txt.replace "\r" "\n" . Txt.replace "\r\n" "\n"

-- | 改行を LF へ正規化した上で、行リストと末尾改行の有無へ分解する。
-- 空文字列は @([], False)@ を返す。
splitLinesPreservingEnd :: Text -> ([Text], Bool)
splitLinesPreservingEnd raw =
    let normalized :: Text
        normalized = normalizeLineEndings raw
    in  if Txt.null normalized
            then ([], False)
            else case Txt.stripSuffix "\n" normalized of
                Just withoutTrailing -> (Txt.splitOn "\n" withoutTrailing, True)
                Nothing              -> (Txt.splitOn "\n" normalized, False)

-- | 行リストと末尾改行フラグから元の末尾改行有無を保って 'Text' へ再結合する。
joinLinesPreservingEnd :: [Text] -> Bool -> Text
joinLinesPreservingEnd []    _                  = ""
joinLinesPreservingEnd lines' hadTrailingNewline =
    let joined :: Text
        joined = Txt.intercalate "\n" lines'
    in  if hadTrailingNewline then joined <> "\n" else joined

-- | 'DocumentBody' の改行を LF 正規化し、行リスト変換関数を適用して末尾改行有無を保ちつつ復元する。
mapLinesPreservingEnd :: ([Text] -> [Text]) -> DocumentBody -> DocumentBody
mapLinesPreservingEnd fn body =
    let (ls, hadTrailingNewline) = splitLinesPreservingEnd (toText body)
    in  DocumentBody (joinLinesPreservingEnd (fn ls) hadTrailingNewline)
