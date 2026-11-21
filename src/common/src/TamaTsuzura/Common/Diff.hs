{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module TamaTsuzura.Common.Diff
    ( -- * 差分データ型
      UnifiedDiff(..)
    , emptyUnifiedDiff
    , DiffLineKind(..)
    , DiffLine(..)
    , DiffHunk(..)
    , ParsedDiff(..)
    , DiffError(..)
      -- * 差分生成・シリアライズ・パース
    , defaultContextLines
    , generateDiff
    , generateDiffWithContext
    , generateUnifiedDiff
    , generateUnifiedDiffWithContext
    , renderUnifiedDiff
    , parseUnifiedDiff
      -- * 差分適用・反転
    , applyParsedDiff
    , applyUnifiedDiff
    , reverseParsedDiff
    , reverseUnifiedDiff
    , applyUnifiedDiffReverse
    ) where

import           Data.Aeson              (FromJSON(..), ToJSON(..))
import qualified Data.Char               as Char
import qualified Data.Foldable           as Foldable
import           Data.IntMap.Strict      (IntMap)
import qualified Data.IntMap.Strict      as IntMap
import           Data.Sequence           (Seq)
import qualified Data.Sequence           as Seq
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text               as Txt
import           TamaTsuzura.Common.Text (ToText(..))

-- | unified diff 形式の文字列表現を包む newtype。
-- 変更が存在しない場合は空文字列 ('emptyUnifiedDiff') で表現する。
newtype UnifiedDiff = UnifiedDiff
    { unWrap :: Text
      -- ^ unified diff 本文テキスト
    } deriving (Eq, Ord, IsString, ToText)

instance Show UnifiedDiff where
    show = Txt.unpack . toText

instance ToJSON UnifiedDiff where
    toJSON = toJSON . toText

instance FromJSON UnifiedDiff where
    parseJSON v = UnifiedDiff <$> parseJSON v

-- | 変更が存在しないことを表す空の 'UnifiedDiff'。
emptyUnifiedDiff :: UnifiedDiff
emptyUnifiedDiff = UnifiedDiff ""

-- | unified diff の hunk 内に現れる行種別。
data DiffLineKind
    = ContextLine
      -- ^ 変更前後の共通コンテキスト行 (先頭 @' '@)
    | RemovedLine
      -- ^ 旧テキストから削除された行 (先頭 @'-'@)
    | AddedLine
      -- ^ 新テキストへ追加された行 (先頭 @'+'@)
    deriving (Eq, Ord, Enum, Bounded, Show)

-- | unified diff の 1 行分の表現。
-- 末尾改行 (@\n@) の有無を明示的に保持することで、末尾改行のないファイルや
-- 末尾改行の有無のみが変化する差分も完全に可逆的に表現する。
data DiffLine = DiffLine
    { kind       :: !DiffLineKind
      -- ^ 行種別 (コンテキスト / 削除 / 追加)
    , content    :: !Text
      -- ^ 末尾 @\n@ を除いた行内容 (@\r@ はそのまま保持)
    , hasNewline :: !Bool
      -- ^ この行が @\n@ で終端されているか ('False' の場合は @\\ No newline at end of file@ を伴う)
    } deriving (Eq, Show)

-- | unified diff の 1 つの hunk (@\@\@ -oldStart,oldCount +newStart,newCount \@\@@)。
data DiffHunk = DiffHunk
    { oldStart :: !Int
      -- ^ 旧テキスト側の開始行番号 (1 始まり。@oldCount == 0@ の場合は挿入直前の行番号、先頭なら 0)
    , oldCount :: !Int
      -- ^ 旧テキスト側の対象行数 (コンテキスト行 + 削除行)
    , newStart :: !Int
      -- ^ 新テキスト側の開始行番号 (1 始まり。@newCount == 0@ の場合は削除直前の行番号、先頭なら 0)
    , newCount :: !Int
      -- ^ 新テキスト側の対象行数 (コンテキスト行 + 追加行)
    , lines    :: ![DiffLine]
      -- ^ hunk を構成する差分行リスト
    } deriving (Eq, Show)

-- | パースまたは生成された構造化 unified diff。
newtype ParsedDiff = ParsedDiff
    { hunks :: [DiffHunk]
      -- ^ 昇順・非重複に並んだ hunk 列
    } deriving (Eq, Show)

-- | unified diff のパースまたは適用に失敗した際の構造化エラー。
data DiffError
    = MalformedHunkHeader !Int !Text
      -- ^ hunk ヘッダの構文が不正である (1 始まり行番号, 該当行)
    | UnexpectedDiffLine !Int !Text
      -- ^ hunk 外または不正なプレフィックスを持つ行が現れた (1 始まり行番号, 該当行)
    | UnexpectedNoNewlineMarker !Int
      -- ^ 対象行が存在しない位置に @\\ No newline at end of file@ が現れた (1 始まり行番号)
    | HunkLineCountMismatch !Int !Int !Int
      -- ^ hunk ヘッダの行数宣言と実体行数が一致しない (1 始まり hunk 番号, 期待行数, 実際行数)
    | HunkOutOfRange !Int !Int !Int
      -- ^ hunk の参照範囲が元テキストの行数を超過している (1 始まり hunk 番号, 1 始まり参照行番号, 元テキスト行数)
    | HunkContextMismatch !Int !Int !Text !Text
      -- ^ 適用時のコンテキスト行または削除行の内容が元テキストと一致しない (hunk 番号, 元テキスト 1 始まり行番号, 期待行, 実際行)
    | HunkNewlineMismatch !Int !Int
      -- ^ 適用時の行の末尾改行有無が元テキストと一致しない、またはファイル末尾以外の行で改行が欠落している (hunk 番号, 1 始まり行番号)
    | OverlappingHunks !Int
      -- ^ hunk が前の hunk と重複または逆転している (1 始まり hunk 番号)
    | HunkOffsetMismatch !Int !Int !Int
      -- ^ hunk 間の未変更スキップ行数が旧側と新側で一致しない (1 始まり hunk 番号, 旧側スキップ行数, 新側スキップ行数)
    deriving (Eq)

instance ToText DiffError where
    toText (MalformedHunkHeader lineNo raw) =
        "Malformed hunk header at line " <> showInt lineNo <> ": " <> raw
    toText (UnexpectedDiffLine lineNo raw) =
        "Unexpected diff line at line " <> showInt lineNo <> ": " <> raw
    toText (UnexpectedNoNewlineMarker lineNo) =
        "Unexpected no-newline marker at line " <> showInt lineNo
    toText (HunkLineCountMismatch hunkNo expected actual) =
        "Hunk #" <> showInt hunkNo <> " line count mismatch: expected " <> showInt expected <> ", got " <> showInt actual
    toText (HunkOutOfRange hunkNo startLine totalLines) =
        "Hunk #" <> showInt hunkNo <> " references line " <> showInt startLine <> " out of range (" <> showInt totalLines <> " lines)"
    toText (HunkContextMismatch hunkNo lineNo expected actual) =
        "Hunk #" <> showInt hunkNo <> " mismatch at line " <> showInt lineNo
            <> ": expected " <> expected <> ", got " <> actual
    toText (HunkNewlineMismatch hunkNo lineNo) =
        "Hunk #" <> showInt hunkNo <> " trailing newline mismatch at line " <> showInt lineNo
    toText (OverlappingHunks hunkNo) =
        "Hunk #" <> showInt hunkNo <> " overlaps or precedes the previous hunk"
    toText (HunkOffsetMismatch hunkNo oldSkip newSkip) =
        "Hunk #" <> showInt hunkNo <> " offset mismatch: old skipped " <> showInt oldSkip <> " lines, new skipped " <> showInt newSkip <> " lines"

instance Show DiffError where
    show = Txt.unpack . toText

-- | unified diff 生成時の既定コンテキスト行数 (標準 @diff -u@ と同じ 3 行)。
defaultContextLines :: Int
defaultContextLines = 3

-- | Myers O(ND) 探索を打ち切って全置換 hunk へ安全フォールバックする最大編集距離。
-- 数 MB 級の完全書き換え時に O(D^2) の探索トレースによるメモリ膨張とタイムアウトを防ぐ。
maxMyersEditDistance :: Int
maxMyersEditDistance = 4096

-- | @\\ No newline at end of file@ メタマーカーの固定文字列。
noNewlineMarker :: Text
noNewlineMarker = "\\ No newline at end of file"

-- | 改行終端フラグ付きの内部行表現。
data TextLine = TextLine
    { content    :: !Text
    , hasNewline :: !Bool
    } deriving (Eq, Show)

-- | 内部編集スクリプトの各操作。
data EditOp
    = EditEqual !TextLine
    | EditRemove !TextLine
    | EditAdd !TextLine
    deriving (Eq, Show)

-- | 既定コンテキスト行数 ('defaultContextLines') を用いて 2 つのテキストから構造化差分を生成する。
generateDiff :: Text -> Text -> ParsedDiff
generateDiff = generateDiffWithContext defaultContextLines

-- | コンテキスト行数を指定して 2 つのテキストから構造化差分を生成する。
-- コンテキスト行数に負数が指定された場合は 0 として扱う。
-- 先頭・末尾の共通行を O(N) で除外した上で中間区間へ Myers O(ND) diff を適用する。
generateDiffWithContext :: Int -> Text -> Text -> ParsedDiff
generateDiffWithContext rawContext oldText newText
    | oldText == newText = ParsedDiff []
    | otherwise          =
        let ctx :: Int
            ctx = max 0 rawContext

            oldLines :: [TextLine]
            oldLines = splitLinesWithNewline oldText

            newLines :: [TextLine]
            newLines = splitLinesWithNewline newText

            editOps :: [EditOp]
            editOps = normalizeEditBlocks (computeEditScript oldLines newLines)
        in  ParsedDiff (buildHunks ctx editOps)

-- | 既定コンテキスト行数 ('defaultContextLines') を用いて unified diff テキストを生成する。
generateUnifiedDiff :: Text -> Text -> UnifiedDiff
generateUnifiedDiff = generateUnifiedDiffWithContext defaultContextLines

-- | コンテキスト行数を指定して unified diff テキストを生成する。
generateUnifiedDiffWithContext :: Int -> Text -> Text -> UnifiedDiff
generateUnifiedDiffWithContext ctx oldText newText =
    renderUnifiedDiff (generateDiffWithContext ctx oldText newText)

-- | 構造化 'ParsedDiff' を標準 unified diff 形式の 'UnifiedDiff' へレンダリングする。
renderUnifiedDiff :: ParsedDiff -> UnifiedDiff
renderUnifiedDiff parsed
    | null parsed.hunks = emptyUnifiedDiff
    | otherwise         = UnifiedDiff (Txt.concat (map renderHunk parsed.hunks))
  where
    renderHunk :: DiffHunk -> Text
    renderHunk hunk =
        "@@ -"
            <> renderRange hunk.oldStart hunk.oldCount
            <> " +"
            <> renderRange hunk.newStart hunk.newCount
            <> " @@\n"
            <> Txt.concat (map renderLine hunk.lines)

    renderRange :: Int -> Int -> Text
    renderRange start count
        | count == 1 = showInt start
        | otherwise  = showInt start <> "," <> showInt count

    renderLine :: DiffLine -> Text
    renderLine dl =
        let prefix :: Text
            prefix = case dl.kind of
                ContextLine -> " "
                RemovedLine -> "-"
                AddedLine   -> "+"

            baseLine :: Text
            baseLine = prefix <> dl.content <> "\n"
        in  if dl.hasNewline
                then baseLine
                else baseLine <> noNewlineMarker <> "\n"

-- | unified diff テキストを検証しつつ構造化 'ParsedDiff' へパースする。
-- 空文字列は変更なし ('ParsedDiff' @[]@) として受理し、先頭に @--- ...@ / @+++ ...@ の
-- ファイルヘッダペアが存在する場合は外部 @diff -u@ / @git diff@ 互換のため読み飛ばす。
parseUnifiedDiff :: UnifiedDiff -> Either DiffError ParsedDiff
parseUnifiedDiff diff
    | Txt.null (toText diff) = Right (ParsedDiff [])
    | otherwise              =
        let numbered :: [(Int, Text)]
            numbered = zip [1 ..] (map (\ tl -> tl.content) (splitLinesWithNewline (toText diff)))

            afterHeaders :: [(Int, Text)]
            afterHeaders = skipFileHeaders numbered
        in  do
                hunks <- parseHunks 1 afterHeaders
                validateHunks 1 0 0 False False hunks
                Right (ParsedDiff hunks)
  where
    skipFileHeaders :: [(Int, Text)] -> [(Int, Text)]
    skipFileHeaders ((_, l1) : (_, l2) : rest)
        | Txt.isPrefixOf "--- " l1 && Txt.isPrefixOf "+++ " l2 = rest
    skipFileHeaders ls = ls

    parseHunks :: Int -> [(Int, Text)] -> Either DiffError [DiffHunk]
    parseHunks _      []                         = Right []
    parseHunks hunkNo ((lineNo, headerTxt) : rest)
        | Txt.isPrefixOf "@@ " headerTxt = do
            (oStart, oCount, nStart, nCount) <- parseHunkHeader lineNo headerTxt
            (bodyLines, remaining) <- parseHunkBody rest []
            let actualOld :: Int
                actualOld = countOldLines bodyLines

                actualNew :: Int
                actualNew = countNewLines bodyLines
            if actualOld /= oCount
                then Left (HunkLineCountMismatch hunkNo oCount actualOld)
                else if actualNew /= nCount
                    then Left (HunkLineCountMismatch hunkNo nCount actualNew)
                    else do
                        let hunk :: DiffHunk
                            hunk = DiffHunk
                                { oldStart = oStart
                                , oldCount = oCount
                                , newStart = nStart
                                , newCount = nCount
                                , lines    = bodyLines
                                }
                        moreHunks <- parseHunks (hunkNo + 1) remaining
                        Right (hunk : moreHunks)
        | otherwise = Left (UnexpectedDiffLine lineNo headerTxt)

    parseHunkBody :: [(Int, Text)] -> [DiffLine] -> Either DiffError ([DiffLine], [(Int, Text)])
    parseHunkBody [] acc = Right (reverse acc, [])
    parseHunkBody remaining@((lineNo, raw) : rest) acc
        | Txt.isPrefixOf "@@ " raw = Right (reverse acc, remaining)
        | raw == noNewlineMarker   = case acc of
            []        -> Left (UnexpectedNoNewlineMarker lineNo)
            prev : ps
                | not prev.hasNewline -> Left (UnexpectedNoNewlineMarker lineNo)
                | otherwise           ->
                    let updated :: DiffLine
                        updated = DiffLine
                            { kind       = prev.kind
                            , content    = prev.content
                            , hasNewline = False
                            }
                    in  parseHunkBody rest (updated : ps)
        | otherwise = case Txt.uncons raw of
            Just (' ', body) ->
                parseHunkBody rest (DiffLine ContextLine body True : acc)
            Just ('-', body) ->
                parseHunkBody rest (DiffLine RemovedLine body True : acc)
            Just ('+', body) ->
                parseHunkBody rest (DiffLine AddedLine body True : acc)
            _ ->
                Left (UnexpectedDiffLine lineNo raw)

    validateHunks :: Int -> Int -> Int -> Bool -> Bool -> [DiffHunk] -> Either DiffError ()
    validateHunks _      _          _          _        _        []         = Right ()
    validateHunks hunkNo prevOldEnd prevNewEnd oldEnded newEnded (h : rest) =
        let oldStartIdx :: Int
            oldStartIdx = hunkOldStartIndex h

            newStartIdx :: Int
            newStartIdx = hunkNewStartIndex h

            oldSkip :: Int
            oldSkip = oldStartIdx - prevOldEnd

            newSkip :: Int
            newSkip = newStartIdx - prevNewEnd
        in  if oldSkip < 0 || newSkip < 0
                then Left (OverlappingHunks hunkNo)
                else if oldSkip /= newSkip
                    then Left (HunkOffsetMismatch hunkNo oldSkip newSkip)
                    else if (oldEnded || newEnded) && oldSkip > 0
                        then Left (HunkNewlineMismatch hunkNo (prevOldEnd + 1))
                        else do
                            (nextOldEnded, nextNewEnded) <- validateHunkNewlines hunkNo (oldStartIdx + 1) oldEnded newEnded h.lines
                            validateHunks
                                (hunkNo + 1)
                                (oldStartIdx + h.oldCount)
                                (newStartIdx + h.newCount)
                                nextOldEnded
                                nextNewEnded
                                rest

    validateHunkNewlines :: Int -> Int -> Bool -> Bool -> [DiffLine] -> Either DiffError (Bool, Bool)
    validateHunkNewlines _      _      !oldEnded !newEnded []          = Right (oldEnded, newEnded)
    validateHunkNewlines hunkNo !lineNo !oldEnded !newEnded (dl : rest) = case dl.kind of
        ContextLine
            | oldEnded || newEnded -> Left (HunkNewlineMismatch hunkNo lineNo)
            | otherwise            -> validateHunkNewlines hunkNo (lineNo + 1) (not dl.hasNewline) (not dl.hasNewline) rest
        RemovedLine
            | oldEnded  -> Left (HunkNewlineMismatch hunkNo lineNo)
            | otherwise -> validateHunkNewlines hunkNo (lineNo + 1) (not dl.hasNewline) newEnded rest
        AddedLine
            | newEnded  -> Left (HunkNewlineMismatch hunkNo lineNo)
            | otherwise -> validateHunkNewlines hunkNo lineNo oldEnded (not dl.hasNewline) rest

-- | 構造化 'ParsedDiff' を旧テキストへ厳格に適用し、新テキストを再構成する。
-- コンテキスト行や削除行の内容・末尾改行有無が一致しない場合は 'Left' を返す。
applyParsedDiff :: Text -> ParsedDiff -> Either DiffError Text
applyParsedDiff oldText parsed
    | null parsed.hunks = Right oldText
    | otherwise         =
        let oldSeq :: Seq TextLine
            oldSeq = Seq.fromList (splitLinesWithNewline oldText)

            totalOld :: Int
            totalOld = Seq.length oldSeq
        in  do
                (cursor, newEnded, revChunks) <- applyHunks oldSeq totalOld 1 0 False [] parsed.hunks
                if newEnded && cursor < totalOld
                    then Left (HunkNewlineMismatch (length parsed.hunks) (cursor + 1))
                    else
                        let tailChunk :: [TextLine]
                            tailChunk = Foldable.toList (Seq.drop cursor oldSeq)

                            allLines :: [TextLine]
                            allLines = concat (reverse (tailChunk : revChunks))
                        in  Right (joinLinesWithNewline allLines)
  where
    applyHunks ::
           Seq TextLine
        -> Int
        -> Int
        -> Int
        -> Bool
        -> [[TextLine]]
        -> [DiffHunk]
        -> Either DiffError (Int, Bool, [[TextLine]])
    applyHunks _      _        _      cursor !newEnded acc []         = Right (cursor, newEnded, acc)
    applyHunks oldSeq totalOld hunkNo cursor !newEnded acc (h : rest) =
        let targetIdx :: Int
            targetIdx = hunkOldStartIndex h
        in  if targetIdx < cursor
                then Left (OverlappingHunks hunkNo)
                else if newEnded && targetIdx > cursor
                    then Left (HunkNewlineMismatch hunkNo (cursor + 1))
                    else if targetIdx > totalOld || targetIdx + h.oldCount > totalOld
                        then Left (HunkOutOfRange hunkNo (targetIdx + 1) totalOld)
                        else do
                            let unchanged :: [TextLine]
                                unchanged = Foldable.toList (Seq.take (targetIdx - cursor) (Seq.drop cursor oldSeq))
                            (nextCursor, nextNewEnded, outLines) <- applySingleHunk oldSeq hunkNo targetIdx newEnded h.lines []
                            applyHunks oldSeq totalOld (hunkNo + 1) nextCursor nextNewEnded (outLines : unchanged : acc) rest

    applySingleHunk ::
           Seq TextLine
        -> Int
        -> Int
        -> Bool
        -> [DiffLine]
        -> [TextLine]
        -> Either DiffError (Int, Bool, [TextLine])
    applySingleHunk _      _      idx !newEnded []          revOut = Right (idx, newEnded, reverse revOut)
    applySingleHunk oldSeq hunkNo idx !newEnded (dl : rest) revOut = case dl.kind of
        AddedLine
            | newEnded  -> Left (HunkNewlineMismatch hunkNo (idx + 1))
            | otherwise -> applySingleHunk oldSeq hunkNo idx (not dl.hasNewline) rest (TextLine dl.content dl.hasNewline : revOut)
        RemovedLine -> do
            _ <- verifyOldLine oldSeq hunkNo idx dl
            applySingleHunk oldSeq hunkNo (idx + 1) newEnded rest revOut
        ContextLine
            | newEnded  -> Left (HunkNewlineMismatch hunkNo (idx + 1))
            | otherwise -> do
                matched <- verifyOldLine oldSeq hunkNo idx dl
                applySingleHunk oldSeq hunkNo (idx + 1) (not dl.hasNewline) rest (matched : revOut)

    verifyOldLine :: Seq TextLine -> Int -> Int -> DiffLine -> Either DiffError TextLine
    verifyOldLine oldSeq hunkNo idx dl = case Seq.lookup idx oldSeq of
        Nothing ->
            Left (HunkOutOfRange hunkNo (idx + 1) (Seq.length oldSeq))
        Just actual
            | actual.content /= dl.content ->
                Left (HunkContextMismatch hunkNo (idx + 1) dl.content actual.content)
            | actual.hasNewline /= dl.hasNewline ->
                Left (HunkNewlineMismatch hunkNo (idx + 1))
            | otherwise ->
                Right actual

-- | unified diff テキストをパースして旧テキストへ適用する。
applyUnifiedDiff :: Text -> UnifiedDiff -> Either DiffError Text
applyUnifiedDiff oldText diff = do
    parsed <- parseUnifiedDiff diff
    applyParsedDiff oldText parsed

-- | 構造化 'ParsedDiff' の適用方向を反転する (旧 → 新 の差分を 新 → 旧 の差分へ変換)。
reverseParsedDiff :: ParsedDiff -> ParsedDiff
reverseParsedDiff parsed = ParsedDiff (map reverseHunk parsed.hunks)
  where
    reverseHunk :: DiffHunk -> DiffHunk
    reverseHunk h = DiffHunk
        { oldStart = h.newStart
        , oldCount = h.newCount
        , newStart = h.oldStart
        , newCount = h.oldCount
        , lines    = normalizeDiffLineBlocks (map flipLine h.lines)
        }

    flipLine :: DiffLine -> DiffLine
    flipLine dl = DiffLine
        { kind       = case dl.kind of
            ContextLine -> ContextLine
            RemovedLine -> AddedLine
            AddedLine   -> RemovedLine
        , content    = dl.content
        , hasNewline = dl.hasNewline
        }

    normalizeDiffLineBlocks :: [DiffLine] -> [DiffLine]
    normalizeDiffLineBlocks [] = []
    normalizeDiffLineBlocks (dl : rest)
        | dl.kind == ContextLine = dl : normalizeDiffLineBlocks rest
        | otherwise              =
            let (block, after) = span (\ x -> x.kind /= ContextLine) (dl : rest)
                removed :: [DiffLine]
                removed = filter (\ x -> x.kind == RemovedLine) block
                added :: [DiffLine]
                added = filter (\ x -> x.kind == AddedLine) block
            in  removed <> added <> normalizeDiffLineBlocks after

-- | unified diff テキストの適用方向を反転する (旧 → 新 の差分を 新 → 旧 の差分へ変換)。
reverseUnifiedDiff :: UnifiedDiff -> Either DiffError UnifiedDiff
reverseUnifiedDiff diff = renderUnifiedDiff . reverseParsedDiff <$> parseUnifiedDiff diff

-- | 新テキストへ unified diff を逆適用し、旧テキストを再構成する。
-- 最新本文から過去リビジョンへ遡る復元用途に利用できる。
applyUnifiedDiffReverse :: Text -> UnifiedDiff -> Either DiffError Text
applyUnifiedDiffReverse newText diff = do
    parsed <- parseUnifiedDiff diff
    applyParsedDiff newText (reverseParsedDiff parsed)

-- ---------------------------------------------------------------------------
-- 内部実装: 行分割・結合
-- ---------------------------------------------------------------------------

splitLinesWithNewline :: Text -> [TextLine]
splitLinesWithNewline txt
    | Txt.null txt = []
    | otherwise    = go txt
  where
    go :: Text -> [TextLine]
    go remaining
        | Txt.null remaining = []
        | otherwise          =
            let (before, after) = Txt.breakOn "\n" remaining
            in  if Txt.null after
                    then [TextLine before False]
                    else TextLine before True : go (Txt.drop 1 after)

joinLinesWithNewline :: [TextLine] -> Text
joinLinesWithNewline = Txt.concat . map renderTextLine
  where
    renderTextLine :: TextLine -> Text
    renderTextLine tl
        | tl.hasNewline = tl.content <> "\n"
        | otherwise     = tl.content

-- ---------------------------------------------------------------------------
-- 内部実装: 共通前後トリミング + Myers O(ND) diff
-- ---------------------------------------------------------------------------

computeEditScript :: [TextLine] -> [TextLine] -> [EditOp]
computeEditScript oldLines newLines =
    let oldSeq :: Seq TextLine
        oldSeq = Seq.fromList oldLines

        newSeq :: Seq TextLine
        newSeq = Seq.fromList newLines

        n :: Int
        n = Seq.length oldSeq

        m :: Int
        m = Seq.length newSeq

        prefixLen :: Int
        prefixLen = countCommonPrefix oldSeq newSeq 0 (min n m)

        maxSuffix :: Int
        maxSuffix = min (n - prefixLen) (m - prefixLen)

        suffixLen :: Int
        suffixLen = countCommonSuffix oldSeq newSeq n m 0 maxSuffix

        midOld :: Seq TextLine
        midOld = sliceSeq prefixLen (n - prefixLen - suffixLen) oldSeq

        midNew :: Seq TextLine
        midNew = sliceSeq prefixLen (m - prefixLen - suffixLen) newSeq

        prefixOps :: [EditOp]
        prefixOps = map EditEqual (Foldable.toList (Seq.take prefixLen oldSeq))

        midOps :: [EditOp]
        midOps = myersDiff midOld midNew

        suffixOps :: [EditOp]
        suffixOps = map EditEqual (Foldable.toList (Seq.drop (n - suffixLen) oldSeq))
    in  prefixOps <> midOps <> suffixOps

countCommonPrefix :: Seq TextLine -> Seq TextLine -> Int -> Int -> Int
countCommonPrefix oldSeq newSeq idx limit
    | idx >= limit = idx
    | otherwise    = case (Seq.lookup idx oldSeq, Seq.lookup idx newSeq) of
        (Just a, Just b) | a == b -> countCommonPrefix oldSeq newSeq (idx + 1) limit
        _                         -> idx

countCommonSuffix :: Seq TextLine -> Seq TextLine -> Int -> Int -> Int -> Int -> Int
countCommonSuffix oldSeq newSeq n m matched limit
    | matched >= limit = matched
    | otherwise        =
        let oi :: Int
            oi = n - 1 - matched

            ni :: Int
            ni = m - 1 - matched
        in  case (Seq.lookup oi oldSeq, Seq.lookup ni newSeq) of
                (Just a, Just b) | a == b -> countCommonSuffix oldSeq newSeq n m (matched + 1) limit
                _                         -> matched

sliceSeq :: Int -> Int -> Seq a -> Seq a
sliceSeq start len = Seq.take len . Seq.drop start

myersDiff :: Seq TextLine -> Seq TextLine -> [EditOp]
myersDiff oldSeq newSeq
    | n == 0 && m == 0 = []
    | n == 0           = map EditAdd (Foldable.toList newSeq)
    | m == 0           = map EditRemove (Foldable.toList oldSeq)
    | otherwise        =
        case search 0 (IntMap.singleton 1 0) [] of
            Just trace -> backtrack n m trace []
            Nothing    ->
                -- 最大編集距離を超えた場合は安全に全削除 + 全追加へフォールバックする
                map EditRemove (Foldable.toList oldSeq) <> map EditAdd (Foldable.toList newSeq)
  where
    n :: Int
    n = Seq.length oldSeq

    m :: Int
    m = Seq.length newSeq

    maxD :: Int
    maxD = min maxMyersEditDistance (n + m)

    lookupV :: Int -> IntMap Int -> Int
    lookupV = IntMap.findWithDefault 0

    extendSnake :: Int -> Int -> (Int, Int)
    extendSnake !x !y
        | x < n && y < m = case (Seq.lookup x oldSeq, Seq.lookup y newSeq) of
            (Just a, Just b) | a == b -> extendSnake (x + 1) (y + 1)
            _                         -> (x, y)
        | otherwise      = (x, y)

    search :: Int -> IntMap Int -> [(Int, IntMap Int)] -> Maybe [(Int, IntMap Int)]
    search !d vPrev trace
        | d > maxD  = Nothing
        | otherwise =
            let ks :: [Int]
                ks = [-d, -d + 2 .. d]

                stepK :: (IntMap Int, Bool) -> Int -> (IntMap Int, Bool)
                stepK (!vAcc, !found) k
                    | found     = (vAcc, True)
                    | otherwise =
                        let startX :: Int
                            startX
                                | k == -d || (k /= d && lookupV (k - 1) vPrev < lookupV (k + 1) vPrev) =
                                    lookupV (k + 1) vPrev
                                | otherwise =
                                    lookupV (k - 1) vPrev + 1

                            startY :: Int
                            startY = startX - k

                            (!endX, !endY) = extendSnake startX startY

                            !vNext = IntMap.insert k endX vAcc
                        in  (vNext, endX >= n && endY >= m)

                (!vCurr, !reached) = foldl' stepK (IntMap.empty, False) ks
                nextTrace :: [(Int, IntMap Int)]
                nextTrace = (d, vPrev) : trace
            in  if reached
                    then Just nextTrace
                    else search (d + 1) vCurr nextTrace

    backtrack :: Int -> Int -> [(Int, IntMap Int)] -> [EditOp] -> [EditOp]
    backtrack !x !y [] acc = prependDiagonal x y 0 0 acc
    backtrack !x !y ((d, vPrev) : rest) acc
        | d == 0    = prependDiagonal x y 0 0 acc
        | otherwise =
            let k :: Int
                k = x - y

                prevK :: Int
                prevK
                    | k == -d || (k /= d && lookupV (k - 1) vPrev < lookupV (k + 1) vPrev) = k + 1
                    | otherwise                                                            = k - 1

                prevX :: Int
                prevX = lookupV prevK vPrev

                prevY :: Int
                prevY = prevX - prevK

                -- まず (x, y) から編集直後の地点まで対角線 (Equal) を遡る
                midX :: Int
                midX
                    | prevK == k + 1 = prevX
                    | otherwise      = prevX + 1

                midY :: Int
                midY = midX - k

                accAfterSnake :: [EditOp]
                accAfterSnake = prependDiagonal x y midX midY acc

                accAfterEdit :: [EditOp]
                accAfterEdit
                    | midX > prevX = case Seq.lookup prevX oldSeq of
                        Just removed -> EditRemove removed : accAfterSnake
                        Nothing      -> accAfterSnake
                    | otherwise    = case Seq.lookup prevY newSeq of
                        Just added -> EditAdd added : accAfterSnake
                        Nothing    -> accAfterSnake
            in  backtrack prevX prevY rest accAfterEdit

    prependDiagonal :: Int -> Int -> Int -> Int -> [EditOp] -> [EditOp]
    prependDiagonal !x !y !targetX !targetY acc
        | x > targetX && y > targetY =
            case Seq.lookup (x - 1) oldSeq of
                Just eqLine -> prependDiagonal (x - 1) (y - 1) targetX targetY (EditEqual eqLine : acc)
                Nothing     -> acc
        | otherwise = acc

-- | 連続する非 Equal 操作ブロック内を「削除行すべて → 追加行すべて」の標準順へ整列する。
normalizeEditBlocks :: [EditOp] -> [EditOp]
normalizeEditBlocks []                      = []
normalizeEditBlocks (op@EditEqual{} : rest) = op : normalizeEditBlocks rest
normalizeEditBlocks ops                     =
    let (block, after) = break isEditEqual ops
        removes :: [EditOp]
        removes = filter isEditRemove block
        adds :: [EditOp]
        adds = filter isEditAdd block
    in  removes <> adds <> normalizeEditBlocks after
  where
    isEditEqual :: EditOp -> Bool
    isEditEqual EditEqual{} = True
    isEditEqual _           = False

    isEditRemove :: EditOp -> Bool
    isEditRemove EditRemove{} = True
    isEditRemove _            = False

    isEditAdd :: EditOp -> Bool
    isEditAdd EditAdd{} = True
    isEditAdd _         = False

-- ---------------------------------------------------------------------------
-- 内部実装: 編集スクリプトからの Hunk グループ化
-- ---------------------------------------------------------------------------

-- | 0 始まり座標付きの編集操作。
data IndexedEdit = IndexedEdit
    { oldIdx :: !Int
    , newIdx :: !Int
    , op     :: !EditOp
    }

buildHunks :: Int -> [EditOp] -> [DiffHunk]
buildHunks ctx ops =
    let indexed :: [IndexedEdit]
        indexed = indexEdits 0 0 ops
    in  groupFromLeading indexed
  where
    indexEdits :: Int -> Int -> [EditOp] -> [IndexedEdit]
    indexEdits _      _      []          = []
    indexEdits !oIdx  !nIdx  (op : rest) = case op of
        EditEqual{}  -> IndexedEdit oIdx nIdx op : indexEdits (oIdx + 1) (nIdx + 1) rest
        EditRemove{} -> IndexedEdit oIdx nIdx op : indexEdits (oIdx + 1) nIdx rest
        EditAdd{}    -> IndexedEdit oIdx nIdx op : indexEdits oIdx (nIdx + 1) rest

    isEqualIndexed :: IndexedEdit -> Bool
    isEqualIndexed ie = case ie.op of
        EditEqual{} -> True
        _           -> False

    groupFromLeading :: [IndexedEdit] -> [DiffHunk]
    groupFromLeading items =
        let (leadingEq, afterEq) = span isEqualIndexed items
        in  case afterEq of
                [] -> []
                _  ->
                    let leadCtx :: [IndexedEdit]
                        leadCtx = takeLast ctx leadingEq
                    in  collectHunk leadCtx afterEq

    collectHunk :: [IndexedEdit] -> [IndexedEdit] -> [DiffHunk]
    collectHunk prefixItems restItems =
        let (changes, afterChanges) = break isEqualIndexed restItems
            (midEq, afterMidEq) = span isEqualIndexed afterChanges
        in  case afterMidEq of
                [] ->
                    let trailCtx :: [IndexedEdit]
                        trailCtx = take ctx midEq
                    in  [finalizeHunk (prefixItems <> changes <> trailCtx)]
                _
                    | length midEq <= ctx * 2 ->
                        -- 次の変更までの間隔が 2 * ctx 以下なら同一 hunk として連結する
                        collectHunk (prefixItems <> changes <> midEq) afterMidEq
                    | otherwise ->
                        let trailCtx :: [IndexedEdit]
                            trailCtx = take ctx midEq

                            nextLeadCtx :: [IndexedEdit]
                            nextLeadCtx = takeLast ctx midEq

                            hunk :: DiffHunk
                            hunk = finalizeHunk (prefixItems <> changes <> trailCtx)
                        in  hunk : collectHunk nextLeadCtx afterMidEq

    finalizeHunk :: [IndexedEdit] -> DiffHunk
    finalizeHunk items =
        let diffLines :: [DiffLine]
            diffLines = map toDiffLine items

            oCount :: Int
            oCount = countOldLines diffLines

            nCount :: Int
            nCount = countNewLines diffLines

            (startOldIdx, startNewIdx) = case items of
                []        -> (0, 0)
                first : _ -> (first.oldIdx, first.newIdx)

            oStart :: Int
            oStart = if oCount == 0 then startOldIdx else startOldIdx + 1

            nStart :: Int
            nStart = if nCount == 0 then startNewIdx else startNewIdx + 1
        in  DiffHunk
                { oldStart = oStart
                , oldCount = oCount
                , newStart = nStart
                , newCount = nCount
                , lines    = diffLines
                }

    toDiffLine :: IndexedEdit -> DiffLine
    toDiffLine ie = case ie.op of
        EditEqual tl  -> DiffLine ContextLine tl.content tl.hasNewline
        EditRemove tl -> DiffLine RemovedLine tl.content tl.hasNewline
        EditAdd tl    -> DiffLine AddedLine tl.content tl.hasNewline

takeLast :: Int -> [a] -> [a]
takeLast k xs
    | k <= 0    = []
    | otherwise = drop (max 0 (length xs - k)) xs

-- ---------------------------------------------------------------------------
-- 内部実装: Hunk ヘッダ解析・共通ヘルパ
-- ---------------------------------------------------------------------------

parseHunkHeader :: Int -> Text -> Either DiffError (Int, Int, Int, Int)
parseHunkHeader lineNo raw =
    case Txt.stripPrefix "@@ -" raw of
        Nothing -> Left (MalformedHunkHeader lineNo raw)
        Just afterMinus ->
            let (oldRangeTxt, rest1) = Txt.breakOn " +" afterMinus
            in  case Txt.stripPrefix " +" rest1 of
                    Nothing -> Left (MalformedHunkHeader lineNo raw)
                    Just afterPlus ->
                        let (newRangeTxt, rest2) = Txt.breakOn " @@" afterPlus
                        in  if Txt.isPrefixOf " @@" rest2
                                then case (parseRange oldRangeTxt, parseRange newRangeTxt) of
                                    (Just (os, oc), Just (ns, nc))
                                        | oc == 0 && nc == 0 -> Left (MalformedHunkHeader lineNo raw)
                                        | otherwise          -> Right (os, oc, ns, nc)
                                    _ -> Left (MalformedHunkHeader lineNo raw)
                                else Left (MalformedHunkHeader lineNo raw)
  where
    parseRange :: Text -> Maybe (Int, Int)
    parseRange txt = case Txt.splitOn "," txt of
        [startTxt] -> do
            s <- parseNonNegativeInt startTxt
            if s >= 1 then Just (s, 1) else Nothing
        [startTxt, countTxt] -> do
            s <- parseNonNegativeInt startTxt
            c <- parseNonNegativeInt countTxt
            if c > 0 && s < 1
                then Nothing
                else Just (s, c)
        _ -> Nothing

-- | 32-bit Wasm 環境 (Int 最大値 2,147,483,647) でも @startIdx + count@ がオーバーフローしないよう
-- 最大 9 桁 (999,999,999) までの非負整数のみを受理する。
parseNonNegativeInt :: Text -> Maybe Int
parseNonNegativeInt txt
    | Txt.null txt || Txt.length txt > 9 || not (Txt.all Char.isDigit txt) = Nothing
    | otherwise = Just (Txt.foldl' (\ acc ch -> acc * 10 + Char.digitToInt ch) 0 txt)

hunkOldStartIndex :: DiffHunk -> Int
hunkOldStartIndex h
    | h.oldCount == 0 = h.oldStart
    | otherwise       = h.oldStart - 1

hunkNewStartIndex :: DiffHunk -> Int
hunkNewStartIndex h
    | h.newCount == 0 = h.newStart
    | otherwise       = h.newStart - 1

countOldLines :: [DiffLine] -> Int
countOldLines = foldl' step 0
  where
    step :: Int -> DiffLine -> Int
    step !acc dl = case dl.kind of
        ContextLine -> acc + 1
        RemovedLine -> acc + 1
        AddedLine   -> acc

countNewLines :: [DiffLine] -> Int
countNewLines = foldl' step 0
  where
    step :: Int -> DiffLine -> Int
    step !acc dl = case dl.kind of
        ContextLine -> acc + 1
        RemovedLine -> acc
        AddedLine   -> acc + 1

showInt :: Int -> Text
showInt = Txt.pack . show
