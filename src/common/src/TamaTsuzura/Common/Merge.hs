{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module TamaTsuzura.Common.Merge
    ( -- * 3-way マージ結果・競合表現型
      ConflictBlock(..)
    , MergeRegion(..)
    , MergeOutcome(..)
    , ConflictChoice(..)
    , ConflictMarkerLabels(..)
    , defaultConflictMarkerLabels
      -- * 純粋な 3-way マージ・レンダリング・手動解決関数
    , splitLinesPreserveNewlines
    , mergeThreeWay
    , renderMergedText
    , resolveMergeOutcome
    , resolveConflictBlock
    ) where

import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Sequence           (Seq)
import qualified Data.Sequence           as Seq
import           Data.Text               (Text)
import qualified Data.Text               as Txt
import           TamaTsuzura.Common.Diff
    ( DiffHunk(..)
    , DiffLine(..)
    , DiffLineKind(..)
    , ParsedDiff(..)
    , generateDiffWithContext
    )

-- | 3-way マージで自動統合できなかった 1 箇所分の競合ブロック。
-- 各リストの要素は元の改行 (@\n@) を末尾に保持した行トークンであり、
-- 'Txt.concat' で元テキスト断片と完全一致する。
data ConflictBlock = ConflictBlock
    { conflictIndex :: !Int
      -- ^ 0 始まりの競合識別インデックス
    , baseLines     :: ![Text]
      -- ^ 共通祖先 (@base@) 側の該当行群
    , oursLines     :: ![Text]
      -- ^ ローカル (@ours@) 側の該当行群
    , theirsLines   :: ![Text]
      -- ^ サーバ (@theirs@) 側の該当行群
    } deriving (Eq, Show)

-- | 3-way マージ出力を構成する連続領域。
data MergeRegion
    = MergeClean ![Text]
      -- ^ 競合なく確定した行トークン列
    | MergeConflict !ConflictBlock
      -- ^ 両側で異なる変更が衝突した競合ブロック
    deriving (Eq, Show)

-- | 3-way マージ全体の構造化結果。
data MergeOutcome = MergeOutcome
    { regions       :: ![MergeRegion]
      -- ^ 文書先頭から末尾へ並んだマージ領域列 (隣接する 'MergeClean' は統合済み)
    , conflictCount :: !Int
      -- ^ 検出された競合ブロックの総数 (0 以上)
    , hasConflicts  :: !Bool
      -- ^ 1 件以上の競合が存在するかどうか (@conflictCount > 0@ と同値)
    } deriving (Eq, Show)

-- | 競合ブロック 1 件に対するユーザーの手動解決方針。
data ConflictChoice
    = ChooseOurs
      -- ^ ローカル (@ours@) の変更を採用する
    | ChooseTheirs
      -- ^ サーバ (@theirs@) の変更を採用する
    | ChooseBothOursFirst
      -- ^ ローカルの変更に続けてサーバの変更を両方残す
    | ChooseBothTheirsFirst
      -- ^ サーバの変更に続けてローカルの変更を両方残す
    | ChooseBase
      -- ^ 共通祖先 (@base@) の内容へ戻す
    | ChooseCustom !Text
      -- ^ ユーザーが手動編集した任意テキストを採用する
    deriving (Eq, Show)

-- | コンフリクトマーカー付きテキスト生成時のラベル設定。
data ConflictMarkerLabels = ConflictMarkerLabels
    { oursLabel   :: !Text
      -- ^ @<<<<<<<@ 行へ付与するローカル側ラベル
    , baseLabel   :: !Text
      -- ^ 必要時に共通祖先を表すラベル
    , theirsLabel :: !Text
      -- ^ @>>>>>>>@ 行へ付与するサーバ側ラベル
    } deriving (Eq, Show)

-- | 標準のコンフリクトマーカーラベル (@LOCAL@ / @BASE@ / @SERVER@)。
defaultConflictMarkerLabels :: ConflictMarkerLabels
defaultConflictMarkerLabels = ConflictMarkerLabels
    { oursLabel   = "LOCAL"
    , baseLabel   = "BASE"
    , theirsLabel = "SERVER"
    }

-- | テキストを末尾の @\n@ を保持した行トークン列へ分解する全単射関数。
-- 任意の 'Text' に対して @Txt.concat (splitLinesPreserveNewlines t) == t@ を満たす。
splitLinesPreserveNewlines :: Text -> [Text]
splitLinesPreserveNewlines raw
    | Txt.null raw = []
    | otherwise    = go raw
  where
    go :: Text -> [Text]
    go remaining
        | Txt.null remaining = []
        | otherwise =
            let (lineBody, rest) = Txt.breakOn "\n" remaining
            in  case Txt.uncons rest of
                    Nothing           -> [lineBody]
                    Just ('\n', rem') -> (lineBody <> "\n") : go rem'
                    Just _            -> [remaining]

-- | 共通祖先 @base@、ローカル @ours@、サーバ @theirs@ に対して行単位の純粋な 3-way マージ (diff3) を実行する。
-- 片側のみの変更および両側で同一内容へ収束した変更は自動でクリーン統合し、
-- 同一基準範囲に対する異なる変更のみを 'MergeConflict' として抽出する。
mergeThreeWay :: Text -> Text -> Text -> MergeOutcome
mergeThreeWay baseText oursText theirsText =
    let baseSeq :: Seq Text
        baseSeq = Seq.fromList (splitLinesPreserveNewlines baseText)

        oursSpans :: [EditSpan]
        oursSpans = extractEditSpans baseText oursText

        theirsSpans :: [EditSpan]
        theirsSpans = extractEditSpans baseText theirsText

        rawRegions :: [MergeRegion]
        rawRegions = mergeSpanStreams baseSeq 0 0 oursSpans theirsSpans

        normalized :: [MergeRegion]
        normalized = coalesceCleanRegions rawRegions

        conflicts :: Int
        conflicts = length [ () | MergeConflict{} <- normalized ]
    in  MergeOutcome
            { regions       = normalized
            , conflictCount = conflicts
            , hasConflicts  = conflicts > 0
            }

-- | 'MergeOutcome' を単一の 'Text' へレンダリングする。
-- 競合がない場合はそのままクリーン統合テキストを返し、競合がある箇所には
-- @<<<<<<<@ / @=======@ / @>>>>>>>@ のコンフリクトマーカーを挿入する。
renderMergedText :: ConflictMarkerLabels -> MergeOutcome -> Text
renderMergedText labels outcome =
    Txt.concat (map renderRegion outcome.regions)
  where
    renderRegion :: MergeRegion -> Text
    renderRegion (MergeClean ls)    = Txt.concat ls
    renderRegion (MergeConflict cb) =
        let oursTxt :: Text
            oursTxt = ensureTrailingNewlineIfNonEmpty (Txt.concat cb.oursLines)

            theirsTxt :: Text
            theirsTxt = ensureTrailingNewlineIfNonEmpty (Txt.concat cb.theirsLines)
        in  "<<<<<<< " <> labels.oursLabel <> "\n"
                <> oursTxt
                <> "=======\n"
                <> theirsTxt
                <> ">>>>>>> " <> labels.theirsLabel <> "\n"

    ensureTrailingNewlineIfNonEmpty :: Text -> Text
    ensureTrailingNewlineIfNonEmpty t
        | Txt.null t               = ""
        | Txt.isSuffixOf "\n" t    = t
        | otherwise                = t <> "\n"

-- | 単一の 'ConflictBlock' に選択済み 'ConflictChoice' を適用してテキスト断片を得る。
resolveConflictBlock :: ConflictChoice -> ConflictBlock -> Text
resolveConflictBlock choice cb = case choice of
    ChooseOurs            -> Txt.concat cb.oursLines
    ChooseTheirs          -> Txt.concat cb.theirsLines
    ChooseBothOursFirst   -> joinSides cb.oursLines cb.theirsLines
    ChooseBothTheirsFirst -> joinSides cb.theirsLines cb.oursLines
    ChooseBase            -> Txt.concat cb.baseLines
    ChooseCustom custom   -> custom
  where
    joinSides :: [Text] -> [Text] -> Text
    joinSides firstLines secondLines =
        let firstTxt :: Text
            firstTxt = Txt.concat firstLines

            secondTxt :: Text
            secondTxt = Txt.concat secondLines
        in  if Txt.null firstTxt || Txt.null secondTxt || Txt.isSuffixOf "\n" firstTxt
                then firstTxt <> secondTxt
                else firstTxt <> "\n" <> secondTxt

-- | 競合ブロックごとの選択マップを用いて 'MergeOutcome' 全体を最終テキストへ解決する。
-- 1 件でも未選択の競合ブロックが残っている場合は、最初の未解決競合インデックスを 'Left' で返す。
resolveMergeOutcome :: Map Int ConflictChoice -> MergeOutcome -> Either Int Text
resolveMergeOutcome choices outcome =
    Txt.concat <$> traverse resolveRegion outcome.regions
  where
    resolveRegion :: MergeRegion -> Either Int Text
    resolveRegion (MergeClean ls)    = Right (Txt.concat ls)
    resolveRegion (MergeConflict cb) = case Map.lookup cb.conflictIndex choices of
        Nothing     -> Left cb.conflictIndex
        Just choice -> Right (resolveConflictBlock choice cb)

-- | @base@ 上の 0 始まり半開区間 @[baseStart, baseEnd)@ に対する置換スパン。
data EditSpan = EditSpan
    { baseStart   :: !Int
    , baseEnd     :: !Int
    , replacement :: ![Text]
    } deriving (Eq, Show)

extractEditSpans :: Text -> Text -> [EditSpan]
extractEditSpans baseText sideText =
    let parsed :: ParsedDiff
        parsed = generateDiffWithContext 0 baseText sideText
    in  map hunkToSpan parsed.hunks
  where
    hunkToSpan :: DiffHunk -> EditSpan
    hunkToSpan hunk =
        let start0 :: Int
            start0 =
                if hunk.oldCount == 0
                    then hunk.oldStart
                    else hunk.oldStart - 1

            end0 :: Int
            end0 = start0 + hunk.oldCount

            added :: [Text]
            added =
                [ diffLineToToken dl
                | dl <- hunk.lines
                , dl.kind == AddedLine
                ]
        in  EditSpan
                { baseStart   = start0
                , baseEnd     = end0
                , replacement = added
                }

    diffLineToToken :: DiffLine -> Text
    diffLineToToken dl
        | dl.hasNewline = dl.content <> "\n"
        | otherwise     = dl.content

mergeSpanStreams :: Seq Text -> Int -> Int -> [EditSpan] -> [EditSpan] -> [MergeRegion]
mergeSpanStreams baseSeq !pos !_ [] [] =
    emitCleanSlice baseSeq pos (Seq.length baseSeq) []
mergeSpanStreams baseSeq !pos !nextIdx os ts =
    let (clusterStart, initialEnd) = case (os, ts) of
            (o : _, t : _)
                | o.baseStart < t.baseStart -> (o.baseStart, o.baseEnd)
                | t.baseStart < o.baseStart -> (t.baseStart, t.baseEnd)
                | otherwise                 -> (o.baseStart, max o.baseEnd t.baseEnd)
            (o : _, [])    -> (o.baseStart, o.baseEnd)
            ([],    t : _) -> (t.baseStart, t.baseEnd)

        (clusterEnd, oCluster, osRest, tCluster, tsRest) =
            collectCluster clusterStart initialEnd os ts

        baseSlice :: [Text]
        baseSlice = sliceSeq baseSeq clusterStart clusterEnd

        oursResult :: [Text]
        oursResult = applyClusterSpans baseSeq clusterStart clusterEnd oCluster

        theirsResult :: [Text]
        theirsResult = applyClusterSpans baseSeq clusterStart clusterEnd tCluster

        (region, nextConflictIdx) = case (oCluster, tCluster) of
            ([], _) -> (MergeClean theirsResult, nextIdx)
            (_, []) -> (MergeClean oursResult,   nextIdx)
            _
                | oursResult == theirsResult -> (MergeClean oursResult, nextIdx)
                | otherwise ->
                    ( MergeConflict ConflictBlock
                        { conflictIndex = nextIdx
                        , baseLines     = baseSlice
                        , oursLines     = oursResult
                        , theirsLines   = theirsResult
                        }
                    , nextIdx + 1
                    )
    in  emitCleanSlice baseSeq pos clusterStart
            (region : mergeSpanStreams baseSeq clusterEnd nextConflictIdx osRest tsRest)

collectCluster ::
       Int
    -> Int
    -> [EditSpan]
    -> [EditSpan]
    -> (Int, [EditSpan], [EditSpan], [EditSpan], [EditSpan])
collectCluster cStart cEnd = go cEnd [] []
  where
    go ::
           Int
        -> [EditSpan]
        -> [EditSpan]
        -> [EditSpan]
        -> [EditSpan]
        -> (Int, [EditSpan], [EditSpan], [EditSpan], [EditSpan])
    go !curEnd accO accT os ts =
        let (takenO, restO, endAfterO) = takeOverlapping cStart curEnd os
            (takenT, restT, endAfterT) = takeOverlapping cStart endAfterO ts
        in  if null takenO && null takenT
                then (curEnd, reverse accO, os, reverse accT, ts)
                else go (max endAfterO endAfterT) (reverse takenO <> accO) (reverse takenT <> accT) restO restT

    takeOverlapping :: Int -> Int -> [EditSpan] -> ([EditSpan], [EditSpan], Int)
    takeOverlapping startBound = loop []
      where
        loop :: [EditSpan] -> Int -> [EditSpan] -> ([EditSpan], [EditSpan], Int)
        loop acc curEnd [] = (reverse acc, [], curEnd)
        loop acc curEnd (sp : rest)
            | overlaps startBound curEnd sp = loop (sp : acc) (max curEnd sp.baseEnd) rest
            | otherwise                     = (reverse acc, sp : rest, curEnd)

    overlaps :: Int -> Int -> EditSpan -> Bool
    overlaps startBound endBound sp
        | startBound == endBound     = sp.baseStart == startBound
        | sp.baseStart == sp.baseEnd = sp.baseStart >= startBound && sp.baseStart <= endBound
        | otherwise                  = sp.baseStart < endBound

applyClusterSpans :: Seq Text -> Int -> Int -> [EditSpan] -> [Text]
applyClusterSpans baseSeq startPos endPos = go startPos
  where
    go :: Int -> [EditSpan] -> [Text]
    go !cur [] = sliceSeq baseSeq cur endPos
    go !cur (sp : rest) =
        sliceSeq baseSeq cur sp.baseStart
            <> sp.replacement
            <> go sp.baseEnd rest

sliceSeq :: Seq Text -> Int -> Int -> [Text]
sliceSeq seqData fromIdx toIdx
    | toIdx <= fromIdx = []
    | otherwise        = foldr (:) [] (Seq.take (toIdx - fromIdx) (Seq.drop fromIdx seqData))

emitCleanSlice :: Seq Text -> Int -> Int -> [MergeRegion] -> [MergeRegion]
emitCleanSlice baseSeq fromIdx toIdx rest =
    case sliceSeq baseSeq fromIdx toIdx of
        [] -> rest
        ls -> MergeClean ls : rest

coalesceCleanRegions :: [MergeRegion] -> [MergeRegion]
coalesceCleanRegions []                  = []
coalesceCleanRegions (MergeClean [] : r) = coalesceCleanRegions r
coalesceCleanRegions (MergeClean a  : r) = case coalesceCleanRegions r of
    MergeClean b : rest -> MergeClean (a <> b) : rest
    rest                -> MergeClean a : rest
coalesceCleanRegions (reg           : r) = reg : coalesceCleanRegions r
