{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Markdown 文書向けの書式整形・目次 (TOC) 生成を行う同梱拡張。
-- フェンスドコードブロック (@```@ / @~~~@) 内のテキストは破壊せずそのまま保持し、
-- 改行コードは CRLF 混入時の不整合を防ぐため LF へ正規化する。
module TamaTsuzura.Extensions.MarkdownFormatter
    ( markdownFormatterExtension
    , formatMarkdownBody
    , insertOrUpdateToc
    ) where

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
    , ExtensionError(..)
    , ExtensionId(..)
    , ExtensionMetadata(..)
    , ExtensionName(..)
    , ExtensionResult(..)
    , defaultExtensionVersion
    )
import           TamaTsuzura.Extensions.Internal (mapLinesPreservingEnd, splitLinesPreservingEnd)

-- | Markdown の見出し・箇条書き整形および目次 (TOC) 生成を提供する同梱拡張。
markdownFormatterExtension :: Extension
markdownFormatterExtension = Extension
    { metadata = ExtensionMetadata
        { extensionId    = ExtensionId "markdown-formatter"
        , name           = ExtensionName "Markdown 整形"
        , description    = "見出し・箇条書きの書式統一と目次 (TOC) の自動生成を行います。"
        , version        = defaultExtensionVersion
        , category       = CategoryMarkdown
        , defaultEnabled = True
        }
    , commands =
        [ ExtensionCommand
            { commandId    = CommandId "format-markdown"
            , label        = CommandName "Markdown 書式整形"
            , description  = "見出しのスペース統一・箇条書き記号の '-' 統一・行末空白除去を行います（コードブロックは保護）。"
            , markdownOnly = True
            , runCommand   = \ ctx ->
                Right ExtensionResult
                    { nextBody = formatMarkdownBody ctx.body
                    , summary  = Just "Markdown の書式を整形しました。"
                    }
            }
        , ExtensionCommand
            { commandId    = CommandId "insert-toc"
            , label        = CommandName "目次 (TOC) を生成・更新"
            , description  = "文書内の見出し (#〜###) から目次ブロックを先頭へ挿入または更新します。"
            , markdownOnly = True
            , runCommand   = \ ctx ->
                case insertOrUpdateToc ctx.body of
                    Left errMsg -> Left (ExtensionCommandFailed errMsg)
                    Right next  -> Right ExtensionResult
                        { nextBody = next
                        , summary  = Just "目次 (TOC) を生成・更新しました。"
                        }
            }
        ]
    , transformBuffer = formatMarkdownBody
    }

-- | フェンスドコードブロックの開始記号 (バッククォートまたはチルダ) と長さ。
data FenceState = FenceState
    { fenceChar :: !Char
    , fenceLen  :: !Int
    } deriving (Eq, Show)

-- | Markdown 本文の書式を純粋に整形する (CRLF は LF へ正規化する)。
-- コードブロック外の行に対して見出しの空白正規化、箇条書きマーカー (@*@ / @+@ -> @-@、水平区切り線は保護) の統一、
-- 行末空白除去、および 2 行以上連続する空行の 1 行圧縮を行う。
formatMarkdownBody :: DocumentBody -> DocumentBody
formatMarkdownBody = mapLinesPreservingEnd (collapseOutsideFence Nothing False)
  where
    collapseOutsideFence :: Maybe FenceState -> Bool -> [Text] -> [Text]
    collapseOutsideFence _      _         []            = []
    collapseOutsideFence mFence prevBlank (line : rest) = case mFence of
        Just fence
            | isClosingFence fence line ->
                trimTrailing line : collapseOutsideFence Nothing False rest
            | otherwise ->
                line : collapseOutsideFence (Just fence) False rest
        Nothing -> case parseOpeningFence line of
            Just fence ->
                trimTrailing line : collapseOutsideFence (Just fence) False rest
            Nothing ->
                let normalized :: Text
                    normalized = formatMarkdownLine line
                in  if Txt.null normalized
                        then
                            if prevBlank
                                then collapseOutsideFence Nothing True rest
                                else "" : collapseOutsideFence Nothing True rest
                        else
                            normalized : collapseOutsideFence Nothing False rest

    formatMarkdownLine :: Text -> Text
    formatMarkdownLine line =
        let trimmedEnd :: Text
            trimmedEnd = trimTrailing line
        in  normalizeListMarker (normalizeHeading trimmedEnd)

    normalizeHeading :: Text -> Text
    normalizeHeading line =
        let (hashes, rest) = Txt.span (== '#') line
            level :: Int
            level = Txt.length hashes
        in  if level >= 1 && level <= 6 && not (Txt.null (Txt.strip rest))
                then hashes <> " " <> Txt.strip rest
                else line

    normalizeListMarker :: Text -> Text
    normalizeListMarker line
        | isThematicBreak line = line
        | otherwise =
            let (indent, rest) = Txt.span (== ' ') line
            in  case Txt.uncons rest of
                    Just (marker, afterMarker)
                        | (marker == '*' || marker == '+' || marker == '-')
                            && hasLeadingHorizontalSpace afterMarker ->
                            indent <> "- " <> Txt.dropWhile isHorizontalSpace afterMarker
                    _ -> line

    -- CommonMark 水平区切り線 (先頭空白 0〜3 個に続き、同一の '*', '-', '_' が 3 個以上と空白のみ) を保護する。
    isThematicBreak :: Text -> Bool
    isThematicBreak line =
        let (leading, rest) = Txt.span (== ' ') line
            stripped :: Text
            stripped = Txt.filter (not . isHorizontalSpace) rest
        in  Txt.length leading <= 3
                && Txt.length stripped >= 3
                && case Txt.uncons stripped of
                    Just (ch, _) ->
                        (ch == '*' || ch == '-' || ch == '_')
                            && Txt.all (== ch) stripped
                    Nothing -> False

    trimTrailing :: Text -> Text
    trimTrailing = Txt.dropWhileEnd isHorizontalSpace

-- | TOC ブロックの開始・終了マーカー。
tocStartMarker :: Text
tocStartMarker = "<!-- TOC -->"

tocEndMarker :: Text
tocEndMarker = "<!-- /TOC -->"

-- | 文書内の見出し (@#@ 〜 @###@、コードブロック内を除く) から TOC ブロックを生成し、
-- 既存の @<!-- TOC --> ... <!-- /TOC -->@ を置換または先頭へ挿入する (冪等、CRLF は LF へ正規化)。
-- 未閉鎖の @<!-- TOC -->@ やコードブロック内のマーカー例示は破壊せずそのまま保持する。
-- 見出しが 1 つも存在しない場合はエラーメッセージを返す。
insertOrUpdateToc :: DocumentBody -> Either Text DocumentBody
insertOrUpdateToc body =
    let (rawLines, _) = splitLinesPreservingEnd (toText body)

        cleanedLines :: [Text]
        cleanedLines = stripExistingToc Nothing rawLines

        headings :: [(Int, Text)]
        headings = extractHeadings Nothing cleanedLines
    in  if null headings
            then Left "見出し (#〜###) が見つからないため目次を生成できません。"
            else
                let tocLines :: [Text]
                    tocLines =
                        [tocStartMarker]
                            <> map formatTocEntry headings
                            <> [tocEndMarker]

                    mergedLines :: [Text]
                    mergedLines =
                        case dropWhile (Txt.null . Txt.strip) cleanedLines of
                            []   -> tocLines
                            rest -> tocLines <> [""] <> rest
                in  Right (DocumentBody (Txt.intercalate "\n" mergedLines <> "\n"))
  where
    stripExistingToc :: Maybe FenceState -> [Text] -> [Text]
    stripExistingToc _      []            = []
    stripExistingToc mFence (line : rest) = case mFence of
        Just fence ->
            let nextFence :: Maybe FenceState
                nextFence = if isClosingFence fence line then Nothing else Just fence
            in  line : stripExistingToc nextFence rest
        Nothing -> case parseOpeningFence line of
            Just fence ->
                line : stripExistingToc (Just fence) rest
            Nothing
                | Txt.strip line == tocStartMarker ->
                    case break (\ l -> Txt.strip l == tocEndMarker) rest of
                        (_, _ : afterEnd) ->
                            stripExistingToc Nothing (dropWhile (Txt.null . Txt.strip) afterEnd)
                        (_, []) ->
                            -- 閉じマーカーが無い未閉鎖 TOC マーカーでは後続本文を消さずそのまま残す
                            line : stripExistingToc Nothing rest
                | otherwise ->
                    line : stripExistingToc Nothing rest

    extractHeadings :: Maybe FenceState -> [Text] -> [(Int, Text)]
    extractHeadings _      []            = []
    extractHeadings mFence (line : rest) = case mFence of
        Just fence ->
            let nextFence :: Maybe FenceState
                nextFence = if isClosingFence fence line then Nothing else Just fence
            in  extractHeadings nextFence rest
        Nothing -> case parseOpeningFence line of
            Just fence -> extractHeadings (Just fence) rest
            Nothing ->
                let (hashes, afterHashes) = Txt.span (== '#') line
                    level :: Int
                    level = Txt.length hashes
                    title :: Text
                    title = Txt.strip afterHashes
                in  if level >= 1 && level <= 3 && hasLeadingHorizontalSpace afterHashes && not (Txt.null title)
                        then (level, title) : extractHeadings Nothing rest
                        else extractHeadings Nothing rest

    formatTocEntry :: (Int, Text) -> Text
    formatTocEntry (level, title) =
        let indent :: Text
            indent = Txt.replicate ((level - 1) * 2) " "
        in  indent <> "- " <> title

-- | CommonMark 準拠のフェンス開始判定 (先頭空白 0〜3 個、長さ 3 以上、バックティックフェンスの info に @`@ を含まない)。
parseOpeningFence :: Text -> Maybe FenceState
parseOpeningFence line =
    let (leadingSpaces, stripped) = Txt.span (== ' ') line
    in  if Txt.length leadingSpaces > 3
            then Nothing
            else case Txt.uncons stripped of
                Just (ch, _) | ch == '`' || ch == '~' ->
                    let (ticks, info) = Txt.span (== ch) stripped
                        len :: Int
                        len = Txt.length ticks
                    in  if len >= 3 && not (ch == '`' && Txt.any (== '`') info)
                            then Just FenceState { fenceChar = ch, fenceLen = len }
                            else Nothing
                _ -> Nothing

-- | CommonMark 準拠のフェンス終了判定 (先頭空白 0〜3 個、開始長以上の同種記号、後続は水平空白のみ)。
isClosingFence :: FenceState -> Text -> Bool
isClosingFence fence line =
    let (leadingSpaces, stripped) = Txt.span (== ' ') line
        (ticks, rest) = Txt.span (== fence.fenceChar) stripped
    in  Txt.length leadingSpaces <= 3
            && Txt.length ticks >= fence.fenceLen
            && Txt.all isHorizontalSpace rest

isHorizontalSpace :: Char -> Bool
isHorizontalSpace ch = ch == ' ' || ch == '\t'

hasLeadingHorizontalSpace :: Text -> Bool
hasLeadingHorizontalSpace txt = case Txt.uncons txt of
    Just (ch, _) -> isHorizontalSpace ch
    Nothing      -> False
