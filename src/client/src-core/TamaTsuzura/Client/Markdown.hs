{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}

module TamaTsuzura.Client.Markdown
    ( -- * 安全な URL 型
      SafeUrl
    , mkSafeUrl
      -- * Markdown AST 型
    , TableAlign(..)
    , ListItem(..)
    , MarkdownInline(..)
    , MarkdownBlock(..)
      -- * 純粋パーサ・Miso レンダラ
    , parseMarkdown
    , parseInlines
    , renderMarkdownBlocks
    , renderMarkdownView
    ) where

import qualified Data.Char               as Char
import           Data.Maybe              (fromMaybe, isJust)
import           Data.Text               (Text)
import qualified Data.Text               as Txt
import           Miso                    (Attribute, View, text)
import qualified Miso.Html.Element       as H
import qualified Miso.Html.Property      as HP
import qualified Miso.Property           as MP
import           Miso.String             (ms)
import           TamaTsuzura.Common.Text (ToText(..))

-- | XSS 対策として許可済みスキームまたは安全な相対パスのみを保持する検証済み URL 型。
-- コンストラクタを非公開とすることで、'javascript:' や 'data:' 等の危険な URI が
-- プレビュー DOM の @href@ / @src@ 属性へ到達することをコンパイル時に防止する。
newtype SafeUrl = SafeUrl Text
    deriving (Eq, Ord, ToText)

instance Show SafeUrl where
    show = Txt.unpack . toText

-- | 生 URL 文字列を検証し、安全なスキーム (@http:@ / @https:@ / @mailto:@) または
-- 相対パス・フラグメントのみを 'SafeUrl' として受け入れる。
-- WHATWG URL 仕様でプロトコル相対 URL へ正規化されうる @//@ やバックスラッシュを含む URI も拒否する。
mkSafeUrl :: Text -> Maybe SafeUrl
mkSafeUrl raw
    | Txt.null trimmed                 = Nothing
    | Txt.any isUnsafeUrlChar trimmed  = Nothing
    | Txt.isPrefixOf "//" trimmed      = Nothing
    | otherwise                        = case extractScheme trimmed of
        Nothing     -> Just (SafeUrl trimmed)
        Just scheme
            | isAllowedScheme scheme -> Just (SafeUrl trimmed)
            | otherwise              -> Nothing
  where
    trimmed :: Text
    trimmed = Txt.strip raw

    -- 制御文字・内部空白に加え、WHATWG URL でスラッシュ扱いとなりプロトコル相対遷移を起こす
    -- バックスラッシュ ('\\') も一律拒否する
    isUnsafeUrlChar :: Char -> Bool
    isUnsafeUrlChar ch = Char.isControl ch || Char.isSpace ch || ch == '\\'

    extractScheme :: Text -> Maybe Text
    extractScheme txt =
        let (prefix, rest) = Txt.breakOn ":" txt
        in  if Txt.null rest || Txt.any isPathDelimiter prefix
                then Nothing
                else Just (Txt.toLower prefix)

    isPathDelimiter :: Char -> Bool
    isPathDelimiter ch = ch == '/' || ch == '?' || ch == '#'

    isAllowedScheme :: Text -> Bool
    isAllowedScheme "http"   = True
    isAllowedScheme "https"  = True
    isAllowedScheme "mailto" = True
    isAllowedScheme _        = False

-- | GFM テーブルの列揃え指定。
data TableAlign
    = AlignDefault
      -- ^ 指定なし
    | AlignLeft
      -- ^ 左揃え (@:---@)
    | AlignCenter
      -- ^ 中央揃え (@:---:@)
    | AlignRight
      -- ^ 右揃え (@---:@)
    deriving (Eq, Show)

-- | リストの 1 項目 (タスクリストのチェック状態を含む)。
data ListItem = ListItem
    { taskChecked :: !(Maybe Bool)
      -- ^ タスクリスト項目なら 'Just' (チェック済みなら 'True')、通常項目は 'Nothing'
    , content     :: ![MarkdownInline]
      -- ^ 項目のインライン内容
    } deriving (Eq, Show)

-- | Markdown インライン要素の AST。
data MarkdownInline
    = InlineText !Text
      -- ^ プレーンテキスト
    | InlineCode !Text
      -- ^ インラインコード
    | InlineEmph ![MarkdownInline]
      -- ^ 斜体強調
    | InlineStrong ![MarkdownInline]
      -- ^ 太字強調
    | InlineStrike ![MarkdownInline]
      -- ^ 打ち消し線
    | InlineLink !SafeUrl ![MarkdownInline]
      -- ^ 安全なハイパーリンク
    | InlineImage !SafeUrl !Text
      -- ^ 安全な画像 (URL, 代替テキスト)
    | InlineSoftBreak
      -- ^ 段落内の通常改行
    | InlineHardBreak
      -- ^ 末尾 2 スペースによる強制改行
    deriving (Eq, Show)

-- | Markdown ブロック要素の AST。
data MarkdownBlock
    = MdHeading !Int ![MarkdownInline]
      -- ^ 見出し (レベル 1〜6, インライン内容)
    | MdParagraph ![MarkdownInline]
      -- ^ 段落
    | MdCodeBlock !(Maybe Text) !Text
      -- ^ フェンスコードブロック (言語名ヒント, コード本体)
    | MdBlockQuote ![MarkdownBlock]
      -- ^ 引用ブロック (ネスト対応)
    | MdUnorderedList ![ListItem]
      -- ^ 箇条書きリスト
    | MdOrderedList !Int ![ListItem]
      -- ^ 番号付きリスト (開始番号, 項目一覧)
    | MdTable ![TableAlign] ![[MarkdownInline]] ![[[MarkdownInline]]]
      -- ^ GFM テーブル (列揃え, ヘッダセル一覧, ボディ行一覧)
    | MdThematicBreak
      -- ^ 水平区切り線
    deriving (Eq, Show)

-- | Markdown テキストをブロック AST リストへ純粋にパースする。
-- 日常執筆で利用頻度の高い CommonMark / GFM 実用サブセットを網羅し、
-- 任意の入力に対して部分関数や例外を用いず全域で安全に結果を返す。
parseMarkdown :: Text -> [MarkdownBlock]
parseMarkdown input = parseBlocks (splitNormalizedLines input)

-- | 改行コード (CRLF / CR) を LF に正規化して行リストへ分割する。
splitNormalizedLines :: Text -> [Text]
splitNormalizedLines raw =
    let normalized :: Text
        normalized = Txt.replace "\r" "\n" (Txt.replace "\r\n" "\n" raw)
    in  if Txt.null normalized
            then []
            else Txt.splitOn "\n" normalized

parseBlocks :: [Text] -> [MarkdownBlock]
parseBlocks [] = []
parseBlocks (line : rest)
    | Txt.null (Txt.strip line) =
        parseBlocks rest
    | Just (fenceChar, fenceLen, mLang) <- parseFenceOpen line =
        let (codeLines, afterFence) = collectFencedCode fenceChar fenceLen rest
        in  MdCodeBlock mLang (Txt.intercalate "\n" codeLines) : parseBlocks afterFence
    | isThematicBreak line =
        MdThematicBreak : parseBlocks rest
    | Just (level, headingBody) <- parseAtxHeading line =
        MdHeading level (parseInlines headingBody) : parseBlocks rest
    | isBlockQuoteLine line =
        let (quoteLines, afterQuote) = span isBlockQuoteLine (line : rest)
            stripped :: [Text]
            stripped = map stripQuotePrefix quoteLines
        in  MdBlockQuote (parseBlocks stripped) : parseBlocks afterQuote
    | Just (headers, aligns, afterHeader) <- parseTableStart line rest =
        let (rowLines, afterTable) = span isTableRowLine afterHeader
            colCount :: Int
            colCount = length headers

            parsedRows :: [[[MarkdownInline]]]
            parsedRows = map (parseTableCells colCount) rowLines
        in  MdTable aligns headers parsedRows : parseBlocks afterTable
    | Just firstItem <- parseUnorderedListItem line =
        let (moreItems, afterList) = collectUnorderedItems rest
        in  MdUnorderedList (firstItem : moreItems) : parseBlocks afterList
    | Just (startNum, firstItem) <- parseOrderedListItem line =
        let (moreItems, afterList) = collectOrderedItems rest
        in  MdOrderedList startNum (firstItem : moreItems) : parseBlocks afterList
    | otherwise =
        let (paraLines, afterPara) = collectParagraphLines (line : rest)
        in  MdParagraph (parseParagraphInlines paraLines) : parseBlocks afterPara

parseFenceOpen :: Text -> Maybe (Char, Int, Maybe Text)
parseFenceOpen line =
    let trimmedLeft :: Text
        trimmedLeft = stripUpTo3Spaces line
    in  case Txt.uncons trimmedLeft of
            Just (ch, _)
                | ch == '`' || ch == '~' ->
                    let (fence, afterFence) = Txt.span (== ch) trimmedLeft
                        fenceLen :: Int
                        fenceLen = Txt.length fence

                        info :: Text
                        info = Txt.strip afterFence

                        invalidBacktickInfo :: Bool
                        invalidBacktickInfo = ch == '`' && Txt.any (== '`') info
                    in  if fenceLen >= 3 && not invalidBacktickInfo
                            then Just (ch, fenceLen, if Txt.null info then Nothing else Just (firstWord info))
                            else Nothing
            _ -> Nothing
  where
    firstWord :: Text -> Text
    firstWord = Txt.takeWhile (not . Char.isSpace)

collectFencedCode :: Char -> Int -> [Text] -> ([Text], [Text])
collectFencedCode fenceChar fenceLen = go []
  where
    go :: [Text] -> [Text] -> ([Text], [Text])
    go acc [] = (reverse acc, [])
    go acc (l : ls)
        | isFenceClose fenceChar fenceLen l = (reverse acc, ls)
        | otherwise                         = go (l : acc) ls

isFenceClose :: Char -> Int -> Text -> Bool
isFenceClose fenceChar minLen line =
    let trimmedLeft :: Text
        trimmedLeft = stripUpTo3Spaces line

        (fence, rest) = Txt.span (== fenceChar) trimmedLeft
    in  Txt.length fence >= minLen && Txt.null (Txt.strip rest)

isThematicBreak :: Text -> Bool
isThematicBreak line =
    let compact :: Text
        compact = Txt.filter (\ ch -> ch /= ' ' && ch /= '\t') (stripUpTo3Spaces line)
    in  case Txt.uncons compact of
            Just (ch, _)
                | ch == '-' || ch == '*' || ch == '_' ->
                    Txt.length compact >= 3 && Txt.all (== ch) compact
            _ -> False

parseAtxHeading :: Text -> Maybe (Int, Text)
parseAtxHeading line =
    let trimmedLeft :: Text
        trimmedLeft = stripUpTo3Spaces line

        (hashes, afterHashes) = Txt.span (== '#') trimmedLeft

        level :: Int
        level = Txt.length hashes
    in  if level >= 1 && level <= 6
            then case Txt.uncons afterHashes of
                Nothing -> Just (level, "")
                Just (ch, _)
                    | ch == ' ' || ch == '\t' ->
                        Just (level, stripClosingHashes (Txt.strip afterHashes))
                    | otherwise ->
                        Nothing
            else Nothing
  where
    stripClosingHashes :: Text -> Text
    stripClosingHashes txt =
        let withoutTrailingHashes :: Text
            withoutTrailingHashes = Txt.dropWhileEnd (== '#') txt
        in  if Txt.null withoutTrailingHashes
                then ""
                else if Txt.length withoutTrailingHashes < Txt.length txt
                    && maybe False (Char.isSpace . snd) (Txt.unsnoc withoutTrailingHashes)
                    then Txt.stripEnd withoutTrailingHashes
                    else txt

isBlockQuoteLine :: Text -> Bool
isBlockQuoteLine line =
    Txt.isPrefixOf ">" (stripUpTo3Spaces line)

stripQuotePrefix :: Text -> Text
stripQuotePrefix line =
    let afterAngle :: Text
        afterAngle = Txt.drop 1 (stripUpTo3Spaces line)
    in  case Txt.uncons afterAngle of
            Just (' ', rest) -> rest
            _                -> afterAngle

parseTableStart :: Text -> [Text] -> Maybe ([[MarkdownInline]], [TableAlign], [Text])
parseTableStart _ [] = Nothing
parseTableStart headerLine (sepLine : rest)
    | not (Txt.any (== '|') headerLine) = Nothing
    | otherwise = do
        aligns <- parseTableSeparator sepLine
        let headerCellsRaw :: [Text]
            headerCellsRaw = splitTableRow headerLine
        if length headerCellsRaw == length aligns && not (null aligns)
            then Just (map parseInlines headerCellsRaw, aligns, rest)
            else Nothing

parseTableSeparator :: Text -> Maybe [TableAlign]
parseTableSeparator line
    | not (Txt.any (== '|') line) = Nothing
    | otherwise =
        let cells :: [Text]
            cells = splitTableRow line
        in  if null cells
                then Nothing
                else traverse parseAlignCell cells
  where
    parseAlignCell :: Text -> Maybe TableAlign
    parseAlignCell rawCell =
        let cell :: Text
            cell = Txt.strip rawCell

            leftColon :: Bool
            leftColon = Txt.isPrefixOf ":" cell

            rightColon :: Bool
            rightColon = Txt.isSuffixOf ":" cell

            inner :: Text
            inner =
                let afterLeft :: Text
                    afterLeft = if leftColon then Txt.drop 1 cell else cell
                in  if rightColon then Txt.dropEnd 1 afterLeft else afterLeft
        in  if Txt.length inner >= 3 && Txt.all (== '-') inner
                then Just $ case (leftColon, rightColon) of
                    (True,  True)  -> AlignCenter
                    (True,  False) -> AlignLeft
                    (False, True)  -> AlignRight
                    (False, False) -> AlignDefault
                else Nothing

isTableRowLine :: Text -> Bool
isTableRowLine line =
    let trimmed :: Text
        trimmed = Txt.strip line
    in  not (Txt.null trimmed)
            && Txt.any (== '|') trimmed
            && not (isThematicBreak line)
            && not (isBlockQuoteLine line)

splitTableRow :: Text -> [Text]
splitTableRow line =
    let trimmed :: Text
        trimmed = Txt.strip line

        withoutLeading :: Text
        withoutLeading = fromMaybe trimmed (Txt.stripPrefix "|" trimmed)

        withoutTrailing :: Text
        withoutTrailing = fromMaybe withoutLeading (Txt.stripSuffix "|" withoutLeading)
    in  map Txt.strip (splitUnescapedPipe withoutTrailing)

-- | O(n) で未エスケープの @|@ によりテーブル行を分割する (チャンク蓄積 + 'Txt.concat')。
splitUnescapedPipe :: Text -> [Text]
splitUnescapedPipe = go []
  where
    go :: [Text] -> Text -> [Text]
    go chunks remaining =
        let (plain, rest) = Txt.break isPipeOrEscape remaining
            chunks1 :: [Text]
            chunks1 = if Txt.null plain then chunks else plain : chunks
        in  case Txt.uncons rest of
                Nothing ->
                    [Txt.concat (reverse chunks1)]
                Just ('|', afterPipe) ->
                    Txt.concat (reverse chunks1) : go [] afterPipe
                Just ('\\', afterSlash) ->
                    case Txt.uncons afterSlash of
                        Nothing ->
                            [Txt.concat (reverse ("\\" : chunks1))]
                        Just ('|', afterEsc) ->
                            go ("|" : chunks1) afterEsc
                        Just (other, afterEsc) ->
                            go (Txt.pack ['\\', other] : chunks1) afterEsc
                Just (ch, afterCh) ->
                    go (Txt.singleton ch : chunks1) afterCh

    isPipeOrEscape :: Char -> Bool
    isPipeOrEscape c = c == '|' || c == '\\'

parseTableCells :: Int -> Text -> [[MarkdownInline]]
parseTableCells colCount line =
    let rawCells :: [Text]
        rawCells = splitTableRow line

        padded :: [Text]
        padded = take colCount (rawCells <> repeat "")
    in  map parseInlines padded

parseUnorderedListItem :: Text -> Maybe ListItem
parseUnorderedListItem line =
    let trimmedLeft :: Text
        trimmedLeft = stripUpTo3Spaces line
    in  case Txt.uncons trimmedLeft of
            Just (marker, afterMarker)
                | marker == '-' || marker == '*' || marker == '+' ->
                    case Txt.uncons afterMarker of
                        Just (sp, body)
                            | sp == ' ' || sp == '\t' -> Just (buildListItem (Txt.stripStart body))
                        _ -> Nothing
            _ -> Nothing

collectUnorderedItems :: [Text] -> ([ListItem], [Text])
collectUnorderedItems = go []
  where
    go :: [ListItem] -> [Text] -> ([ListItem], [Text])
    go acc [] = (reverse acc, [])
    go acc (l : ls) = case parseUnorderedListItem l of
        Just item -> go (item : acc) ls
        Nothing   -> (reverse acc, l : ls)

parseOrderedListItem :: Text -> Maybe (Int, ListItem)
parseOrderedListItem line =
    let trimmedLeft :: Text
        trimmedLeft = stripUpTo3Spaces line

        (digits, afterDigits) = Txt.span Char.isDigit trimmedLeft
    in  if Txt.null digits || Txt.length digits > 9
            then Nothing
            else case Txt.uncons afterDigits of
                Just (punct, afterPunct)
                    | punct == '.' || punct == ')' ->
                        case Txt.uncons afterPunct of
                            Just (sp, body)
                                | sp == ' ' || sp == '\t' ->
                                    let num :: Int
                                        num = foldl' (\ acc ch -> acc * 10 + Char.digitToInt ch) 0 (Txt.unpack digits)
                                    in  Just (num, buildListItem (Txt.stripStart body))
                            _ -> Nothing
                _ -> Nothing

collectOrderedItems :: [Text] -> ([ListItem], [Text])
collectOrderedItems = go []
  where
    go :: [ListItem] -> [Text] -> ([ListItem], [Text])
    go acc [] = (reverse acc, [])
    go acc (l : ls) = case parseOrderedListItem l of
        Just (_, item) -> go (item : acc) ls
        Nothing        -> (reverse acc, l : ls)

buildListItem :: Text -> ListItem
buildListItem body
    | Just rest <- Txt.stripPrefix "[ ] " body = ListItem (Just False) (parseInlines rest)
    | Just rest <- Txt.stripPrefix "[x] " body = ListItem (Just True)  (parseInlines rest)
    | Just rest <- Txt.stripPrefix "[X] " body = ListItem (Just True)  (parseInlines rest)
    | body == "[ ]"                            = ListItem (Just False) []
    | body == "[x]" || body == "[X]"           = ListItem (Just True)  []
    | otherwise                                = ListItem Nothing      (parseInlines body)

collectParagraphLines :: [Text] -> ([Text], [Text])
collectParagraphLines [] = ([], [])
collectParagraphLines (first : rest) = go [first] rest
  where
    go :: [Text] -> [Text] -> ([Text], [Text])
    go acc [] = (reverse acc, [])
    go acc (l : ls)
        | startsNewBlock l ls = (reverse acc, l : ls)
        | otherwise           = go (l : acc) ls

    startsNewBlock :: Text -> [Text] -> Bool
    startsNewBlock l ls =
        Txt.null (Txt.strip l)
            || isJust (parseFenceOpen l)
            || isThematicBreak l
            || isJust (parseAtxHeading l)
            || isBlockQuoteLine l
            || isJust (parseTableStart l ls)
            || isJust (parseUnorderedListItem l)
            || isJust (parseOrderedListItem l)

parseParagraphInlines :: [Text] -> [MarkdownInline]
parseParagraphInlines [] = []
parseParagraphInlines [single] = parseInlines (Txt.stripStart single)
parseParagraphInlines (l : ls) =
    let hasHardBreak :: Bool
        hasHardBreak = Txt.isSuffixOf "  " l || Txt.isSuffixOf "\\" l

        cleanedLine :: Text
        cleanedLine
            | Txt.isSuffixOf "  " l = Txt.stripEnd l
            | Txt.isSuffixOf "\\" l = Txt.dropEnd 1 l
            | otherwise             = Txt.stripEnd l

        breakNode :: MarkdownInline
        breakNode = if hasHardBreak then InlineHardBreak else InlineSoftBreak
    in  parseInlines (Txt.stripStart cleanedLine) <> [breakNode] <> parseParagraphInlines ls

-- | 単一行 (または段落セグメント) のインライン Markdown 記法をパースする。
parseInlines :: Text -> [MarkdownInline]
parseInlines = mergeAdjacentText . go False
  where
    go :: Bool -> Text -> [MarkdownInline]
    go _ txt | Txt.null txt = []
    go prevWord txt = case Txt.uncons txt of
        Nothing -> []
        Just (ch, rest)
            | ch == '\\' ->
                case Txt.uncons rest of
                    Just (escaped, afterEsc) | isAsciiPunctuation escaped ->
                        InlineText (Txt.singleton escaped) : go False afterEsc
                    _ ->
                        InlineText "\\" : go False rest
            | ch == '`' ->
                case parseInlineCode txt of
                    Just (codeText, afterCode) -> InlineCode codeText : go False afterCode
                    Nothing                    -> InlineText "`" : go False rest
            | ch == '!' && Txt.isPrefixOf "[" rest ->
                case parseBracketAndParen rest of
                    Just (altText, urlRaw, afterImg) ->
                        case mkSafeUrl urlRaw of
                            Just safeUrl -> InlineImage safeUrl altText : go False afterImg
                            Nothing      -> InlineText altText : go False afterImg
                    Nothing ->
                        InlineText "!" : go False rest
            | ch == '[' ->
                case parseBracketAndParen txt of
                    Just (labelText, urlRaw, afterLink) ->
                        case mkSafeUrl urlRaw of
                            Just safeUrl -> InlineLink safeUrl (parseInlines labelText) : go False afterLink
                            Nothing      -> parseInlines labelText <> go False afterLink
                    Nothing ->
                        InlineText "[" : go False rest
            | Txt.isPrefixOf "**" txt ->
                case parseDelimited "**" False txt of
                    Just (inner, after) -> InlineStrong (parseInlines inner) : go False after
                    Nothing             -> InlineText "**" : go False (Txt.drop 2 txt)
            | Txt.isPrefixOf "__" txt && not prevWord ->
                case parseDelimited "__" True txt of
                    Just (inner, after) -> InlineStrong (parseInlines inner) : go False after
                    Nothing             -> InlineText "__" : go False (Txt.drop 2 txt)
            | Txt.isPrefixOf "~~" txt ->
                case parseDelimited "~~" False txt of
                    Just (inner, after) -> InlineStrike (parseInlines inner) : go False after
                    Nothing             -> InlineText "~~" : go False (Txt.drop 2 txt)
            | ch == '*' ->
                case parseDelimited "*" False txt of
                    Just (inner, after) -> InlineEmph (parseInlines inner) : go False after
                    Nothing             -> InlineText "*" : go False rest
            | ch == '_' && not prevWord ->
                case parseDelimited "_" True txt of
                    Just (inner, after) -> InlineEmph (parseInlines inner) : go False after
                    Nothing             -> InlineText "_" : go False rest
            | otherwise ->
                let (plain, remaining) = Txt.break isInlineSpecial rest
                    chunk :: Text
                    chunk = Txt.cons ch plain

                    endsWithWord :: Bool
                    endsWithWord = maybe False (isWordChar . snd) (Txt.unsnoc chunk)
                in  InlineText chunk : go endsWithWord remaining

    isInlineSpecial :: Char -> Bool
    isInlineSpecial c =
        c == '\\' || c == '`' || c == '*' || c == '_' || c == '~' || c == '[' || c == '!'

    isWordChar :: Char -> Bool
    isWordChar = Char.isAlphaNum

    isAsciiPunctuation :: Char -> Bool
    isAsciiPunctuation c =
        Char.isAscii c && (Char.isPunctuation c || Char.isSymbol c)

parseInlineCode :: Text -> Maybe (Text, Text)
parseInlineCode txt =
    let (ticks, afterOpen) = Txt.span (== '`') txt
        tickLen :: Int
        tickLen = Txt.length ticks
    in  if tickLen == 0
            then Nothing
            else findClosingTicks ticks afterOpen
  where
    findClosingTicks :: Text -> Text -> Maybe (Text, Text)
    findClosingTicks delim remaining =
        let (before, matchAndRest) = Txt.breakOn delim remaining
        in  if Txt.null matchAndRest
                then Nothing
                else
                    let afterDelim :: Text
                        afterDelim = Txt.drop (Txt.length delim) matchAndRest
                    in  Just (Txt.strip before, afterDelim)

-- | 区切り記号 (例: @**@, @_@) で囲まれたインライン区間を切り出す。
-- @requireWordBoundary@ が 'True' の場合 (アンダースコア系)、閉じ記号の直後が英数字であれば
-- 語中アンダースコア (@snake_case_name@ 等) とみなし強調として解釈しない。
parseDelimited :: Text -> Bool -> Text -> Maybe (Text, Text)
parseDelimited delim requireWordBoundary txt = do
    afterOpen <- Txt.stripPrefix delim txt
    if Txt.null afterOpen || Txt.isPrefixOf " " afterOpen
        then Nothing
        else do
            let (inner, matchAndRest) = Txt.breakOn delim afterOpen
                afterClose :: Text
                afterClose = Txt.drop (Txt.length delim) matchAndRest

                followedByWord :: Bool
                followedByWord = maybe False (Char.isAlphaNum . fst) (Txt.uncons afterClose)
            if Txt.null matchAndRest
                || Txt.null inner
                || Txt.isSuffixOf " " inner
                || (requireWordBoundary && followedByWord)
                then Nothing
                else Just (inner, afterClose)

-- | 先頭 @[@ に対応する最初の @]@ をブラケットのネストを考慮して特定し、
-- その直後が @(@ であれば対応する閉じ @)@ までを O(n) のスライスで抽出する。
-- これにより @[note] and [link](url)@ のような複数ブラケットを含む文でも誤結合しない。
parseBracketAndParen :: Text -> Maybe (Text, Text, Text)
parseBracketAndParen txt = do
    afterOpenBracket <- Txt.stripPrefix "[" txt
    (label, afterCloseBracket) <- splitBalanced '[' ']' afterOpenBracket
    afterOpenParen <- Txt.stripPrefix "(" afterCloseBracket
    (rawUrl, afterCloseParen) <- splitBalanced '(' ')' afterOpenParen
    Just (label, Txt.strip rawUrl, afterCloseParen)

-- | 開き/閉じ文字のネストを追跡しつつ O(n) スライスで最初の対応閉じ文字までを分割する。
splitBalanced :: Char -> Char -> Text -> Maybe (Text, Text)
splitBalanced openCh closeCh input = do
    closeIdx <- findCloseIndex (0 :: Int) (0 :: Int) input
    let (inside, closeAndRest) = Txt.splitAt closeIdx input
    Just (inside, Txt.drop 1 closeAndRest)
  where
    findCloseIndex :: Int -> Int -> Text -> Maybe Int
    findCloseIndex depth idx remaining =
        let (skipped, rest) = Txt.break isRelevant remaining
            idx1 :: Int
            idx1 = idx + Txt.length skipped
        in  case Txt.uncons rest of
                Nothing -> Nothing
                Just (ch, afterCh)
                    | ch == '\\' ->
                        case Txt.uncons afterCh of
                            Nothing         -> Nothing
                            Just (_, rest2) -> findCloseIndex depth (idx1 + 2) rest2
                    | ch == openCh ->
                        findCloseIndex (depth + 1) (idx1 + 1) afterCh
                    | ch == closeCh && depth == 0 ->
                        Just idx1
                    | ch == closeCh ->
                        findCloseIndex (depth - 1) (idx1 + 1) afterCh
                    | otherwise ->
                        findCloseIndex depth (idx1 + 1) afterCh

    isRelevant :: Char -> Bool
    isRelevant c = c == openCh || c == closeCh || c == '\\'

-- | 連続する 'InlineText' ノードを O(n) の単一 'Txt.concat' で結合する。
mergeAdjacentText :: [MarkdownInline] -> [MarkdownInline]
mergeAdjacentText [] = []
mergeAdjacentText (InlineText first : rest) =
    let (moreTexts, afterTexts) = collectTexts [first] rest
    in  InlineText (Txt.concat (reverse moreTexts)) : mergeAdjacentText afterTexts
  where
    collectTexts :: [Text] -> [MarkdownInline] -> ([Text], [MarkdownInline])
    collectTexts acc (InlineText t : xs) = collectTexts (t : acc) xs
    collectTexts acc xs                  = (acc, xs)
mergeAdjacentText (item : rest) =
    item : mergeAdjacentText rest

stripUpTo3Spaces :: Text -> Text
stripUpTo3Spaces = go (0 :: Int)
  where
    go :: Int -> Text -> Text
    go n s
        | n >= 3    = s
        | otherwise = case Txt.uncons s of
            Just (' ', rest) -> go (n + 1) rest
            _                -> s

-- | パース済み Markdown ブロック AST を安全な Miso 'View' ツリーへレンダリングする。
renderMarkdownBlocks :: [MarkdownBlock] -> View context props model action
renderMarkdownBlocks blocks =
    H.div_ [ HP.class_ "tt-md-preview" ] (map renderBlock blocks)

-- | Markdown テキストを直接安全な Miso 'View' へレンダリングする。
renderMarkdownView :: Text -> View context props model action
renderMarkdownView = renderMarkdownBlocks . parseMarkdown

renderBlock :: MarkdownBlock -> View context props model action
renderBlock (MdHeading level inlines) =
    let children :: [View context props model action]
        children = map renderInline inlines
    in  case level of
            1 -> H.h1_ [] children
            2 -> H.h2_ [] children
            3 -> H.h3_ [] children
            4 -> H.h4_ [] children
            5 -> H.h5_ [] children
            _ -> H.h6_ [] children
renderBlock (MdParagraph inlines) =
    H.p_ [] (map renderInline inlines)
renderBlock (MdCodeBlock mLang codeBody) =
    let codeAttrs :: [Attribute model action]
        codeAttrs = case mLang of
            Nothing   -> []
            Just lang -> [ HP.class_ (ms ("language-" <> lang)) ]
    in  H.pre_ [ HP.class_ "tt-md-pre" ]
            [ H.code_ codeAttrs [ text (ms codeBody) ] ]
renderBlock (MdBlockQuote innerBlocks) =
    H.blockquote_ [ HP.class_ "tt-md-quote" ] (map renderBlock innerBlocks)
renderBlock (MdUnorderedList items) =
    H.ul_ [ HP.class_ "tt-md-list" ] (map renderListItem items)
renderBlock (MdOrderedList startNum items) =
    let listAttrs :: [Attribute model action]
        listAttrs =
            [ HP.class_ "tt-md-list" ]
                <> [ MP.textProp "start" (ms (Txt.pack (show startNum))) | startNum /= 1 ]
    in  H.ol_ listAttrs (map renderListItem items)
renderBlock (MdTable aligns headers rows) =
    H.div_ [ HP.class_ "tt-md-table-wrap" ]
        [ H.table_ [ HP.class_ "tt-md-table" ]
            [ H.thead_ []
                [ H.tr_ [] (zipWith renderHeaderCell paddedAligns headers) ]
            , H.tbody_ []
                (map (renderBodyRow paddedAligns) rows)
            ]
        ]
  where
    paddedAligns :: [TableAlign]
    paddedAligns = aligns <> repeat AlignDefault

    renderHeaderCell :: TableAlign -> [MarkdownInline] -> View context props model action
    renderHeaderCell align cellInlines =
        H.th_ (alignClassAttr align) (map renderInline cellInlines)

    renderBodyRow :: [TableAlign] -> [[MarkdownInline]] -> View context props model action
    renderBodyRow colAligns cells =
        H.tr_ [] (zipWith renderBodyCell colAligns cells)

    renderBodyCell :: TableAlign -> [MarkdownInline] -> View context props model action
    renderBodyCell align cellInlines =
        H.td_ (alignClassAttr align) (map renderInline cellInlines)

    alignClassAttr :: TableAlign -> [Attribute model action]
    alignClassAttr AlignDefault = []
    alignClassAttr AlignLeft    = [ HP.class_ "tt-md-align-left" ]
    alignClassAttr AlignCenter  = [ HP.class_ "tt-md-align-center" ]
    alignClassAttr AlignRight   = [ HP.class_ "tt-md-align-right" ]
renderBlock MdThematicBreak =
    H.hr_ [ HP.class_ "tt-md-hr" ]

renderListItem :: ListItem -> View context props model action
renderListItem item = case item.taskChecked of
    Nothing ->
        H.li_ [] (map renderInline item.content)
    Just isDone ->
        H.li_ [ HP.class_ "tt-md-task-item" ]
            ( H.input_
                [ HP.type_ "checkbox"
                , HP.class_ "tt-md-task-box"
                , HP.checked_ isDone
                , HP.disabled_
                ]
            : map renderInline item.content
            )

renderInline :: MarkdownInline -> View context props model action
renderInline (InlineText txt) =
    text (ms txt)
renderInline (InlineCode codeText) =
    H.code_ [ HP.class_ "tt-md-inline-code" ] [ text (ms codeText) ]
renderInline (InlineEmph inlines) =
    H.em_ [] (map renderInline inlines)
renderInline (InlineStrong inlines) =
    H.strong_ [] (map renderInline inlines)
renderInline (InlineStrike inlines) =
    H.del_ [] (map renderInline inlines)
renderInline (InlineLink safeUrl labelInlines) =
    let hrefTxt :: Text
        hrefTxt = toText safeUrl

        externalAttrs :: [Attribute model action]
        externalAttrs =
            if isExternalUrl hrefTxt
                then [ HP.target_ "_blank", HP.rel_ "noopener noreferrer" ]
                else []
    in  H.a_ ([ HP.href_ (ms hrefTxt) ] <> externalAttrs) (map renderInline labelInlines)
renderInline (InlineImage safeUrl altText) =
    H.img_
        [ HP.src_ (ms (toText safeUrl))
        , HP.alt_ (ms altText)
        , HP.class_ "tt-md-img"
        , HP.loading_ "lazy"
        ]
renderInline InlineSoftBreak =
    text " "
renderInline InlineHardBreak =
    H.br_ []

isExternalUrl :: Text -> Bool
isExternalUrl url =
    Txt.isPrefixOf "http://" (Txt.toLower url)
        || Txt.isPrefixOf "https://" (Txt.toLower url)
