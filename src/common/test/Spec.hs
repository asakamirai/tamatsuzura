{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main
    ( main
    ) where

import qualified Data.Aeson                    as Aeson
import qualified Data.Char                     as Char
import           Data.Either                   (isLeft)
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                    (isNothing)
import           Data.Text                     (Text)
import qualified Data.Text                     as Txt
import           Data.Time.Calendar            (fromGregorian)
import           Data.Time.Clock               (UTCTime(..), secondsToDiffTime)
import           TamaTsuzura.Common.Api
    ( ApiError(..)
    , ApiErrorCode(..)
    , CreateDocumentRequest(..)
    , DocumentListQuery(..)
    , DocumentListResponse(..)
    , DocumentStatusFilter(..)
    , FolderListResponse(..)
    , HealthStatus
    , TagListResponse(..)
    , UpdateDocumentRequest(..)
    , defaultCreateDocumentRequest
    , defaultDocumentListQuery
    , healthOk
    , listLimitValue
    , listOffsetValue
    , matchesDocumentStatus
    , mkListLimit
    , mkListOffset
    , mkSearchQuery
    , parseApiErrorCode
    , parseDocumentStatusFilter
    , validateCreateDocumentRequest
    , validateUpdateDocumentRequest
    , validationErrorToApiError
    )
import           TamaTsuzura.Common.Auth
    ( AuthSessionResponse(..)
    , CsrfToken
    , DisplayName
    , LoginRequest(..)
    , Password
    , RegisterRequest(..)
    , UserId
    , UserProfile(..)
    , Username
    , mkCsrfToken
    , mkDisplayName
    , mkPassword
    , mkUserId
    , mkUsername
    , passwordByteLength
    , passwordCharLength
    , unsafePasswordText
    , validateNewPassword
    , validateRegisterRequest
    )
import           TamaTsuzura.Common.Diff
    ( DiffError(..)
    , ParsedDiff(..)
    , UnifiedDiff(..)
    , applyParsedDiff
    , applyUnifiedDiff
    , applyUnifiedDiffReverse
    , defaultContextLines
    , emptyUnifiedDiff
    , generateDiff
    , generateDiffWithContext
    , generateUnifiedDiff
    , generateUnifiedDiffWithContext
    , parseUnifiedDiff
    , renderUnifiedDiff
    , reverseParsedDiff
    , reverseUnifiedDiff
    )
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody(..)
    , DocumentFormat(..)
    , DocumentId
    , DocumentSummary(..)
    , DocumentTitle
    , FolderPath
    , RevisionNumber
    , Tag
    , TagSet
    , defaultDocumentFormat
    , documentBodyByteLength
    , emptyDocumentBody
    , emptyDocumentTitle
    , emptyTagSet
    , folderParent
    , folderSegments
    , initialRevisionNumber
    , isFolderPrefixOf
    , isSummaryTrashed
    , isTrashed
    , memberTag
    , mkDocumentBody
    , mkDocumentId
    , mkDocumentTitle
    , mkFolderPath
    , mkRevisionNumber
    , mkTag
    , mkTagSet
    , mkTagSetFromTexts
    , nextRevisionNumber
    , parseDocumentFormat
    , revisionNumberValue
    , tagSetSize
    , tagSetToList
    , toDocumentSummary
    )
import           TamaTsuzura.Common.Revision
    ( ContentHash
    , RestoreRevisionRequest(..)
    , RevisionDetail(..)
    , RevisionId
    , RevisionListResponse(..)
    , RevisionSummary(..)
    , defaultRestoreRevisionRequest
    , mkContentHash
    , mkRevisionId
    , revisionContentCanonicalText
    , toRevisionSummary
    )
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation
    ( ValidationError(..)
    , contentHashHexLength
    , defaultListLimit
    , maxCsrfTokenLength
    , maxDisplayNameLength
    , maxDocumentBodyBytes
    , maxDocumentIdLength
    , maxDocumentTitleLength
    , maxFolderDepth
    , maxFolderPathLength
    , maxListLimit
    , maxPasswordBytes
    , maxRevisionIdLength
    , maxSearchQueryLength
    , maxTagLength
    , maxTagsPerDocument
    , maxUserIdLength
    , maxUsernameLength
    , minCsrfTokenLength
    , minListLimit
    , minPasswordLength
    , minRevisionNumber
    , minUsernameLength
    , validationErrorField
    )
import           Test.Hspec                    (Spec, describe, expectationFailure, hspec, it, shouldBe, shouldSatisfy)
import           Test.QuickCheck               (Gen, chooseInt, elements, forAll, listOf, listOf1, property, vectorOf)

main :: IO ()
main = hspec spec

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2026 11 20) (secondsToDiffTime 18000)

sampleLaterTime :: UTCTime
sampleLaterTime = UTCTime (fromGregorian 2026 11 20) (secondsToDiffTime 21600)

genValidTagText :: Gen Text
genValidTagText = do
    len <- chooseInt (1, 16)
    chars <- vectorOf len (elements tagAlphabet)
    pure (Txt.pack chars)
  where
    tagAlphabet :: [Char]
    tagAlphabet = ['a' .. 'z'] <> ['0' .. '9'] <> ['-', '_', 'あ', '漢']

genValidTagList :: Gen [Text]
genValidTagList = do
    count <- chooseInt (0, maxTagsPerDocument)
    vectorOf count genValidTagText

genValidFolderSegments :: Gen [Text]
genValidFolderSegments = do
    depth <- chooseInt (1, maxFolderDepth)
    vectorOf depth genSegment
  where
    segAlphabet :: [Char]
    segAlphabet = ['a' .. 'z'] <> ['0' .. '9'] <> ['-', '_', '日', '記']

    genSegment :: Gen Text
    genSegment = do
        chars <- listOf1 (elements segAlphabet)
        pure (Txt.take 16 (Txt.pack chars))

genValidUsernameText :: Gen Text
genValidUsernameText = do
    hd <- elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])
    restLen <- chooseInt (minUsernameLength - 1, maxUsernameLength - 1)
    rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_', '-']))
    pure (Txt.pack (hd : rest))

-- | 差分アルゴリズム検証用の多様なテキスト生成器。
-- 空行・重複行・日本語マルチバイト・CRLF・@\\ No newline at end of file@ 文字列・末尾改行有無を網羅する。
genDiffText :: Gen Text
genDiffText = do
    lineList <- listOf genLine
    hasTrailingNewline <- elements [True, False]
    pure $ case lineList of
        [] -> ""
        _  ->
            let joined :: Text
                joined = Txt.intercalate "\n" lineList
            in  if hasTrailingNewline then joined <> "\n" else joined
  where
    genLine :: Gen Text
    genLine = elements
        [ ""
        , "alpha"
        , "beta"
        , "gamma"
        , "delta"
        , "日本語の行"
        , "line with \r"
        , "\\ No newline at end of file"
        , "@@ -1,2 +1,2 @@"
        , "+plus-prefixed"
        , "-minus-prefixed"
        , " space-prefixed"
        ]

isStrictlyAscending :: Ord a => [a] -> Bool
isStrictlyAscending []             = True
isStrictlyAscending [_]            = True
isStrictlyAscending (x : y : rest) = x < y && isStrictlyAscending (y : rest)

spec :: Spec
spec = do
    describe "HealthStatus JSON serialization" $ do
        it "roundtrips healthOk through JSON" $
            (Aeson.decode (Aeson.encode healthOk) :: Maybe HealthStatus) `shouldBe` Just healthOk

        it "encodes status field matching OpenAPI schema" $
            Aeson.encode healthOk `shouldBe` "{\"status\":\"ok\"}"

    describe "DocumentId validation and JSON" $ do
        it "accepts UUIDs and URL-safe ASCII identifiers up to max length" $ do
            fmap toText (mkDocumentId "550e8400-e29b-41d4-a716-446655440000")
                `shouldBe` Right "550e8400-e29b-41d4-a716-446655440000"
            let maxId :: Text
                maxId = Txt.replicate maxDocumentIdLength "a"
            fmap toText (mkDocumentId maxId) `shouldBe` Right maxId

        it "rejects empty, overly long, or non-URL-safe IDs" $ do
            mkDocumentId "" `shouldBe` Left EmptyDocumentId
            mkDocumentId (Txt.replicate (maxDocumentIdLength + 1) "a")
                `shouldBe` Left (DocumentIdTooLong (maxDocumentIdLength + 1) maxDocumentIdLength)
            mkDocumentId "bad/id" `shouldBe` Left (InvalidDocumentIdChar "bad/id")
            mkDocumentId "bad id" `shouldBe` Left (InvalidDocumentIdChar "bad id")
            mkDocumentId "日本語" `shouldBe` Left (InvalidDocumentIdChar "日本語")

        it "roundtrips valid DocumentId and rejects invalid JSON" $ do
            case mkDocumentId "doc_01-abc" of
                Left err    -> expectationFailure ("Expected valid DocumentId, got: " <> show err)
                Right docId -> Aeson.decode (Aeson.encode docId) `shouldBe` Just docId
            (Aeson.decode "\"bad/id\"" :: Maybe DocumentId) `shouldBe` Nothing

    describe "DocumentTitle validation and JSON" $ do
        it "allows empty and whitespace-only titles by normalizing to emptyDocumentTitle" $ do
            mkDocumentTitle "" `shouldBe` Right emptyDocumentTitle
            mkDocumentTitle "   " `shouldBe` Right emptyDocumentTitle
            fmap toText (mkDocumentTitle "  日記メモ  ") `shouldBe` Right "日記メモ"

        it "enforces maximum length and rejects control characters" $ do
            let maxTitle :: Text
                maxTitle = Txt.replicate maxDocumentTitleLength "あ"
            fmap toText (mkDocumentTitle maxTitle) `shouldBe` Right maxTitle
            mkDocumentTitle (Txt.replicate (maxDocumentTitleLength + 1) "あ")
                `shouldBe` Left (TitleTooLong (maxDocumentTitleLength + 1) maxDocumentTitleLength)
            mkDocumentTitle "line1\nline2" `shouldBe` Left TitleContainsControlChar
            mkDocumentTitle "tab\there" `shouldBe` Left TitleContainsControlChar
            mkDocumentTitle "nul\0byte" `shouldBe` Left TitleContainsControlChar

        it "roundtrips arbitrary valid titles through JSON" $
            property $ \ str ->
                let clean :: Text
                    clean = Txt.take maxDocumentTitleLength (Txt.filter (not . Char.isControl) (Txt.pack str))
                in  case mkDocumentTitle clean of
                        Left _      -> False
                        Right title -> Aeson.decode (Aeson.encode title) == Just title

        it "rejects control characters during JSON decoding" $
            (Aeson.decode "\"hello\\nworld\"" :: Maybe DocumentTitle) `shouldBe` Nothing

    describe "DocumentBody validation and UTF-8 size" $ do
        it "measures UTF-8 byte length accurately for multibyte text" $ do
            documentBodyByteLength (DocumentBody "abc") `shouldBe` 3
            documentBodyByteLength (DocumentBody "あいう") `shouldBe` 9

        it "accepts body at exact maxDocumentBodyBytes and rejects ASCII and multibyte overflows" $ do
            let exactBody :: Text
                exactBody = Txt.replicate maxDocumentBodyBytes "a"
                overAsciiBody :: Text
                overAsciiBody = exactBody <> "a"
                multibyteCount :: Int
                multibyteCount = (maxDocumentBodyBytes `div` 3) + 1
                overMultibyteBody :: Text
                overMultibyteBody = Txt.replicate multibyteCount "あ"
            fmap documentBodyByteLength (mkDocumentBody exactBody) `shouldBe` Right maxDocumentBodyBytes
            mkDocumentBody overAsciiBody
                `shouldBe` Left (BodyTooLarge (maxDocumentBodyBytes + 1) maxDocumentBodyBytes)
            mkDocumentBody overMultibyteBody
                `shouldBe` Left (BodyTooLarge (multibyteCount * 3) maxDocumentBodyBytes)

        it "preserves arbitrary UTF-8 string content via ToText" $
            property $ \ str ->
                let txt :: Text
                    txt = Txt.pack str
                in  toText (DocumentBody txt) == txt

    describe "DocumentFormat" $ do
        it "defaults to FormatMarkdown and roundtrips all enum values through text and JSON" $ do
            defaultDocumentFormat `shouldBe` FormatMarkdown
            let allFormats :: [DocumentFormat]
                allFormats = [minBound .. maxBound]
            map (parseDocumentFormat . toText) allFormats `shouldBe` map Right allFormats
            map (Aeson.decode . Aeson.encode) allFormats `shouldBe` map Just allFormats

        it "rejects unknown format names" $ do
            parseDocumentFormat "html" `shouldBe` Left (InvalidDocumentFormat "html")
            (Aeson.decode "\"html\"" :: Maybe DocumentFormat) `shouldBe` Nothing

    describe "Tag and TagSet" $ do
        it "validates and trims tags while rejecting empty, control chars, and overly long tags" $ do
            fmap toText (mkTag "  仕事  ") `shouldBe` Right "仕事"
            mkTag "" `shouldBe` Left EmptyTag
            mkTag "   " `shouldBe` Left EmptyTag
            mkTag "a\nb" `shouldBe` Left (TagContainsControlChar "a\nb")
            let maxTag :: Text
                maxTag = Txt.replicate maxTagLength "t"
            fmap toText (mkTag maxTag) `shouldBe` Right maxTag
            mkTag (Txt.replicate (maxTagLength + 1) "t")
                `shouldBe` Left (TagTooLong (maxTagLength + 1) maxTagLength)

        it "deduplicates, sorts canonically, and enforces maxTagsPerDocument on raw input length" $ do
            case mkTagSetFromTexts ["beta", "alpha", "beta"] of
                Left err     -> expectationFailure ("Expected valid TagSet, got: " <> show err)
                Right tagSet -> do
                    map toText (tagSetToList tagSet) `shouldBe` ["alpha", "beta"]
                    tagSetSize tagSet `shouldBe` 2
                    case mkTag "alpha" of
                        Left err  -> expectationFailure ("Expected valid Tag, got: " <> show err)
                        Right tag -> memberTag tag tagSet `shouldBe` True
            let exactTagTexts :: [Text]
                exactTagTexts = map (\ n -> "tag-" <> Txt.pack (show n)) [1 .. maxTagsPerDocument]
                tooManyTagTexts :: [Text]
                tooManyTagTexts = map (\ n -> "tag-" <> Txt.pack (show n)) [1 .. maxTagsPerDocument + 1]
                tooManyDupTexts :: [Text]
                tooManyDupTexts = replicate (maxTagsPerDocument + 1) "dup"
            fmap tagSetSize (mkTagSetFromTexts exactTagTexts) `shouldBe` Right maxTagsPerDocument
            mkTagSetFromTexts tooManyTagTexts
                `shouldBe` Left (TooManyTags (maxTagsPerDocument + 1) maxTagsPerDocument)
            mkTagSetFromTexts tooManyDupTexts
                `shouldBe` Left (TooManyTags (maxTagsPerDocument + 1) maxTagsPerDocument)

        it "deduplicates tags during JSON decoding and rejects invalid or oversized tag arrays" $ do
            let decoded :: Maybe TagSet
                decoded = Aeson.decode "[\"b\",\"a\",\"b\"]"
            fmap (map toText . tagSetToList) decoded `shouldBe` Just ["a", "b"]
            (Aeson.decode "[\"\"]" :: Maybe TagSet) `shouldBe` Nothing
            let tooManyJson :: Maybe TagSet
                tooManyJson = Aeson.decode (Aeson.encode (replicate (maxTagsPerDocument + 1) ("dup" :: Text)))
            tooManyJson `shouldBe` Nothing

        it "satisfies deduplication, ordering, membership, and JSON roundtrip properties" $
            forAll genValidTagList $ \ rawTexts ->
                case (traverse mkTag rawTexts, mkTagSetFromTexts rawTexts) of
                    (Right tags, Right tagSet) ->
                        let sorted :: [Tag]
                            sorted = tagSetToList tagSet
                        in  isStrictlyAscending sorted
                                && tagSetSize tagSet <= length rawTexts
                                && all (`memberTag` tagSet) tags
                                && Aeson.decode (Aeson.encode tagSet) == Just tagSet
                    _ -> False

    describe "FolderPath" $ do
        it "normalizes leading/trailing slashes and computes segments, parent, and prefix" $ do
            case mkFolderPath "/work/notes/2026/" of
                Left err -> expectationFailure ("Expected valid FolderPath, got: " <> show err)
                Right fp -> do
                    toText fp `shouldBe` "work/notes/2026"
                    NE.toList (folderSegments fp) `shouldBe` ["work", "notes", "2026"]
                    fmap toText (folderParent fp) `shouldBe` Just "work/notes"
                    Aeson.decode (Aeson.encode fp) `shouldBe` Just fp

            case mkFolderPath "work" of
                Left err -> expectationFailure ("Expected valid FolderPath, got: " <> show err)
                Right fp -> folderParent fp `shouldBe` Nothing

            case (mkFolderPath "work", mkFolderPath "work/notes", mkFolderPath "workshop", mkFolderPath "a/b", mkFolderPath "a/b/c", mkFolderPath "a/bc") of
                (Right work, Right workNotes, Right workshop, Right ab, Right abc, Right abC) -> do
                    isFolderPrefixOf work work `shouldBe` True
                    isFolderPrefixOf work workNotes `shouldBe` True
                    isFolderPrefixOf work workshop `shouldBe` False
                    isFolderPrefixOf ab abc `shouldBe` True
                    isFolderPrefixOf ab abC `shouldBe` False
                _ -> expectationFailure "Expected all sample folder paths to construct successfully"

        it "rejects empty, traversal, backslash, control chars, edge whitespace, and excessive depth/length" $ do
            mkFolderPath "" `shouldBe` Left EmptyFolderPath
            mkFolderPath "/" `shouldBe` Left EmptyFolderPath
            mkFolderPath "///" `shouldBe` Left EmptyFolderPath
            mkFolderPath "work//notes" `shouldBe` Left (InvalidFolderPathSegment "work//notes")
            mkFolderPath "." `shouldBe` Left (InvalidFolderPathSegment ".")
            mkFolderPath ".." `shouldBe` Left (InvalidFolderPathSegment "..")
            mkFolderPath "work/./notes" `shouldBe` Left (InvalidFolderPathSegment "work/./notes")
            mkFolderPath "work/../secret" `shouldBe` Left (InvalidFolderPathSegment "work/../secret")
            mkFolderPath "work\\notes" `shouldBe` Left (InvalidFolderPathSegment "work\\notes")
            mkFolderPath "work/\nnotes" `shouldBe` Left (InvalidFolderPathSegment "work/\nnotes")
            mkFolderPath " work/notes" `shouldBe` Left (InvalidFolderPathSegment " work/notes")
            mkFolderPath "work/notes " `shouldBe` Left (InvalidFolderPathSegment "work/notes ")

            let maxDepthPath :: Text
                maxDepthPath = Txt.intercalate "/" (replicate maxFolderDepth "a")
                tooDeepPath :: Text
                tooDeepPath = Txt.intercalate "/" (replicate (maxFolderDepth + 1) "a")
            fmap toText (mkFolderPath maxDepthPath) `shouldBe` Right maxDepthPath
            mkFolderPath tooDeepPath `shouldBe` Left (FolderPathTooDeep (maxFolderDepth + 1) maxFolderDepth)

            let maxLenPath :: Text
                maxLenPath = Txt.replicate maxFolderPathLength "a"
                tooLongPath :: Text
                tooLongPath = Txt.replicate (maxFolderPathLength + 1) "a"
            fmap toText (mkFolderPath maxLenPath) `shouldBe` Right maxLenPath
            mkFolderPath tooLongPath `shouldBe` Left (FolderPathTooLong (maxFolderPathLength + 1) maxFolderPathLength)

        it "rejects empty string when decoding FolderPath from JSON" $
            (Aeson.decode "\"\"" :: Maybe FolderPath) `shouldBe` Nothing

        it "satisfies normalization idempotency, segment reconstruction, parent prefix, and JSON roundtrip properties" $
            forAll genValidFolderSegments $ \ segs ->
                let raw :: Text
                    raw = Txt.intercalate "/" segs
                in  case mkFolderPath raw of
                        Left _   -> False
                        Right fp ->
                            let parentValid :: Bool
                                parentValid = case folderParent fp of
                                    Nothing     -> length segs == 1
                                    Just parent -> isFolderPrefixOf parent fp && parent /= fp
                            in  mkFolderPath (toText fp) == Right fp
                                    && mkFolderPath ("/" <> toText fp <> "/") == Right fp
                                    && Txt.intercalate "/" (NE.toList (folderSegments fp)) == toText fp
                                    && parentValid
                                    && Aeson.decode (Aeson.encode fp) == Just fp

    describe "Document and DocumentSummary" $ do
        it "encodes exact OpenAPI snake_case keys and roundtrips active and trashed documents" $ do
            case (mkDocumentId "doc-1", mkDocumentTitle "朝のメモ", mkFolderPath "diary/2026", mkTagSetFromTexts ["daily", "memo"]) of
                (Right docId, Right docTitle, Right docFolder, Right docTags) -> do
                    let doc :: Document
                        doc = Document
                            { id        = docId
                            , title     = docTitle
                            , body      = DocumentBody "# Hello\n本文です。"
                            , format    = FormatMarkdown
                            , folder    = Just docFolder
                            , tags      = docTags
                            , revision  = initialRevisionNumber
                            , createdAt = sampleTime
                            , updatedAt = sampleLaterTime
                            , deletedAt = Nothing
                            }
                        summary :: DocumentSummary
                        summary = toDocumentSummary doc
                        trashedDoc :: Document
                        trashedDoc = Document
                            { id        = doc.id
                            , title     = doc.title
                            , body      = doc.body
                            , format    = doc.format
                            , folder    = doc.folder
                            , tags      = doc.tags
                            , revision  = nextRevisionNumber doc.revision
                            , createdAt = doc.createdAt
                            , updatedAt = doc.updatedAt
                            , deletedAt = Just sampleLaterTime
                            }
                        expectedDocValue :: Aeson.Value
                        expectedDocValue = Aeson.object
                            [ "id"         Aeson..= ("doc-1" :: Text)
                            , "title"      Aeson..= ("朝のメモ" :: Text)
                            , "body"       Aeson..= ("# Hello\n本文です。" :: Text)
                            , "format"     Aeson..= ("markdown" :: Text)
                            , "folder"     Aeson..= ("diary/2026" :: Text)
                            , "tags"       Aeson..= (["daily", "memo"] :: [Text])
                            , "revision"   Aeson..= (1 :: Int)
                            , "created_at" Aeson..= sampleTime
                            , "updated_at" Aeson..= sampleLaterTime
                            , "deleted_at" Aeson..= Aeson.Null
                            ]
                        expectedSummaryValue :: Aeson.Value
                        expectedSummaryValue = Aeson.object
                            [ "id"         Aeson..= ("doc-1" :: Text)
                            , "title"      Aeson..= ("朝のメモ" :: Text)
                            , "format"     Aeson..= ("markdown" :: Text)
                            , "folder"     Aeson..= ("diary/2026" :: Text)
                            , "tags"       Aeson..= (["daily", "memo"] :: [Text])
                            , "revision"   Aeson..= (1 :: Int)
                            , "created_at" Aeson..= sampleTime
                            , "updated_at" Aeson..= sampleLaterTime
                            , "deleted_at" Aeson..= Aeson.Null
                            ]
                    isTrashed doc `shouldBe` False
                    isSummaryTrashed summary `shouldBe` False
                    isTrashed trashedDoc `shouldBe` True
                    isSummaryTrashed (toDocumentSummary trashedDoc) `shouldBe` True
                    Aeson.toJSON doc `shouldBe` expectedDocValue
                    Aeson.toJSON summary `shouldBe` expectedSummaryValue
                    Aeson.decode (Aeson.encode doc) `shouldBe` Just doc
                    Aeson.decode (Aeson.encode trashedDoc) `shouldBe` Just trashedDoc
                    Aeson.decode (Aeson.encode summary) `shouldBe` Just summary
                _ -> expectationFailure "Expected sample Document components to construct successfully"

        it "rejects Document and DocumentSummary JSON when required nullable keys are missing" $ do
            let missingKeysJson :: Aeson.Value
                missingKeysJson = Aeson.object
                    [ "id"         Aeson..= ("doc-1" :: Text)
                    , "title"      Aeson..= ("" :: Text)
                    , "body"       Aeson..= ("" :: Text)
                    , "format"     Aeson..= ("markdown" :: Text)
                    , "tags"       Aeson..= ([] :: [Text])
                    , "created_at" Aeson..= sampleTime
                    , "updated_at" Aeson..= sampleTime
                    ]
            (Aeson.decode (Aeson.encode missingKeysJson) :: Maybe Document) `shouldBe` Nothing
            (Aeson.decode (Aeson.encode missingKeysJson) :: Maybe DocumentSummary) `shouldBe` Nothing

    describe "API Query Types and DTOs" $ do
        it "provides expected defaultDocumentListQuery values" $ do
            defaultDocumentListQuery.status `shouldBe` StatusActive
            defaultDocumentListQuery.tag `shouldBe` Nothing
            defaultDocumentListQuery.folder `shouldBe` Nothing
            defaultDocumentListQuery.includeSubfolders `shouldBe` False
            defaultDocumentListQuery.search `shouldBe` Nothing
            listLimitValue defaultDocumentListQuery.limit `shouldBe` defaultListLimit
            listOffsetValue defaultDocumentListQuery.offset `shouldBe` 0

        it "parses and matches DocumentStatusFilter values" $ do
            let allFilters :: [DocumentStatusFilter]
                allFilters = [minBound .. maxBound]
            map (parseDocumentStatusFilter . toText) allFilters `shouldBe` map Right allFilters
            map (Aeson.decode . Aeson.encode) allFilters `shouldBe` map Just allFilters
            parseDocumentStatusFilter "deleted" `shouldBe` Left (InvalidDocumentStatusFilter "deleted")

            matchesDocumentStatus StatusActive Nothing `shouldBe` True
            matchesDocumentStatus StatusActive (Just sampleTime) `shouldBe` False
            matchesDocumentStatus StatusTrashed Nothing `shouldBe` False
            matchesDocumentStatus StatusTrashed (Just sampleTime) `shouldBe` True
            matchesDocumentStatus StatusAll Nothing `shouldBe` True
            matchesDocumentStatus StatusAll (Just sampleTime) `shouldBe` True

        it "validates SearchQuery, ListLimit, and ListOffset boundaries" $ do
            mkSearchQuery "" `shouldBe` Right Nothing
            mkSearchQuery "   " `shouldBe` Right Nothing
            fmap (fmap toText) (mkSearchQuery "  Haskell  ") `shouldBe` Right (Just "Haskell")
            let maxQuery :: Text
                maxQuery = Txt.replicate maxSearchQueryLength "q"
            fmap (fmap toText) (mkSearchQuery maxQuery) `shouldBe` Right (Just maxQuery)
            mkSearchQuery "bad\nquery" `shouldBe` Left SearchQueryContainsControlChar
            mkSearchQuery (Txt.replicate (maxSearchQueryLength + 1) "q")
                `shouldBe` Left (SearchQueryTooLong (maxSearchQueryLength + 1) maxSearchQueryLength)

            fmap listLimitValue (mkListLimit minListLimit) `shouldBe` Right minListLimit
            fmap listLimitValue (mkListLimit maxListLimit) `shouldBe` Right maxListLimit
            mkListLimit 0 `shouldBe` Left (ListLimitOutOfRange 0 minListLimit maxListLimit)
            mkListLimit (maxListLimit + 1)
                `shouldBe` Left (ListLimitOutOfRange (maxListLimit + 1) minListLimit maxListLimit)

            fmap listOffsetValue (mkListOffset 0) `shouldBe` Right 0
            fmap listOffsetValue (mkListOffset 100) `shouldBe` Right 100
            mkListOffset (-1) `shouldBe` Left (NegativeListOffset (-1))

        it "decodes empty object into defaultCreateDocumentRequest, rejects null on non-nullable fields, and validates body size" $ do
            Aeson.decode "{}" `shouldBe` Just defaultCreateDocumentRequest
            Aeson.decode "{\"folder\":null}" `shouldBe` Just defaultCreateDocumentRequest
            (Aeson.decode "{\"title\":null}" :: Maybe CreateDocumentRequest) `shouldBe` Nothing
            (Aeson.decode "{\"body\":null}" :: Maybe CreateDocumentRequest) `shouldBe` Nothing
            (Aeson.decode "{\"format\":null}" :: Maybe CreateDocumentRequest) `shouldBe` Nothing
            (Aeson.decode "{\"tags\":null}" :: Maybe CreateDocumentRequest) `shouldBe` Nothing
            validateCreateDocumentRequest defaultCreateDocumentRequest
                `shouldBe` Right defaultCreateDocumentRequest
            let oversizedReq :: CreateDocumentRequest
                oversizedReq = CreateDocumentRequest
                    { title  = emptyDocumentTitle
                    , body   = DocumentBody (Txt.replicate (maxDocumentBodyBytes + 1) "x")
                    , format = FormatMarkdown
                    , folder = Nothing
                    , tags   = emptyTagSet
                    }
            validateCreateDocumentRequest oversizedReq
                `shouldBe` Left (BodyTooLarge (maxDocumentBodyBytes + 1) maxDocumentBodyBytes)

        it "requires explicit fields on UpdateDocumentRequest and allows optional folder and base_revision" $ do
            let validJson :: Maybe UpdateDocumentRequest
                validJson = Aeson.decode "{\"title\":\"\",\"body\":\"updated\",\"format\":\"plain_text\",\"tags\":[\"work\"]}"
                nullBaseRevJson :: Maybe UpdateDocumentRequest
                nullBaseRevJson = Aeson.decode "{\"title\":\"\",\"body\":\"updated\",\"format\":\"plain_text\",\"tags\":[\"work\"],\"base_revision\":null}"
                withBaseRevJson :: Maybe UpdateDocumentRequest
                withBaseRevJson = Aeson.decode "{\"title\":\"\",\"body\":\"updated\",\"format\":\"plain_text\",\"tags\":[\"work\"],\"base_revision\":3}"
            case validJson of
                Nothing  -> expectationFailure "Expected valid UpdateDocumentRequest JSON to decode"
                Just req -> do
                    req.folder `shouldSatisfy` isNothing
                    req.baseRevision `shouldSatisfy` isNothing
                    validateUpdateDocumentRequest req `shouldBe` Right req
                    Aeson.decode (Aeson.encode req) `shouldBe` Just req
                    Aeson.toJSON req `shouldBe` Aeson.object
                        [ "title"  Aeson..= ("" :: Text)
                        , "body"   Aeson..= ("updated" :: Text)
                        , "format" Aeson..= ("plain_text" :: Text)
                        , "folder" Aeson..= Aeson.Null
                        , "tags"   Aeson..= (["work"] :: [Text])
                        ]
            nullBaseRevJson `shouldBe` validJson
            case withBaseRevJson of
                Nothing  -> expectationFailure "Expected UpdateDocumentRequest with base_revision to decode"
                Just req -> do
                    fmap revisionNumberValue req.baseRevision `shouldBe` Just 3
                    Aeson.decode (Aeson.encode req) `shouldBe` Just req
            (Aeson.decode "{\"title\":\"\",\"body\":\"updated\",\"format\":\"plain_text\",\"tags\":[],\"base_revision\":0}" :: Maybe UpdateDocumentRequest)
                `shouldBe` Nothing
            (Aeson.decode "{\"title\":\"only-title\"}" :: Maybe UpdateDocumentRequest) `shouldBe` Nothing

        it "roundtrips DocumentListResponse, TagListResponse, and FolderListResponse and rejects negative total" $ do
            let emptyListRes :: DocumentListResponse
                emptyListRes = DocumentListResponse
                    { documents = []
                    , total     = 0
                    }
            Aeson.decode (Aeson.encode emptyListRes) `shouldBe` Just emptyListRes
            (Aeson.decode "{\"documents\":[],\"total\":-1}" :: Maybe DocumentListResponse) `shouldBe` Nothing

            case (mkTag "work", mkFolderPath "projects/tamatsuzura") of
                (Right tag, Right fp) -> do
                    let tagRes :: TagListResponse
                        tagRes = TagListResponse { tags = [tag] }
                        folderRes :: FolderListResponse
                        folderRes = FolderListResponse { folders = [fp] }
                    Aeson.decode (Aeson.encode tagRes) `shouldBe` Just tagRes
                    Aeson.decode (Aeson.encode folderRes) `shouldBe` Just folderRes
                _ -> expectationFailure "Expected sample tag and folder path to construct successfully"

        it "encodes ApiError JSON keys accurately, omits field when Nothing, and maps ValidationError variants" $ do
            let allCodes :: [ApiErrorCode]
                allCodes = [minBound .. maxBound]
            map (parseApiErrorCode . toText) allCodes `shouldBe` map Right allCodes
            map (Aeson.decode . Aeson.encode) allCodes `shouldBe` map Just allCodes
            parseApiErrorCode "unknown" `shouldSatisfy` isLeft

            let noFieldErr :: ApiError
                noFieldErr = ApiError
                    { code    = ErrNotFound
                    , message = "Document not found"
                    , field   = Nothing
                    }
                fieldErr :: ApiError
                fieldErr = validationErrorToApiError TitleContainsControlChar
                tooLargeErr :: ApiError
                tooLargeErr = validationErrorToApiError (BodyTooLarge (maxDocumentBodyBytes + 1) maxDocumentBodyBytes)

            Aeson.toJSON noFieldErr `shouldBe` Aeson.object
                [ "error"   Aeson..= ("not_found" :: Text)
                , "message" Aeson..= ("Document not found" :: Text)
                ]
            Aeson.toJSON fieldErr `shouldBe` Aeson.object
                [ "error"   Aeson..= ("validation_error" :: Text)
                , "message" Aeson..= toText TitleContainsControlChar
                , "field"   Aeson..= ("title" :: Text)
                ]
            Aeson.decode (Aeson.encode noFieldErr) `shouldBe` Just noFieldErr
            Aeson.decode (Aeson.encode fieldErr) `shouldBe` Just fieldErr
            fieldErr.code `shouldBe` ErrValidation
            fieldErr.field `shouldBe` Just "title"
            tooLargeErr.code `shouldBe` ErrPayloadTooLarge
            tooLargeErr.field `shouldBe` Just "body"

        it "maps every ValidationError variant to its canonical API field name" $ do
            let cases :: [(ValidationError, Text)]
                cases =
                    [ (EmptyDocumentId,                         "id")
                    , (DocumentIdTooLong 65 64,                 "id")
                    , (InvalidDocumentIdChar "bad/id",          "id")
                    , (TitleTooLong 256 255,                    "title")
                    , (TitleContainsControlChar,                "title")
                    , (BodyTooLarge 10 5,                       "body")
                    , (InvalidDocumentFormat "html",            "format")
                    , (EmptyTag,                                "tags")
                    , (TagTooLong 65 64,                        "tags")
                    , (TagContainsControlChar "a\nb",           "tags")
                    , (TooManyTags 51 50,                       "tags")
                    , (EmptyFolderPath,                         "folder")
                    , (FolderPathTooLong 256 255,               "folder")
                    , (FolderPathTooDeep 11 10,                 "folder")
                    , (InvalidFolderPathSegment "..",           "folder")
                    , (InvalidDocumentStatusFilter "bad",       "status")
                    , (SearchQueryTooLong 201 200,              "q")
                    , (SearchQueryContainsControlChar,          "q")
                    , (ListLimitOutOfRange 0 1 200,             "limit")
                    , (NegativeListOffset (-1),                 "offset")
                    , (EmptyUserId,                             "user_id")
                    , (UserIdTooLong 65 64,                     "user_id")
                    , (InvalidUserIdChar "bad/uid",             "user_id")
                    , (UsernameTooShort 2 3,                    "username")
                    , (UsernameTooLong 33 32,                   "username")
                    , (InvalidUsernameFormat "-bad",            "username")
                    , (EmptyDisplayName,                        "display_name")
                    , (DisplayNameTooLong 65 64,                "display_name")
                    , (DisplayNameContainsControlChar,          "display_name")
                    , (EmptyPassword,                           "password")
                    , (PasswordTooShort 4 8,                    "password")
                    , (PasswordTooLong 129 128,                 "password")
                    , (PasswordContainsNullChar,                "password")
                    , (InvalidCsrfToken,                        "csrf_token")
                    , (EmptyRevisionId,                         "revision_id")
                    , (RevisionIdTooLong 65 64,                 "revision_id")
                    , (InvalidRevisionIdChar "bad/rev",         "revision_id")
                    , (InvalidRevisionNumber 0,                 "revision")
                    , (InvalidContentHash "short",              "content_hash")
                    ]
            map (validationErrorField . fst) cases `shouldBe` map snd cases

    describe "TagSet direct construction" $
        it "returns emptyTagSet for empty input list" $ do
            let emptyInput :: [Tag]
                emptyInput = []
            mkTagSet emptyInput `shouldBe` Right emptyTagSet
            emptyDocumentBody `shouldBe` DocumentBody ""

    describe "Authentication domain types and DTOs" $ do
        it "validates UserId boundaries and JSON roundtrip" $ do
            fmap toText (mkUserId "8f14e45f-ea9a-4f3b-8a6d-123456789abc")
                `shouldBe` Right "8f14e45f-ea9a-4f3b-8a6d-123456789abc"
            let maxUid :: Text
                maxUid = Txt.replicate maxUserIdLength "u"
            fmap toText (mkUserId maxUid) `shouldBe` Right maxUid
            mkUserId "" `shouldBe` Left EmptyUserId
            mkUserId (Txt.replicate (maxUserIdLength + 1) "u")
                `shouldBe` Left (UserIdTooLong (maxUserIdLength + 1) maxUserIdLength)
            mkUserId "user/1" `shouldBe` Left (InvalidUserIdChar "user/1")
            (Aeson.decode "\"user/1\"" :: Maybe UserId) `shouldBe` Nothing

        it "normalizes Username to lowercase ASCII and validates format boundaries" $ do
            fmap toText (mkUsername "  Alice_01-dev  ") `shouldBe` Right "alice_01-dev"
            mkUsername "ab" `shouldBe` Left (UsernameTooShort 2 minUsernameLength)
            mkUsername (Txt.replicate (maxUsernameLength + 1) "a")
                `shouldBe` Left (UsernameTooLong (maxUsernameLength + 1) maxUsernameLength)
            mkUsername "_alice" `shouldBe` Left (InvalidUsernameFormat "_alice")
            mkUsername "-alice" `shouldBe` Left (InvalidUsernameFormat "-alice")
            mkUsername "alice.dev" `shouldBe` Left (InvalidUsernameFormat "alice.dev")
            mkUsername "ありす" `shouldBe` Left (InvalidUsernameFormat "ありす")
            mkUsername "alice\n01" `shouldBe` Left (InvalidUsernameFormat "alice\n01")
            (Aeson.decode "\"_invalid\"" :: Maybe Username) `shouldBe` Nothing

        it "satisfies Username normalization idempotency and JSON roundtrip property" $
            forAll genValidUsernameText $ \ rawName ->
                case mkUsername rawName of
                    Left _      -> False
                    Right uname ->
                        mkUsername (toText uname) == Right uname
                            && mkUsername (" " <> rawName <> " ") == Right uname
                            && Aeson.decode (Aeson.encode uname) == Just uname

        it "validates optional DisplayName with trimming, control-char rejection, and length limits" $ do
            fmap toText (mkDisplayName "  アリス  ") `shouldBe` Right "アリス"
            mkDisplayName "" `shouldBe` Left EmptyDisplayName
            mkDisplayName "   " `shouldBe` Left EmptyDisplayName
            mkDisplayName "line1\nline2" `shouldBe` Left DisplayNameContainsControlChar
            let maxName :: Text
                maxName = Txt.replicate maxDisplayNameLength "あ"
            fmap toText (mkDisplayName maxName) `shouldBe` Right maxName
            mkDisplayName (Txt.replicate (maxDisplayNameLength + 1) "あ")
                `shouldBe` Left (DisplayNameTooLong (maxDisplayNameLength + 1) maxDisplayNameLength)
            (Aeson.decode "\"\"" :: Maybe DisplayName) `shouldBe` Nothing

        it "validates Password, redacts Show output, preserves whitespace, and enforces signup min length" $ do
            mkPassword "" `shouldBe` Left EmptyPassword
            mkPassword "abc\0defg" `shouldBe` Left PasswordContainsNullChar
            mkPassword (Txt.replicate (maxPasswordBytes + 1) "a")
                `shouldBe` Left (PasswordTooLong (maxPasswordBytes + 1) maxPasswordBytes)
            let multibyteOver :: Text
                multibyteOver = Txt.replicate ((maxPasswordBytes `div` 3) + 1) "あ"
            mkPassword multibyteOver
                `shouldBe` Left (PasswordTooLong (((maxPasswordBytes `div` 3) + 1) * 3) maxPasswordBytes)

            -- ログイン用に 8 文字未満も mkPassword 自体は通しつつ、validateNewPassword で拒否する
            case mkPassword " short " of
                Left err -> expectationFailure ("Expected short password to construct for login, got: " <> show err)
                Right pw -> do
                    unsafePasswordText pw `shouldBe` " short "
                    passwordCharLength pw `shouldBe` 7
                    passwordByteLength pw `shouldBe` 7
                    show pw `shouldBe` "<redacted>"
                    validateNewPassword pw `shouldBe` Left (PasswordTooShort 7 minPasswordLength)

            case mkPassword "correct-horse-battery" of
                Left err -> expectationFailure ("Expected valid password, got: " <> show err)
                Right pw -> do
                    validateNewPassword pw `shouldBe` Right pw
                    show pw `shouldBe` "<redacted>"
                    Aeson.decode (Aeson.encode pw) `shouldBe` Just pw
            -- 正当境界値: ちょうど 8 文字およびちょうど 128 UTF-8 バイト
            case mkPassword (Txt.replicate minPasswordLength "p") of
                Left err -> expectationFailure ("Expected 8-char password to succeed, got: " <> show err)
                Right pw -> validateNewPassword pw `shouldBe` Right pw
            case mkPassword (Txt.replicate maxPasswordBytes "a") of
                Left err -> expectationFailure ("Expected 128-byte password to succeed, got: " <> show err)
                Right pw -> do
                    passwordByteLength pw `shouldBe` maxPasswordBytes
                    validateNewPassword pw `shouldBe` Right pw
            (Aeson.decode "\"\"" :: Maybe Password) `shouldBe` Nothing
            (Aeson.decode "\"abc\\u0000def\"" :: Maybe Password) `shouldBe` Nothing

        it "validates CsrfToken boundaries, URL-safe alphabet, and redacts Show" $ do
            let minToken :: Text
                minToken = Txt.replicate minCsrfTokenLength "A"
                maxToken :: Text
                maxToken = Txt.replicate maxCsrfTokenLength "z"
            fmap toText (mkCsrfToken minToken) `shouldBe` Right minToken
            fmap toText (mkCsrfToken maxToken) `shouldBe` Right maxToken
            case (mkCsrfToken minToken, mkCsrfToken (Txt.replicate minCsrfTokenLength "B")) of
                (Right tokA, Right tokB) -> do
                    show tokA `shouldBe` "<CsrfToken>"
                    (tokA == tokA) `shouldBe` True
                    (tokA == tokB) `shouldBe` False
                _ -> expectationFailure "Expected valid CsrfToken samples"
            mkCsrfToken (Txt.replicate (minCsrfTokenLength - 1) "A") `shouldBe` Left InvalidCsrfToken
            mkCsrfToken (Txt.replicate (maxCsrfTokenLength + 1) "A") `shouldBe` Left InvalidCsrfToken
            mkCsrfToken (Txt.replicate minCsrfTokenLength "!") `shouldBe` Left InvalidCsrfToken
            (Aeson.decode "\"too-short\"" :: Maybe CsrfToken) `shouldBe` Nothing

        it "encodes and decodes UserProfile, RegisterRequest, LoginRequest, and AuthSessionResponse matching OpenAPI" $ do
            case (mkUserId "user-1", mkUsername "Alice_01", mkDisplayName "アリス", mkPassword "password123", mkPassword "short77", mkCsrfToken (Txt.replicate 43 "a")) of
                (Right uid, Right uname, Right dname, Right pw, Right shortPw, Right csrf) -> do
                    let profile :: UserProfile
                        profile = UserProfile
                            { id          = uid
                            , username    = uname
                            , displayName = Just dname
                            , createdAt   = sampleTime
                            }
                        profileNoDisplay :: UserProfile
                        profileNoDisplay = UserProfile
                            { id          = uid
                            , username    = uname
                            , displayName = Nothing
                            , createdAt   = sampleTime
                            }
                        sessionRes :: AuthSessionResponse
                        sessionRes = AuthSessionResponse
                            { user      = profile
                            , csrfToken = csrf
                            }
                        regReq :: RegisterRequest
                        regReq = RegisterRequest
                            { username    = uname
                            , displayName = Just dname
                            , password    = pw
                            }
                        shortRegReq :: RegisterRequest
                        shortRegReq = RegisterRequest
                            { username    = uname
                            , displayName = Nothing
                            , password    = shortPw
                            }
                        loginReq :: LoginRequest
                        loginReq = LoginRequest
                            { username = uname
                            , password = pw
                            }
                    Aeson.toJSON profile `shouldBe` Aeson.object
                        [ "id"           Aeson..= ("user-1" :: Text)
                        , "username"     Aeson..= ("alice_01" :: Text)
                        , "display_name" Aeson..= ("アリス" :: Text)
                        , "created_at"   Aeson..= sampleTime
                        ]
                    Aeson.toJSON profileNoDisplay `shouldBe` Aeson.object
                        [ "id"           Aeson..= ("user-1" :: Text)
                        , "username"     Aeson..= ("alice_01" :: Text)
                        , "display_name" Aeson..= Aeson.Null
                        , "created_at"   Aeson..= sampleTime
                        ]
                    Aeson.toJSON sessionRes `shouldBe` Aeson.object
                        [ "user"       Aeson..= profile
                        , "csrf_token" Aeson..= toText csrf
                        ]
                    Aeson.decode (Aeson.encode profile) `shouldBe` Just profile
                    Aeson.decode (Aeson.encode profileNoDisplay) `shouldBe` Just profileNoDisplay
                    Aeson.decode (Aeson.encode sessionRes) `shouldBe` Just sessionRes
                    Aeson.decode (Aeson.encode regReq) `shouldBe` Just regReq
                    Aeson.decode (Aeson.encode loginReq) `shouldBe` Just loginReq
                    validateRegisterRequest regReq `shouldBe` Right regReq
                    validateRegisterRequest shortRegReq `shouldBe` Left (PasswordTooShort 7 minPasswordLength)

                    -- UserProfile の必須 nullable キー display_name が欠落した JSON は拒否される
                    let missingDisplayKey :: Aeson.Value
                        missingDisplayKey = Aeson.object
                            [ "id"         Aeson..= ("user-1" :: Text)
                            , "username"   Aeson..= ("alice_01" :: Text)
                            , "created_at" Aeson..= sampleTime
                            ]
                    (Aeson.decode (Aeson.encode missingDisplayKey) :: Maybe UserProfile) `shouldBe` Nothing

                    -- display_name 省略・null は Nothing として解釈される
                    let omittedDisplay :: Maybe RegisterRequest
                        omittedDisplay = Aeson.decode "{\"username\":\"Alice_01\",\"password\":\"password123\"}"
                        nullDisplay :: Maybe RegisterRequest
                        nullDisplay = Aeson.decode "{\"username\":\"Alice_01\",\"display_name\":null,\"password\":\"password123\"}"
                    fmap (\ r -> r.displayName) omittedDisplay `shouldBe` Just Nothing
                    fmap (\ r -> r.displayName) nullDisplay `shouldBe` Just Nothing

                    -- Show 出力に平文パスワードおよび生 CSRF トークンが含まれない
                    Txt.isInfixOf "password123" (Txt.pack (show regReq)) `shouldBe` False
                    Txt.isInfixOf "password123" (Txt.pack (show loginReq)) `shouldBe` False
                    Txt.isInfixOf (toText csrf) (Txt.pack (show sessionRes)) `shouldBe` False
                _ -> expectationFailure "Expected sample auth DTO components to construct successfully"

    describe "Unified diff generation, parsing, application, and reversal" $ do
        it "returns emptyUnifiedDiff for identical texts and applies it as identity" $ do
            generateUnifiedDiff "" "" `shouldBe` emptyUnifiedDiff
            generateUnifiedDiff "hello\nworld\n" "hello\nworld\n" `shouldBe` emptyUnifiedDiff
            applyUnifiedDiff "hello\nworld\n" emptyUnifiedDiff `shouldBe` Right "hello\nworld\n"
            applyUnifiedDiffReverse "hello\nworld\n" emptyUnifiedDiff `shouldBe` Right "hello\nworld\n"

        it "handles empty-to-non-empty, non-empty-to-empty, and single-newline edge cases" $ do
            let cases :: [(Text, Text)]
                cases =
                    [ ("", "hello\n")
                    , ("hello\n", "")
                    , ("", "hello")
                    , ("hello", "")
                    , ("", "\n")
                    , ("\n", "")
                    , ("a", "a\n")
                    , ("a\n", "a")
                    , ("a\r\nb\r\n", "a\r\nc\r\n")
                    , ("日本語\nテスト\n", "日本語\n改訂テスト\n追記")
                    , ("line1\n\\ No newline at end of file\n", "line1\n\\ No newline at end of file")
                    ]
                verifyPair :: (Text, Text) -> IO ()
                verifyPair (oldTxt, newTxt) = do
                    let diff :: UnifiedDiff
                        diff = generateUnifiedDiff oldTxt newTxt
                    applyUnifiedDiff oldTxt diff `shouldBe` Right newTxt
                    applyUnifiedDiffReverse newTxt diff `shouldBe` Right oldTxt
            mapM_ verifyPair cases

        it "splits distant edits into separate hunks and merges nearby edits within 2*context lines" $ do
            let baseLines :: [Text]
                baseLines = map (\ n -> "line-" <> Txt.pack (show n)) [1 :: Int .. 20]
                oldTxt :: Text
                oldTxt = Txt.unlines baseLines
                distantNew :: Text
                distantNew = Txt.unlines $
                    ["line-1-edited"] <> drop 1 (take 19 baseLines) <> ["line-20-edited"]
                nearbyNew :: Text
                nearbyNew = Txt.unlines $
                    ["line-1-edited"] <> drop 1 (take 6 baseLines) <> ["line-7-edited"] <> drop 7 baseLines
                distantParsed :: ParsedDiff
                distantParsed = generateDiff oldTxt distantNew
                nearbyParsed :: ParsedDiff
                nearbyParsed = generateDiff oldTxt nearbyNew
            defaultContextLines `shouldBe` 3
            length distantParsed.hunks `shouldBe` 2
            length nearbyParsed.hunks `shouldBe` 1
            generateDiffWithContext (-1) oldTxt distantNew `shouldBe` generateDiffWithContext 0 oldTxt distantNew
            applyParsedDiff oldTxt distantParsed `shouldBe` Right distantNew
            applyParsedDiff oldTxt nearbyParsed `shouldBe` Right nearbyNew

        it "parses external unified diff with file headers and optional hunk heading text" $ do
            let externalDiff :: UnifiedDiff
                externalDiff = UnifiedDiff
                    "--- a/note.md\n\
                    \+++ b/note.md\n\
                    \@@ -1,3 +1,3 @@ section heading\n\
                    \ line1\n\
                    \-line2\n\
                    \+line2-updated\n\
                    \ line3\n"
            applyUnifiedDiff "line1\nline2\nline3\n" externalDiff
                `shouldBe` Right "line1\nline2-updated\nline3\n"

        it "reports structured DiffError on malformed headers, bad prefixes, count mismatch, overlap, offset mismatch, and newline violations" $ do
            parseUnifiedDiff (UnifiedDiff "not a diff header\n")
                `shouldBe` Left (UnexpectedDiffLine 1 "not a diff header")
            parseUnifiedDiff (UnifiedDiff "--- a/only-one-header\n@@ -1 +1 @@\n-a\n+b\n")
                `shouldBe` Left (UnexpectedDiffLine 1 "--- a/only-one-header")
            parseUnifiedDiff (UnifiedDiff "@@ -bad +1 @@\n")
                `shouldBe` Left (MalformedHunkHeader 1 "@@ -bad +1 @@")
            parseUnifiedDiff (UnifiedDiff "@@ -0,1 +1,1 @@\n-a\n+b\n")
                `shouldBe` Left (MalformedHunkHeader 1 "@@ -0,1 +1,1 @@")
            parseUnifiedDiff (UnifiedDiff "@@ -0,0 +0,0 @@\n")
                `shouldBe` Left (MalformedHunkHeader 1 "@@ -0,0 +0,0 @@")
            parseUnifiedDiff (UnifiedDiff "@@ -4294967297,1 +1,1 @@\n-a\n+b\n")
                `shouldBe` Left (MalformedHunkHeader 1 "@@ -4294967297,1 +1,1 @@")
            parseUnifiedDiff (UnifiedDiff "@@ -1 +1 @@\n\\ No newline at end of file\n")
                `shouldBe` Left (UnexpectedNoNewlineMarker 2)
            parseUnifiedDiff (UnifiedDiff "@@ -1 +1 @@\n-a\n\\ No newline at end of file\n\\ No newline at end of file\n+b\n")
                `shouldBe` Left (UnexpectedNoNewlineMarker 4)
            parseUnifiedDiff (UnifiedDiff "@@ -1,2 +1,1 @@\n-only-one\n")
                `shouldBe` Left (HunkLineCountMismatch 1 2 1)
            parseUnifiedDiff (UnifiedDiff "@@ -1,1 +1,2 @@\n-old\n+only-one-new\n")
                `shouldBe` Left (HunkLineCountMismatch 1 2 1)
            parseUnifiedDiff (UnifiedDiff "@@ -2,2 +2,2 @@\n a\n b\n@@ -3,1 +3,1 @@\n c\n")
                `shouldBe` Left (OverlappingHunks 2)
            parseUnifiedDiff (UnifiedDiff "@@ -1,1 +1,1 @@\n-a\n+X\n@@ -2,1 +1,1 @@\n-b\n+Y\n")
                `shouldBe` Left (OverlappingHunks 2)
            parseUnifiedDiff (UnifiedDiff "@@ -1,1 +5,1 @@\n-a\n+b\n")
                `shouldBe` Left (HunkOffsetMismatch 1 0 4)
            -- ファイル末尾以外の行に \ No newline at end of file が付いた場合は拒否する
            parseUnifiedDiff (UnifiedDiff "@@ -1,1 +1,2 @@\n-old\n+new1\n\\ No newline at end of file\n+new2\n")
                `shouldBe` Left (HunkNewlineMismatch 1 2)
            case applyUnifiedDiff "a\nb\n" (UnifiedDiff "@@ -1,1 +1,1 @@\n-a\n+X\n\\ No newline at end of file\n") of
                Left (HunkNewlineMismatch 1 2) -> pure ()
                other -> expectationFailure ("Expected HunkNewlineMismatch on trailing lines after no-newline, got: " <> show other)

            let validDiff :: UnifiedDiff
                validDiff = generateUnifiedDiff "a\nb\nc\n" "a\nB\nc\n"
            case applyUnifiedDiff "a\nX\nc\n" validDiff of
                Left (HunkContextMismatch 1 2 "b" "X") -> pure ()
                other -> expectationFailure ("Expected HunkContextMismatch, got: " <> show other)
            case applyUnifiedDiff "a\nb\nc" validDiff of
                Left (HunkNewlineMismatch 1 3) -> pure ()
                other -> expectationFailure ("Expected HunkNewlineMismatch, got: " <> show other)
            case applyUnifiedDiff "only-one\n" validDiff of
                Left HunkOutOfRange{} -> pure ()
                other -> expectationFailure ("Expected HunkOutOfRange, got: " <> show other)

        it "handles large documents with localized edits and full-replacement Myers fallback reversibly" $ do
            let largeOld :: Text
                largeOld = Txt.unlines (map (\ n -> "row-" <> Txt.pack (show n)) [1 :: Int .. 5000])
                largeNew :: Text
                largeNew = Txt.unlines
                    ( map (\ n -> "row-" <> Txt.pack (show n)) [1 :: Int .. 2500]
                        <> ["inserted-middle-row"]
                        <> map (\ n -> "row-" <> Txt.pack (show n)) [2501 :: Int .. 5000]
                    )
                diff :: UnifiedDiff
                diff = generateUnifiedDiff largeOld largeNew
            applyUnifiedDiff largeOld diff `shouldBe` Right largeNew
            applyUnifiedDiffReverse largeNew diff `shouldBe` Right largeOld

            -- 2500 行 × 2500 行の完全置換 (編集距離 5000 > maxMyersEditDistance = 4096) のフォールバック経路
            let fullOld :: Text
                fullOld = Txt.unlines (map (\ n -> "old-" <> Txt.pack (show n)) [1 :: Int .. 2500])
                fullNew :: Text
                fullNew = Txt.unlines (map (\ n -> "new-" <> Txt.pack (show n)) [1 :: Int .. 2500])
                fullDiff :: UnifiedDiff
                fullDiff = generateUnifiedDiff fullOld fullNew
            applyUnifiedDiff fullOld fullDiff `shouldBe` Right fullNew
            applyUnifiedDiffReverse fullNew fullDiff `shouldBe` Right fullOld

        it "satisfies forward/reverse roundtrip and parse/render roundtrip for arbitrary texts (QuickCheck)" $
            forAll ((,) <$> genDiffText <*> genDiffText) $ \ (oldTxt, newTxt) ->
                let parsed :: ParsedDiff
                    parsed = generateDiff oldTxt newTxt
                    rendered :: UnifiedDiff
                    rendered = renderUnifiedDiff parsed
                in  parseUnifiedDiff rendered == Right parsed
                        && applyUnifiedDiff oldTxt rendered == Right newTxt
                        && applyUnifiedDiffReverse newTxt rendered == Right oldTxt
                        && reverseParsedDiff (reverseParsedDiff parsed) == parsed
                        && case reverseUnifiedDiff rendered of
                            Left _    -> False
                            Right rev -> applyUnifiedDiff newTxt rev == Right oldTxt

        it "satisfies roundtrip across arbitrary context line counts 0..5 (QuickCheck)" $
            forAll ((,,) <$> chooseInt (0, 5) <*> genDiffText <*> genDiffText) $ \ (ctx, oldTxt, newTxt) ->
                let diff :: UnifiedDiff
                    diff = generateUnifiedDiffWithContext ctx oldTxt newTxt
                in  applyUnifiedDiff oldTxt diff == Right newTxt
                        && applyUnifiedDiffReverse newTxt diff == Right oldTxt

        it "reconstructs every step of a 3-step revision diff chain both forward and backward (QuickCheck)" $
            forAll ((,,,) <$> genDiffText <*> genDiffText <*> genDiffText <*> genDiffText) $ \ (v0, v1, v2, v3) ->
                let d1 :: UnifiedDiff
                    d1 = generateUnifiedDiff v0 v1
                    d2 :: UnifiedDiff
                    d2 = generateUnifiedDiff v1 v2
                    d3 :: UnifiedDiff
                    d3 = generateUnifiedDiff v2 v3
                    fwd :: Either DiffError Text
                    fwd = applyUnifiedDiff v0 d1 >>= (`applyUnifiedDiff` d2) >>= (`applyUnifiedDiff` d3)
                    bwd :: Either DiffError Text
                    bwd = applyUnifiedDiffReverse v3 d3 >>= (`applyUnifiedDiffReverse` d2) >>= (`applyUnifiedDiffReverse` d1)
                in  fwd == Right v3 && bwd == Right v0

    describe "Revision domain types and DTOs" $ do
        it "validates RevisionId, RevisionNumber, and ContentHash boundaries and JSON" $ do
            fmap toText (mkRevisionId "rev_01-abc") `shouldBe` Right "rev_01-abc"
            let maxRevId :: Text
                maxRevId = Txt.replicate maxRevisionIdLength "r"
            fmap toText (mkRevisionId maxRevId) `shouldBe` Right maxRevId
            mkRevisionId "" `shouldBe` Left EmptyRevisionId
            mkRevisionId (Txt.replicate (maxRevisionIdLength + 1) "r")
                `shouldBe` Left (RevisionIdTooLong (maxRevisionIdLength + 1) maxRevisionIdLength)
            mkRevisionId "rev/1" `shouldBe` Left (InvalidRevisionIdChar "rev/1")
            (Aeson.decode "\"rev/1\"" :: Maybe RevisionId) `shouldBe` Nothing

            revisionNumberValue initialRevisionNumber `shouldBe` minRevisionNumber
            revisionNumberValue (nextRevisionNumber initialRevisionNumber) `shouldBe` 2
            fmap revisionNumberValue (mkRevisionNumber 1) `shouldBe` Right 1
            mkRevisionNumber 0 `shouldBe` Left (InvalidRevisionNumber 0)
            mkRevisionNumber (-5) `shouldBe` Left (InvalidRevisionNumber (-5))
            (Aeson.decode "0" :: Maybe RevisionNumber) `shouldBe` Nothing
            Aeson.decode (Aeson.encode initialRevisionNumber) `shouldBe` Just initialRevisionNumber

            let validHash :: Text
                validHash = Txt.replicate contentHashHexLength "a"
                upperHash :: Text
                upperHash = Txt.replicate contentHashHexLength "A"
                nonHexHash :: Text
                nonHexHash = Txt.replicate contentHashHexLength "g"
            fmap toText (mkContentHash validHash) `shouldBe` Right validHash
            mkContentHash upperHash `shouldBe` Left (InvalidContentHash upperHash)
            mkContentHash nonHexHash `shouldBe` Left (InvalidContentHash nonHexHash)
            mkContentHash "deadbeef" `shouldBe` Left (InvalidContentHash "deadbeef")
            (Aeson.decode "\"deadbeef\"" :: Maybe ContentHash) `shouldBe` Nothing
            (Aeson.decode (Aeson.encode upperHash) :: Maybe ContentHash) `shouldBe` Nothing
            (Aeson.decode (Aeson.encode nonHexHash) :: Maybe ContentHash) `shouldBe` Nothing

        it "encodes and decodes RevisionSummary, RevisionDetail, RevisionListResponse, and RestoreRevisionRequest matching OpenAPI" $ do
            let validHash :: Text
                validHash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            case (mkRevisionId "rev-2", mkRevisionId "rev-1", mkDocumentId "doc-1", mkRevisionNumber 2, mkDocumentTitle "第2版", mkContentHash validHash) of
                (Right revId, Right parentId, Right docId, Right revNum, Right revTitle, Right hashVal) -> do
                    let sampleDiff :: UnifiedDiff
                        sampleDiff = generateUnifiedDiff "old\n" "new\n"
                        detail :: RevisionDetail
                        detail = RevisionDetail
                            { id               = revId
                            , documentId       = docId
                            , revision         = revNum
                            , parentRevisionId = Just parentId
                            , title            = revTitle
                            , body             = DocumentBody "new\n"
                            , format           = FormatMarkdown
                            , diff             = sampleDiff
                            , contentHash      = hashVal
                            , createdAt        = sampleLaterTime
                            }
                        summary :: RevisionSummary
                        summary = toRevisionSummary detail
                        listRes :: RevisionListResponse
                        listRes = RevisionListResponse
                            { revisions = [summary]
                            , total     = 2
                            }
                        restoreReq :: RestoreRevisionRequest
                        restoreReq = RestoreRevisionRequest
                            { baseRevision = Just revNum
                            }
                    Aeson.toJSON summary `shouldBe` Aeson.object
                        [ "id"                 Aeson..= ("rev-2" :: Text)
                        , "document_id"        Aeson..= ("doc-1" :: Text)
                        , "revision"           Aeson..= (2 :: Int)
                        , "parent_revision_id" Aeson..= ("rev-1" :: Text)
                        , "title"              Aeson..= ("第2版" :: Text)
                        , "format"             Aeson..= ("markdown" :: Text)
                        , "content_hash"       Aeson..= validHash
                        , "created_at"         Aeson..= sampleLaterTime
                        ]
                    Aeson.toJSON detail `shouldBe` Aeson.object
                        [ "id"                 Aeson..= ("rev-2" :: Text)
                        , "document_id"        Aeson..= ("doc-1" :: Text)
                        , "revision"           Aeson..= (2 :: Int)
                        , "parent_revision_id" Aeson..= ("rev-1" :: Text)
                        , "title"              Aeson..= ("第2版" :: Text)
                        , "body"               Aeson..= ("new\n" :: Text)
                        , "format"             Aeson..= ("markdown" :: Text)
                        , "diff"               Aeson..= toText sampleDiff
                        , "content_hash"       Aeson..= validHash
                        , "created_at"         Aeson..= sampleLaterTime
                        ]
                    Aeson.decode (Aeson.encode summary) `shouldBe` Just summary
                    Aeson.decode (Aeson.encode detail) `shouldBe` Just detail
                    Aeson.decode (Aeson.encode listRes) `shouldBe` Just listRes
                    (Aeson.decode "{\"revisions\":[],\"total\":-1}" :: Maybe RevisionListResponse) `shouldBe` Nothing

                    Aeson.decode "{}" `shouldBe` Just defaultRestoreRevisionRequest
                    Aeson.decode "{\"base_revision\":null}" `shouldBe` Just defaultRestoreRevisionRequest
                    Aeson.encode defaultRestoreRevisionRequest `shouldBe` "{}"
                    Aeson.decode (Aeson.encode restoreReq) `shouldBe` Just restoreReq
                    (Aeson.decode "{\"base_revision\":0}" :: Maybe RestoreRevisionRequest) `shouldBe` Nothing

                    -- 初期リビジョン (parent_revision_id = null) の往復
                    let initialDetail :: RevisionDetail
                        initialDetail = RevisionDetail
                            { id               = parentId
                            , documentId       = docId
                            , revision         = initialRevisionNumber
                            , parentRevisionId = Nothing
                            , title            = emptyDocumentTitle
                            , body             = DocumentBody "old\n"
                            , format           = FormatMarkdown
                            , diff             = generateUnifiedDiff "" "old\n"
                            , contentHash      = hashVal
                            , createdAt        = sampleTime
                            }
                    Aeson.decode (Aeson.encode initialDetail) `shouldBe` Just initialDetail

                    -- 必須 nullable キー parent_revision_id が欠落した JSON は Summary / Detail 双方で拒否される
                    let missingParentKey :: Aeson.Value
                        missingParentKey = Aeson.object
                            [ "id"           Aeson..= ("rev-2" :: Text)
                            , "document_id"  Aeson..= ("doc-1" :: Text)
                            , "revision"     Aeson..= (2 :: Int)
                            , "title"        Aeson..= ("第2版" :: Text)
                            , "body"         Aeson..= ("new\n" :: Text)
                            , "format"       Aeson..= ("markdown" :: Text)
                            , "diff"         Aeson..= toText sampleDiff
                            , "content_hash" Aeson..= validHash
                            , "created_at"   Aeson..= sampleLaterTime
                            ]
                    (Aeson.decode (Aeson.encode missingParentKey) :: Maybe RevisionSummary) `shouldBe` Nothing
                    (Aeson.decode (Aeson.encode missingParentKey) :: Maybe RevisionDetail) `shouldBe` Nothing
                _ -> expectationFailure "Expected sample revision DTO components to construct successfully"

        it "canonicalizes (title, format, body) injectively for content hashing (QuickCheck)" $
            property $ \ t1Raw t2Raw isMd1 isMd2 b1Raw b2Raw ->
                let cleanTitle :: String -> Either ValidationError DocumentTitle
                    cleanTitle s = mkDocumentTitle (Txt.take 32 (Txt.filter (not . Char.isControl) (Txt.pack s)))
                    f1 :: DocumentFormat
                    f1 = if isMd1 then FormatMarkdown else FormatPlainText
                    f2 :: DocumentFormat
                    f2 = if isMd2 then FormatMarkdown else FormatPlainText
                    b1 :: DocumentBody
                    b1 = DocumentBody (Txt.pack b1Raw)
                    b2 :: DocumentBody
                    b2 = DocumentBody (Txt.pack b2Raw)
                in  case (cleanTitle t1Raw, cleanTitle t2Raw) of
                        (Right t1, Right t2) ->
                            (revisionContentCanonicalText t1 f1 b1 == revisionContentCanonicalText t2 f2 b2)
                                == ((t1, f1, b1) == (t2, f2, b2))
                        _ -> False
