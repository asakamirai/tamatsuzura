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
import           TamaTsuzura.Common.Document
    ( Document(..)
    , DocumentBody(..)
    , DocumentFormat(..)
    , DocumentId
    , DocumentSummary(..)
    , DocumentTitle
    , FolderPath
    , Tag
    , TagSet
    , defaultDocumentFormat
    , documentBodyByteLength
    , emptyDocumentBody
    , emptyDocumentTitle
    , emptyTagSet
    , folderParent
    , folderSegments
    , isFolderPrefixOf
    , isSummaryTrashed
    , isTrashed
    , memberTag
    , mkDocumentBody
    , mkDocumentId
    , mkDocumentTitle
    , mkFolderPath
    , mkTag
    , mkTagSet
    , mkTagSetFromTexts
    , parseDocumentFormat
    , tagSetSize
    , tagSetToList
    , toDocumentSummary
    )
import           TamaTsuzura.Common.Text       (ToText(..))
import           TamaTsuzura.Common.Validation
    ( ValidationError(..)
    , defaultListLimit
    , maxDocumentBodyBytes
    , maxDocumentIdLength
    , maxDocumentTitleLength
    , maxFolderDepth
    , maxFolderPathLength
    , maxListLimit
    , maxSearchQueryLength
    , maxTagLength
    , maxTagsPerDocument
    , minListLimit
    , validationErrorField
    )
import           Test.Hspec                    (Spec, describe, expectationFailure, hspec, it, shouldBe, shouldSatisfy)
import           Test.QuickCheck               (Gen, chooseInt, elements, forAll, listOf1, property, vectorOf)

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

        it "requires explicit fields on UpdateDocumentRequest and allows null or omitted folder" $ do
            let validJson :: Maybe UpdateDocumentRequest
                validJson = Aeson.decode "{\"title\":\"\",\"body\":\"updated\",\"format\":\"plain_text\",\"tags\":[\"work\"]}"
            case validJson of
                Nothing  -> expectationFailure "Expected valid UpdateDocumentRequest JSON to decode"
                Just req -> do
                    req.folder `shouldSatisfy` isNothing
                    validateUpdateDocumentRequest req `shouldBe` Right req
                    Aeson.decode (Aeson.encode req) `shouldBe` Just req
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
                    ]
            map (validationErrorField . fst) cases `shouldBe` map snd cases

    describe "TagSet direct construction" $
        it "returns emptyTagSet for empty input list" $ do
            let emptyInput :: [Tag]
                emptyInput = []
            mkTagSet emptyInput `shouldBe` Right emptyTagSet
            emptyDocumentBody `shouldBe` DocumentBody ""
