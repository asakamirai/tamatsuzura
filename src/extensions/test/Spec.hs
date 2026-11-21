{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main
    ( main
    ) where

import           Data.List                                 (sort)
import qualified Data.Set                                  as Set
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Txt
import           TamaTsuzura.Common.Document               (DocumentBody(..), DocumentFormat(..))
import           TamaTsuzura.Common.Text                   (ToText(..))
import           TamaTsuzura.ExtensionApi
    ( CommandId(..)
    , Extension(..)
    , ExtensionContext(..)
    , ExtensionError(..)
    , ExtensionResult(..)
    , runExtensionCommand
    , validateExtensionRegistry
    )
import           TamaTsuzura.Extensions                    (allBundledExtensions)
import           TamaTsuzura.Extensions.MarkdownFormatter
    ( formatMarkdownBody
    , insertOrUpdateToc
    , markdownFormatterExtension
    )
import           TamaTsuzura.Extensions.TextTransform
    ( dedupeDocumentLines
    , normalizeFullwidthAlnum
    , sortDocumentLines
    , textTransformExtension
    )
import           TamaTsuzura.Extensions.TrimTrailingSpaces
    ( normalizeBlankLines
    , trimTrailingSpaces
    , trimTrailingSpacesExtension
    )
import           Test.Hspec                                (Spec, describe, hspec, it, shouldBe, shouldSatisfy)
import           Test.QuickCheck                           (property)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "allBundledExtensions registry invariants" $
        it "passes validateExtensionRegistry with unique IDs and non-empty commands" $
            fmap length (validateExtensionRegistry allBundledExtensions) `shouldBe` Right 3

    describe "trimTrailingSpacesExtension" $ do
        it "removes trailing spaces and tabs, normalizes CRLF, and handles empty/newline boundaries" $ do
            let input :: DocumentBody
                input = DocumentBody "hello   \r\nworld\t \r\n"
                expected :: DocumentBody
                expected = DocumentBody "hello\nworld\n"
            trimTrailingSpacesExtension.transformBuffer input `shouldBe` expected
            fmap (.nextBody) (runExtensionCommand trimTrailingSpacesExtension (CommandId "trim") (ExtensionContext input FormatPlainText))
                `shouldBe` Right expected
            trimTrailingSpaces (DocumentBody "") `shouldBe` DocumentBody ""
            trimTrailingSpaces (DocumentBody "\n") `shouldBe` DocumentBody "\n"
            trimTrailingSpaces (DocumentBody "no-nl  ") `shouldBe` DocumentBody "no-nl"

        it "collapses excessive blank lines via normalize-blank-lines command including CRLF" $ do
            let ctx :: ExtensionContext
                ctx = ExtensionContext
                    { body   = DocumentBody "a\r\n\r\n\r\n\r\nb\r\n\r\n\r\n"
                    , format = FormatPlainText
                    }
            fmap (.nextBody) (runExtensionCommand trimTrailingSpacesExtension (CommandId "normalize-blank-lines") ctx)
                `shouldBe` Right (DocumentBody "a\n\nb\n")
            normalizeBlankLines (DocumentBody "") `shouldBe` DocumentBody ""
            normalizeBlankLines (DocumentBody "\n\n") `shouldBe` DocumentBody ""

        it "is idempotent for arbitrary text" $
            property $ \ str ->
                let body :: DocumentBody
                    body = DocumentBody (Txt.pack str)
                in  trimTrailingSpaces (trimTrailingSpaces body) == trimTrailingSpaces body
                        && normalizeBlankLines (normalizeBlankLines body) == normalizeBlankLines body

    describe "markdownFormatterExtension" $ do
        it "normalizes headings and list markers while preserving thematic breaks, inline backticks, and fenced code blocks" $ do
            let input :: DocumentBody
                input = DocumentBody $ Txt.intercalate "\r\n"
                    [ "#見出し1   "
                    , ""
                    , ""
                    , "* * *"
                    , "  - - -"
                    , "```inline```"
                    , "*   項目A"
                    , "  + 項目B"
                    , "-   項目C"
                    , "```markdown   "
                    , "#コード内は変更しない"
                    , "    ```"
                    , "* これもそのまま"
                    , "```   "
                    , "##末尾見出し"
                    , ""
                    ]
                expected :: DocumentBody
                expected = DocumentBody $ Txt.unlines
                    [ "# 見出し1"
                    , ""
                    , "* * *"
                    , "  - - -"
                    , "```inline```"
                    , "- 項目A"
                    , "  - 項目B"
                    , "- 項目C"
                    , "```markdown"
                    , "#コード内は変更しない"
                    , "    ```"
                    , "* これもそのまま"
                    , "```"
                    , "## 末尾見出し"
                    ]
            formatMarkdownBody input `shouldBe` expected
            fmap (.nextBody) (runExtensionCommand markdownFormatterExtension (CommandId "format-markdown") (ExtensionContext input FormatMarkdown))
                `shouldBe` Right expected

        it "rejects markdownOnly commands on plain text documents" $ do
            let plainCtx :: ExtensionContext
                plainCtx = ExtensionContext
                    { body   = DocumentBody "# heading"
                    , format = FormatPlainText
                    }
            runExtensionCommand markdownFormatterExtension (CommandId "format-markdown") plainCtx
                `shouldBe` Left (ExtensionUnsupportedFormat FormatMarkdown FormatPlainText)
            runExtensionCommand markdownFormatterExtension (CommandId "insert-toc") plainCtx
                `shouldBe` Left (ExtensionUnsupportedFormat FormatMarkdown FormatPlainText)

        it "inserts and idempotently updates a TOC block while protecting fenced code and unclosed markers" $ do
            let input :: DocumentBody
                input = DocumentBody $ Txt.unlines
                    [ "<!-- TOC -->"
                    , "- 古い見出し"
                    , "<!-- /TOC -->"
                    , ""
                    , "# 第1章"
                    , "```inline```"
                    , "##\t節 1.1"
                    , "```markdown"
                    , "<!-- TOC -->"
                    , "# コード内見出し"
                    , "<!-- /TOC -->"
                    , "```"
                    , "### 項 1.1.1"
                    ]
                expected :: DocumentBody
                expected = DocumentBody $ Txt.unlines
                    [ "<!-- TOC -->"
                    , "- 第1章"
                    , "  - 節 1.1"
                    , "    - 項 1.1.1"
                    , "<!-- /TOC -->"
                    , ""
                    , "# 第1章"
                    , "```inline```"
                    , "##\t節 1.1"
                    , "```markdown"
                    , "<!-- TOC -->"
                    , "# コード内見出し"
                    , "<!-- /TOC -->"
                    , "```"
                    , "### 項 1.1.1"
                    ]
            insertOrUpdateToc input `shouldBe` Right expected
            (insertOrUpdateToc input >>= insertOrUpdateToc) `shouldBe` Right expected
            fmap (.nextBody) (runExtensionCommand markdownFormatterExtension (CommandId "insert-toc") (ExtensionContext input FormatMarkdown))
                `shouldBe` Right expected

            -- Unclosed TOC marker must NOT delete following body text
            let unclosedInput :: DocumentBody
                unclosedInput = DocumentBody "<!-- TOC -->\n# 残る見出し\n本文\n"
            case insertOrUpdateToc unclosedInput of
                Right (DocumentBody out) -> do
                    out `shouldSatisfy` Txt.isInfixOf "# 残る見出し"
                    out `shouldSatisfy` Txt.isInfixOf "本文"
                Left err -> fail ("Expected Right for unclosed TOC, got: " <> Txt.unpack err)

            -- Document without headings returns ExtensionCommandFailed via runExtensionCommand
            runExtensionCommand markdownFormatterExtension (CommandId "insert-toc") (ExtensionContext (DocumentBody "見出しなしテキスト") FormatMarkdown)
                `shouldSatisfy` isCommandFailed

        it "formatMarkdownBody and insertOrUpdateToc satisfy idempotency properties" $
            property $ \ str ->
                let body :: DocumentBody
                    body = DocumentBody (Txt.pack str)

                    tocIdempotent :: Bool
                    tocIdempotent = case insertOrUpdateToc body of
                        Left _    -> True
                        Right res -> insertOrUpdateToc res == Right res
                in  formatMarkdownBody (formatMarkdownBody body) == formatMarkdownBody body
                        && tocIdempotent

    describe "textTransformExtension" $ do
        it "sorts lines, deduplicates lines preserving order, and normalizes fullwidth alphanumerics including CRLF" $ do
            sortDocumentLines (DocumentBody "c\r\na\r\nb\r\n") `shouldBe` DocumentBody "a\nb\nc\n"
            sortDocumentLines (DocumentBody "") `shouldBe` DocumentBody ""
            dedupeDocumentLines (DocumentBody "b\r\na\r\nb\r\nc\r\na") `shouldBe` DocumentBody "b\na\nc"
            normalizeFullwidthAlnum (DocumentBody "Ｔａｍａ　１２３ＡＢＣテスト")
                `shouldBe` DocumentBody "Tama 123ABCテスト"

            let sortCtx :: ExtensionContext
                sortCtx = ExtensionContext (DocumentBody "b\na\n") FormatPlainText

                dedupeCtx :: ExtensionContext
                dedupeCtx = ExtensionContext (DocumentBody "x\ny\nx\n") FormatPlainText

                normCtx :: ExtensionContext
                normCtx = ExtensionContext (DocumentBody "Ａ１　ｂ２") FormatPlainText
            fmap (.nextBody) (runExtensionCommand textTransformExtension (CommandId "sort-lines") sortCtx)
                `shouldBe` Right (DocumentBody "a\nb\n")
            fmap (.nextBody) (runExtensionCommand textTransformExtension (CommandId "dedupe-lines") dedupeCtx)
                `shouldBe` Right (DocumentBody "x\ny\n")
            fmap (.nextBody) (runExtensionCommand textTransformExtension (CommandId "normalize-fullwidth-alnum") normCtx)
                `shouldBe` Right (DocumentBody "A1 b2")

        it "satisfies structural QuickCheck invariants (sortedness, uniqueness, fullwidth elimination, idempotency)" $
            property $ \ str ->
                let body :: DocumentBody
                    body = DocumentBody (Txt.pack str)

                    sortedLines :: [Text]
                    sortedLines = Txt.lines (toText (sortDocumentLines body))

                    dedupedLines :: [Text]
                    dedupedLines = Txt.lines (toText (dedupeDocumentLines body))

                    normalizedTxt :: Text
                    normalizedTxt = toText (normalizeFullwidthAlnum body)
                in  sortedLines == sort sortedLines
                        && length dedupedLines == Set.size (Set.fromList dedupedLines)
                        && not (Txt.any isFullwidthTarget normalizedTxt)
                        && sortDocumentLines (sortDocumentLines body) == sortDocumentLines body
                        && dedupeDocumentLines (dedupeDocumentLines body) == dedupeDocumentLines body
                        && normalizeFullwidthAlnum (normalizeFullwidthAlnum body) == normalizeFullwidthAlnum body

isCommandFailed :: Either ExtensionError ExtensionResult -> Bool
isCommandFailed (Left ExtensionCommandFailed{}) = True
isCommandFailed _                               = False

isFullwidthTarget :: Char -> Bool
isFullwidthTarget ch =
    ch == '\x3000'
        || (ch >= '\xFF10' && ch <= '\xFF19')
        || (ch >= '\xFF21' && ch <= '\xFF3A')
        || (ch >= '\xFF41' && ch <= '\xFF5A')
