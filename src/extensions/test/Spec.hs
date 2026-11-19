{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main
    ( main
    ) where

import qualified Data.Text                                 as Txt
import           TamaTsuzura.Common.Document               (DocumentBody(..))
import           TamaTsuzura.ExtensionApi                  (Extension(..))
import           TamaTsuzura.Extensions.TrimTrailingSpaces (trimTrailingSpaces, trimTrailingSpacesExtension)
import           Test.Hspec                                (Spec, describe, hspec, it, shouldBe)
import           Test.QuickCheck                           (property)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "trimTrailingSpacesExtension" $ do
        it "removes trailing spaces and tabs while preserving line breaks" $ do
            let input :: DocumentBody
                input = DocumentBody "hello   \nworld\t \n"
                expected :: DocumentBody
                expected = DocumentBody "hello\nworld\n"
            trimTrailingSpacesExtension.transformBuffer input `shouldBe` expected

        it "is idempotent for arbitrary text" $
            property $ \ str ->
                let body :: DocumentBody
                    body = DocumentBody (Txt.pack str)
                in  trimTrailingSpaces (trimTrailingSpaces body) == trimTrailingSpaces body
