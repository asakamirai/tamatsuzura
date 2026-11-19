{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import qualified Data.Aeson                  as Aeson
import           Data.Text                   (Text)
import qualified Data.Text                   as Txt
import           TamaTsuzura.Common.Document (DocumentBody(..), healthOk)
import           TamaTsuzura.Common.Text     (ToText(..))
import           Test.Hspec                  (Spec, describe, hspec, it, shouldBe)
import           Test.QuickCheck             (property)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "HealthStatus JSON serialization" $ do
        it "roundtrips healthOk through JSON" $
            Aeson.decode (Aeson.encode healthOk) `shouldBe` Just healthOk

        it "encodes status field matching OpenAPI schema" $
            Aeson.encode healthOk `shouldBe` "{\"status\":\"ok\"}"

    describe "DocumentBody ToText" $
        it "preserves arbitrary UTF-8 string content" $
            property $ \ str ->
                let txt :: Text
                    txt = Txt.pack str
                in  toText (DocumentBody txt) == txt
