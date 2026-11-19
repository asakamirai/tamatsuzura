{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main
    ( main
    ) where

import           TamaTsuzura.Common.Document (DocumentBody(..))
import           TamaTsuzura.Common.Text     (ToText(..))
import           TamaTsuzura.ExtensionApi    (Extension(..), ExtensionId(..), ExtensionMetadata(..), ExtensionName(..))
import           Test.Hspec                  (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "ExtensionApi contract" $
        it "applies a pure buffer transform via Extension" $ do
            let meta :: ExtensionMetadata
                meta = ExtensionMetadata
                    { extensionId = ExtensionId "identity"
                    , name        = ExtensionName "Identity"
                    , description = "No-op extension for contract check"
                    }
                ext :: Extension
                ext = Extension
                    { metadata        = meta
                    , transformBuffer = id
                    }
            toText ext.metadata `shouldBe` "identity: Identity"
            ext.transformBuffer (DocumentBody "hello") `shouldBe` DocumentBody "hello"
