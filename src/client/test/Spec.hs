{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main
    ( main
    ) where

import           TamaTsuzura.Client.App                (ClientState(..), applyExtensionById, initialClientState)
import           TamaTsuzura.Client.ExtensionRegistry  (bundledExtensions)
import           TamaTsuzura.Common.Document           (DocumentBody(..), emptyDocumentBody, emptyDocumentTitle)
import           TamaTsuzura.ExtensionApi              (ExtensionId(..))
import           Test.Hspec                            (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "ClientState and bundled extensions" $ do
        it "initializes empty document state with injected bundled extensions" $ do
            let state :: ClientState
                state = initialClientState bundledExtensions
            state.title `shouldBe` emptyDocumentTitle
            state.body `shouldBe` emptyDocumentBody
            length state.extensions `shouldBe` 1

        it "applies a bundled extension by id" $ do
            let base :: ClientState
                base = (initialClientState bundledExtensions)
                    { body = DocumentBody "line1   \nline2\t\n"
                    }
                updated :: ClientState
                updated = applyExtensionById (ExtensionId "trim-trailing-spaces") base
            updated.body `shouldBe` DocumentBody "line1\nline2\n"

        it "keeps state unchanged when extension id is not registered" $ do
            let base :: ClientState
                base = (initialClientState bundledExtensions)
                    { body = DocumentBody "keep  "
                    }
                updated :: ClientState
                updated = applyExtensionById (ExtensionId "non-existent") base
            updated.body `shouldBe` DocumentBody "keep  "
