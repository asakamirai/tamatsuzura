{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main
    ( main
    ) where

import           TamaTsuzura.Common.Document (DocumentBody(..), DocumentFormat(..))
import           TamaTsuzura.Common.Text     (ToText(..))
import           TamaTsuzura.ExtensionApi
    ( CommandId(..)
    , CommandName(..)
    , Extension(..)
    , ExtensionCategory(..)
    , ExtensionCommand(..)
    , ExtensionCommandInfo(..)
    , ExtensionContext(..)
    , ExtensionError(..)
    , ExtensionId(..)
    , ExtensionMetadata(..)
    , ExtensionName(..)
    , ExtensionRegistryError(..)
    , ExtensionResult(..)
    , defaultExtensionVersion
    , findExtensionCommand
    , mkSimpleExtension
    , runExtensionCommand
    , toExtensionCommandInfo
    , validateExtensionRegistry
    )
import           Test.Hspec                  (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ExtensionApi contract" $ do
        it "applies a pure buffer transform via mkSimpleExtension and runExtensionCommand" $ do
            let meta :: ExtensionMetadata
                meta = ExtensionMetadata
                    { extensionId    = ExtensionId "identity"
                    , name           = ExtensionName "Identity"
                    , description    = "No-op extension for contract check"
                    , version        = defaultExtensionVersion
                    , category       = CategoryFormatting
                    , defaultEnabled = True
                    }
                ext :: Extension
                ext = mkSimpleExtension meta (CommandId "run") False id

                ctx :: ExtensionContext
                ctx = ExtensionContext
                    { body   = DocumentBody "hello"
                    , format = FormatMarkdown
                    }
            toText ext.metadata `shouldBe` "identity: Identity"
            ext.transformBuffer (DocumentBody "hello") `shouldBe` DocumentBody "hello"
            runExtensionCommand ext (CommandId "run") ctx
                `shouldBe` Right ExtensionResult
                    { nextBody = DocumentBody "hello"
                    , summary  = Nothing
                    }

        it "enforces markdownOnly format guard and reports missing command IDs" $ do
            let meta :: ExtensionMetadata
                meta = ExtensionMetadata
                    { extensionId    = ExtensionId "md-only"
                    , name           = ExtensionName "MD Only"
                    , description    = "Markdown-only test"
                    , version        = defaultExtensionVersion
                    , category       = CategoryMarkdown
                    , defaultEnabled = True
                    }
                ext :: Extension
                ext = mkSimpleExtension meta (CommandId "fmt") True id

                plainCtx :: ExtensionContext
                plainCtx = ExtensionContext
                    { body   = DocumentBody "plain"
                    , format = FormatPlainText
                    }

                mdCtx :: ExtensionContext
                mdCtx = ExtensionContext
                    { body   = DocumentBody "# ok"
                    , format = FormatMarkdown
                    }

                failingExt :: Extension
                failingExt = Extension
                    { metadata = mkMeta "fail-ext"
                    , commands =
                        [ ExtensionCommand
                            { commandId    = CommandId "boom"
                            , label        = CommandName "Boom"
                            , description  = "Always fails"
                            , markdownOnly = False
                            , runCommand   = \ _ -> Left (ExtensionCommandFailed "failed reason")
                            }
                        ]
                    , transformBuffer = id
                    }
            runExtensionCommand ext (CommandId "fmt") plainCtx
                `shouldBe` Left (ExtensionUnsupportedFormat FormatMarkdown FormatPlainText)
            runExtensionCommand ext (CommandId "fmt") mdCtx
                `shouldBe` Right (ExtensionResult (DocumentBody "# ok") Nothing)
            runExtensionCommand ext (CommandId "missing") plainCtx
                `shouldBe` Left (ExtensionCommandNotFound (ExtensionId "md-only") (CommandId "missing"))
            runExtensionCommand failingExt (CommandId "boom") plainCtx
                `shouldBe` Left (ExtensionCommandFailed "failed reason")
            fmap toExtensionCommandInfo (findExtensionCommand ext (CommandId "fmt"))
                `shouldBe` Just ExtensionCommandInfo
                    { commandId    = CommandId "fmt"
                    , label        = CommandName "MD Only"
                    , description  = "Markdown-only test"
                    , markdownOnly = True
                    }
            fmap toExtensionCommandInfo (findExtensionCommand ext (CommandId "missing"))
                `shouldBe` Nothing

    describe "validateExtensionRegistry" $ do
        it "accepts empty list and distinct extensions with non-empty distinct commands" $ do
            let extA :: Extension
                extA = mkSimpleExtension (mkMeta "ext-a") (CommandId "cmd-1") False id

                extB :: Extension
                extB = mkSimpleExtension (mkMeta "ext-b") (CommandId "cmd-1") False id
            fmap length (validateExtensionRegistry []) `shouldBe` Right 0
            fmap length (validateExtensionRegistry [extA, extB]) `shouldBe` Right 2

        it "rejects duplicate extension IDs, empty command lists, and duplicate command IDs" $ do
            let extA :: Extension
                extA = mkSimpleExtension (mkMeta "ext-a") (CommandId "cmd-1") False id

                extEmpty :: Extension
                extEmpty = Extension
                    { metadata        = mkMeta "ext-empty"
                    , commands        = []
                    , transformBuffer = id
                    }

                dupCmd :: ExtensionCommand
                dupCmd = ExtensionCommand
                    { commandId    = CommandId "dup"
                    , label        = CommandName "Dup"
                    , description  = "dup"
                    , markdownOnly = False
                    , runCommand   = \ ctx -> Right (ExtensionResult ctx.body Nothing)
                    }

                extDupCmd :: Extension
                extDupCmd = Extension
                    { metadata        = mkMeta "ext-dup-cmd"
                    , commands        = [dupCmd, dupCmd]
                    , transformBuffer = id
                    }
            fmap length (validateExtensionRegistry [extA, extA])
                `shouldBe` Left (DuplicateExtensionId (ExtensionId "ext-a"))
            fmap length (validateExtensionRegistry [extEmpty])
                `shouldBe` Left (EmptyExtensionCommands (ExtensionId "ext-empty"))
            fmap length (validateExtensionRegistry [extDupCmd])
                `shouldBe` Left (DuplicateCommandId (ExtensionId "ext-dup-cmd") (CommandId "dup"))
  where
    mkMeta :: ExtensionId -> ExtensionMetadata
    mkMeta extId = ExtensionMetadata
        { extensionId    = extId
        , name           = ExtensionName (toText extId)
        , description    = "test"
        , version        = defaultExtensionVersion
        , category       = CategoryTextTransform
        , defaultEnabled = True
        }
