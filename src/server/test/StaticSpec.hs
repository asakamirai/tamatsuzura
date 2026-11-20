{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module StaticSpec
    ( staticSpec
    ) where

import           Control.Exception         (bracket)
import           Control.Monad             (when)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LazyBS
import           Data.Text                 (Text)
import qualified Data.Text                 as Txt
import qualified Data.UUID                 as UUID
import qualified Data.UUID.V4              as UUID4
import qualified Network.HTTP.Types        as Http
import qualified Network.Wai               as Wai
import qualified Network.Wai.Test          as WaiTest
import qualified System.Directory.OsPath   as DirOS
import           System.OsPath             (OsPath)
import qualified System.OsPath             as OS
import           TamaTsuzura.Server.App    (mkWarpApplication)
import           TamaTsuzura.Server.Config
    ( WarpConfig(..)
    , envStaticDir
    , resolveWarpConfig
    )
import           TamaTsuzura.Server.Static (contentTypeForOsPath, resolveStaticAssetPath)
import           TamaTsuzura.Server.Storage (defaultStorageConfig)
import           Test.Hspec                (Spec, describe, expectationFailure, it, shouldBe)

staticSpec :: Spec
staticSpec = describe "Warp development static asset serving" $ do
    it "resolves --static-dir and TAMATSUZURA_STATIC_DIR with CLI precedence" $ do
        eFromEnv <- resolveWarpConfig [] [(envStaticDir, "dist/web")]
        case eFromEnv of
            Left err  -> expectationFailure (Txt.unpack err)
            Right cfg -> do
                expected <- OS.encodeFS "dist/web"
                cfg.staticDir `shouldBe` Just expected

        eFromCli <- resolveWarpConfig ["--static-dir", "custom/web"] [(envStaticDir, "dist/web")]
        case eFromCli of
            Left err  -> expectationFailure (Txt.unpack err)
            Right cfg -> do
                expected <- OS.encodeFS "custom/web"
                cfg.staticDir `shouldBe` Just expected

        eEmpty <- resolveWarpConfig ["--static-dir", "   "] []
        eEmpty `shouldBe` Left "Static directory path must not be empty"

    it "assigns accurate Content-Type headers including application/wasm" $ do
        wasmPath <- OS.encodeFS "tamatsuzura-client.wasm"
        htmlPath <- OS.encodeFS "index.html"
        jsPath   <- OS.encodeFS "index.js"
        cssPath  <- OS.encodeFS "styles.css"
        contentTypeForOsPath wasmPath `shouldBe` "application/wasm"
        contentTypeForOsPath htmlPath `shouldBe` "text/html; charset=utf-8"
        contentTypeForOsPath jsPath   `shouldBe` "text/javascript; charset=utf-8"
        contentTypeForOsPath cssPath  `shouldBe` "text/css; charset=utf-8"

    it "serves index.html and Wasm assets while forwarding /api/* and blocking traversal" $
        withTempStaticDir $ \ rootDir dbPath -> do
            writeStaticFile rootDir "index.html" "<html><body>TamaTsuzura</body></html>"
            writeStaticFile rootDir "app.wasm" "\0asm\1\0\0\0"
            writeStaticFile rootDir "index.js" "export const ok = true;"

            let app :: Wai.Application
                app = mkWarpApplication (Just rootDir) (defaultStorageConfig dbPath)

            -- 1. Root path serves index.html
            resRoot <- runWaiRequest app Http.methodGet "/"
            WaiTest.simpleStatus resRoot `shouldBe` Http.status200
            lookup Http.hContentType (WaiTest.simpleHeaders resRoot)
                `shouldBe` Just "text/html; charset=utf-8"
            WaiTest.simpleBody resRoot `shouldBe` "<html><body>TamaTsuzura</body></html>"

            -- 2. Wasm binary served with application/wasm
            resWasm <- runWaiRequest app Http.methodGet "/app.wasm"
            WaiTest.simpleStatus resWasm `shouldBe` Http.status200
            lookup Http.hContentType (WaiTest.simpleHeaders resWasm)
                `shouldBe` Just "application/wasm"
            WaiTest.simpleBody resWasm `shouldBe` "\0asm\1\0\0\0"

            -- 3. HEAD returns headers with empty body
            resHead <- runWaiRequest app Http.methodHead "/index.js"
            WaiTest.simpleStatus resHead `shouldBe` Http.status200
            WaiTest.simpleBody resHead `shouldBe` LazyBS.empty

            -- 4. /api/health passes through untouched to REST API
            resApi <- runWaiRequest app Http.methodGet "/api/health"
            WaiTest.simpleStatus resApi `shouldBe` Http.status200
            WaiTest.simpleBody resApi `shouldBe` "{\"status\":\"ok\"}"

            -- 5. Traversal attempts are rejected
            mTraversal <- resolveStaticAssetPath rootDir ["..", "etc", "passwd"]
            mTraversal `shouldBe` Nothing
            resTraversal <- runWaiRequest app Http.methodGet "/../etc/passwd"
            WaiTest.simpleStatus resTraversal `shouldBe` Http.status404

            -- 6. Non-GET/HEAD on static paths returns 405
            resPost <- runWaiRequest app Http.methodPost "/index.html"
            WaiTest.simpleStatus resPost `shouldBe` Http.status405

    it "serves nested subdirectory assets, rejects directory requests, and blocks symlink escapes" $
        withTempStaticDir $ \ rootDir dbPath -> do
            subSeg <- OS.encodeFS "sub"
            let subDir :: OsPath
                subDir = rootDir OS.</> subSeg
            DirOS.createDirectoryIfMissing True subDir
            writeStaticFile subDir "nested.js" "export const nested = 1;"

            -- Create a file outside rootDir and point a symlink inside rootDir to it
            outsideSeg <- OS.encodeFS "outside-secret.txt"
            escapeSeg  <- OS.encodeFS "escape-link.txt"
            let parentDir :: OsPath
                parentDir = OS.takeDirectory rootDir

                outsideFile :: OsPath
                outsideFile = parentDir OS.</> outsideSeg

                escapeLink :: OsPath
                escapeLink = rootDir OS.</> escapeSeg
            writeStaticFile parentDir "outside-secret.txt" "TOP_SECRET"
            DirOS.createFileLink outsideFile escapeLink

            let app :: Wai.Application
                app = mkWarpApplication (Just rootDir) (defaultStorageConfig dbPath)

            -- Nested asset is served normally
            resNested <- runWaiRequest app Http.methodGet "/sub/nested.js"
            WaiTest.simpleStatus resNested `shouldBe` Http.status200
            lookup Http.hContentType (WaiTest.simpleHeaders resNested)
                `shouldBe` Just "text/javascript; charset=utf-8"
            WaiTest.simpleBody resNested `shouldBe` "export const nested = 1;"

            -- Directory path itself returns 404
            mSubDir <- resolveStaticAssetPath rootDir ["sub"]
            mSubDir `shouldBe` Nothing
            resSubDir <- runWaiRequest app Http.methodGet "/sub"
            WaiTest.simpleStatus resSubDir `shouldBe` Http.status404

            -- Symlink pointing outside rootDir is rejected
            mEscape <- resolveStaticAssetPath rootDir ["escape-link.txt"]
            mEscape `shouldBe` Nothing
            resEscape <- runWaiRequest app Http.methodGet "/escape-link.txt"
            WaiTest.simpleStatus resEscape `shouldBe` Http.status404

withTempStaticDir :: (OsPath -> OsPath -> IO a) -> IO a
withTempStaticDir action = bracket acquire release (uncurry action)
  where
    acquire :: IO (OsPath, OsPath)
    acquire = do
        tmpBase <- DirOS.getTemporaryDirectory
        token <- UUID.toString <$> UUID4.nextRandom
        dirSeg <- OS.encodeFS ("tamatsuzura-static-spec-" <> token)
        webSeg <- OS.encodeFS "web"
        dbSeg  <- OS.encodeFS "spec.sqlite3"
        let baseDir :: OsPath
            baseDir = tmpBase OS.</> dirSeg

            webDir :: OsPath
            webDir = baseDir OS.</> webSeg

            dbPath :: OsPath
            dbPath = baseDir OS.</> dbSeg
        DirOS.createDirectoryIfMissing True webDir
        pure (webDir, dbPath)

    release :: (OsPath, OsPath) -> IO ()
    release (webDir, _) = do
        let baseDir :: OsPath
            baseDir = OS.takeDirectory webDir
        exists <- DirOS.doesDirectoryExist baseDir
        when exists $ DirOS.removeDirectoryRecursive baseDir

writeStaticFile :: OsPath -> Text -> BS.ByteString -> IO ()
writeStaticFile rootDir fileName content = do
    seg <- OS.encodeFS (Txt.unpack fileName)
    -- bytestring の writeFile は FilePath のみ受け付けるため境界でデコードする
    rawPath <- OS.decodeFS (rootDir OS.</> seg)
    BS.writeFile rawPath content

runWaiRequest :: Wai.Application -> Http.Method -> BS.ByteString -> IO WaiTest.SResponse
runWaiRequest app method rawPath =
    WaiTest.runSession
        (WaiTest.request (WaiTest.setPath Wai.defaultRequest { Wai.requestMethod = method } rawPath))
        app
