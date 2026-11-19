{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import qualified Network.HTTP.Types     as Http
import qualified Network.Wai            as Wai
import qualified Network.Wai.Test       as WaiTest
import           TamaTsuzura.Server.App (application, defaultServerPort, mkServerPort, parsePortArgs, serverPortValue)
import           Test.Hspec             (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parsePortArgs" $ do
        it "returns defaultServerPort when no arguments are given" $
            parsePortArgs [] `shouldBe` Right defaultServerPort

        it "parses valid boundary and normal --port arguments" $ do
            fmap serverPortValue (parsePortArgs ["--port", "1"]) `shouldBe` Right 1
            fmap serverPortValue (parsePortArgs ["--port", "3000"]) `shouldBe` Right 3000
            fmap serverPortValue (parsePortArgs ["--port", "65535"]) `shouldBe` Right 65535

        it "rejects out-of-range port numbers" $ do
            parsePortArgs ["--port", "0"] `shouldBe` Left "Invalid port number: 0"
            parsePortArgs ["--port", "65536"] `shouldBe` Left "Invalid port number: 65536"
            mkServerPort (-1) `shouldBe` Left "Invalid port number: -1"

        it "rejects non-numeric port values and malformed flags" $ do
            parsePortArgs ["--port", "abc"] `shouldBe` Left "Invalid port number: abc"
            parsePortArgs ["--port"] `shouldBe` Left "Usage: tamatsuzura-server [--port PORT]"
            parsePortArgs ["--foo"] `shouldBe` Left "Usage: tamatsuzura-server [--port PORT]"

    describe "WAI application" $ do
        it "responds 200 OK with health JSON on GET /api/health" $ do
            let req :: Wai.Request
                req = WaiTest.setPath WaiTest.defaultRequest "/api/health"
            WaiTest.runSession
                ( do
                    res <- WaiTest.request req
                    WaiTest.assertStatus (Http.statusCode Http.status200) res
                    WaiTest.assertBody "{\"status\":\"ok\"}" res
                )
                application

        it "responds 404 Not Found on unknown paths" $ do
            let req :: Wai.Request
                req = WaiTest.setPath WaiTest.defaultRequest "/api/unknown"
            WaiTest.runSession
                ( do
                    res <- WaiTest.request req
                    WaiTest.assertStatus (Http.statusCode Http.status404) res
                )
                application
