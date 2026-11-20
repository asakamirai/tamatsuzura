{-# LANGUAGE OverloadedStrings #-}

module AuthCryptoSpec
    ( authCryptoSpec
    ) where

import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.Either                    (isLeft, isRight)
import qualified Data.Text                      as Txt
import           TamaTsuzura.Common.Auth        (mkPassword)
import           TamaTsuzura.Common.Text        (ToText(..))
import           TamaTsuzura.Server.Auth.Crypto
    ( AuthCryptoError(..)
    , constantTimeTextEq
    , defaultPasswordHashParams
    , fastTestPasswordHashParams
    , generateCsrfToken
    , generateSessionToken
    , hashPassword
    , hashPasswordWithSalt
    , hashSessionToken
    , mkPasswordHashParams
    , mkSessionToken
    , mkSessionTokenHash
    , parsePasswordHash
    , passwordHashIterations
    , passwordHashMemoryKiB
    , passwordHashOutputLength
    , passwordHashParallelism
    , passwordHashSaltLength
    , passwordHashToText
    , sessionTokenRawText
    , verifyCsrfToken
    , verifyPassword
    )
import           Test.Hspec                     (Spec, describe, expectationFailure, it, shouldBe, shouldNotBe, shouldSatisfy)

authCryptoSpec :: Spec
authCryptoSpec = describe "Auth cryptography (Argon2id PHC, session/CSRF tokens)" $ do
    it "validates PasswordHashParams boundaries and exposes OWASP default parameters" $ do
        passwordHashMemoryKiB defaultPasswordHashParams `shouldBe` 19456
        passwordHashIterations defaultPasswordHashParams `shouldBe` 2
        passwordHashParallelism defaultPasswordHashParams `shouldBe` 1
        passwordHashOutputLength defaultPasswordHashParams `shouldBe` 32
        passwordHashSaltLength defaultPasswordHashParams `shouldBe` 16

        mkPasswordHashParams 64 1 1 32 16 `shouldBe` Right fastTestPasswordHashParams
        mkPasswordHashParams 8 1 1 16 8 `shouldSatisfy` isRight
        mkPasswordHashParams 16 1 2 64 64 `shouldSatisfy` isRight
        mkPasswordHashParams 1048576 100 16 64 64 `shouldSatisfy` isRight
        mkPasswordHashParams 7 1 1 32 16 `shouldSatisfy` isLeft
        mkPasswordHashParams 15 1 2 32 16 `shouldSatisfy` isLeft
        mkPasswordHashParams 1048577 1 1 32 16 `shouldSatisfy` isLeft
        mkPasswordHashParams 64 0 1 32 16 `shouldSatisfy` isLeft
        mkPasswordHashParams 64 101 1 32 16 `shouldSatisfy` isLeft
        mkPasswordHashParams 64 1 0 32 16 `shouldSatisfy` isLeft
        mkPasswordHashParams 64 1 17 32 16 `shouldSatisfy` isLeft
        mkPasswordHashParams 64 1 1 15 16 `shouldSatisfy` isLeft
        mkPasswordHashParams 64 1 1 65 16 `shouldSatisfy` isLeft
        mkPasswordHashParams 64 1 1 32 7 `shouldSatisfy` isLeft
        mkPasswordHashParams 64 1 1 32 65 `shouldSatisfy` isLeft

    it "hashes and verifies passwords deterministically with explicit salt in PHC format and redacts Show" $ do
        pw <- mustRight (mkPassword "secret-passphrase-123")
        wrongPw <- mustRight (mkPassword "secret-passphrase-124")
        let fixedSalt :: ByteString
            fixedSalt = "0123456789abcdef"
        hashed1 <- mustRight (hashPasswordWithSalt fastTestPasswordHashParams fixedSalt pw)
        hashed2 <- mustRight (hashPasswordWithSalt fastTestPasswordHashParams fixedSalt pw)
        hashed1 `shouldBe` hashed2
        show hashed1 `shouldBe` "<PasswordHash>"

        let phcText :: Txt.Text
            phcText = passwordHashToText hashed1
        Txt.isPrefixOf "$argon2id$v=19$m=64,t=1,p=1$" phcText `shouldBe` True
        case Txt.splitOn "$" phcText of
            ["", "argon2id", "v=19", "m=64,t=1,p=1", saltPart, hashPart] -> do
                Txt.isInfixOf "=" saltPart `shouldBe` False
                Txt.isInfixOf "=" hashPart `shouldBe` False
            other -> expectationFailure ("Unexpected PHC segments: " <> show other)
        parsePasswordHash phcText `shouldBe` Right hashed1
        verifyPassword pw hashed1 `shouldBe` True
        verifyPassword wrongPw hashed1 `shouldBe` False

        -- ソルト長不正は拒否する
        hashPasswordWithSalt fastTestPasswordHashParams "short" pw
            `shouldBe` Left (InvalidSaltLength 5 8)
        hashPasswordWithSalt fastTestPasswordHashParams (BS.replicate 65 0x41) pw
            `shouldBe` Left (InvalidSaltLength 65 64)

    it "generates unique random salts per hash and verifies with OWASP default parameters" $ do
        pw <- mustRight (mkPassword "日本語パスフレーズ-2026!")
        wrongPw <- mustRight (mkPassword "日本語パスフレーズ-2025!")
        h1 <- mustRight =<< hashPassword fastTestPasswordHashParams pw
        h2 <- mustRight =<< hashPassword fastTestPasswordHashParams pw
        h1 `shouldNotBe` h2
        passwordHashToText h1 `shouldNotBe` passwordHashToText h2
        verifyPassword pw h1 `shouldBe` True
        verifyPassword pw h2 `shouldBe` True
        verifyPassword wrongPw h1 `shouldBe` False

        -- OWASP 既定パラメータ (m=19456 KiB, t=2, p=1) での実計算・検証を 1 回疎通確認する
        defaultHashed <- mustRight =<< hashPassword defaultPasswordHashParams pw
        Txt.isPrefixOf "$argon2id$v=19$m=19456,t=2,p=1$" (passwordHashToText defaultHashed) `shouldBe` True
        verifyPassword pw defaultHashed `shouldBe` True
        verifyPassword wrongPw defaultHashed `shouldBe` False

    it "matches known reference Argon2id PHC vector (phc-winner-argon2)" $ do
        refPw <- mustRight (mkPassword "password")
        wrongPw <- mustRight (mkPassword "password!")
        refParams <- mustRight (mkPasswordHashParams 4096 3 1 32 8)
        let expectedPhc :: Txt.Text
            expectedPhc = "$argon2id$v=19$m=4096,t=3,p=1$c29tZXNhbHQ$qLml5cbqFAO6YxVHhrSBHP0UWdxrIxkNcM8aMX3blzU"
        hashed <- mustRight (hashPasswordWithSalt refParams "somesalt" refPw)
        passwordHashToText hashed `shouldBe` expectedPhc
        parsed <- mustRight (parsePasswordHash expectedPhc)
        verifyPassword refPw parsed `shouldBe` True
        verifyPassword wrongPw parsed `shouldBe` False

    it "rejects malformed PHC strings, parameter overflows, leading zeros, and non-canonical Base64 encodings" $ do
        let oversizeB64 :: Txt.Text
            oversizeB64 = Txt.replicate 88 "A"
            badPhcSamples :: [Txt.Text]
            badPhcSamples =
                [ ""
                , "$argon2i$v=19$m=64,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=16$m=64,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=64,t=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=0,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=064,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=64,t=01,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=1048577,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=64,t=101,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=64,t=1,p=17$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=99999999999,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=abc,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=64,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg==$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
                -- 非ゼロ余剰パディングビットを含む非正規 Base64 (remLen == 2 および remLen == 3)
                , "$argon2id$v=19$m=64,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZh$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=64,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"
                -- ソルト・ダイジェストの下限未満・上限超過
                , "$argon2id$v=19$m=64,t=1,p=1$short$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=64,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$AAAA"
                , "$argon2id$v=19$m=64,t=1,p=1$" <> oversizeB64 <> "$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "$argon2id$v=19$m=64,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$" <> oversizeB64
                ]
        mapM_ (\ sample -> parsePasswordHash sample `shouldSatisfy` isLeft) badPhcSamples

    it "generates 256-bit session tokens, deterministic SHA-256 token hashes, and CSRF tokens" $ do
        tok1 <- generateSessionToken
        tok2 <- generateSessionToken
        tok1 `shouldNotBe` tok2
        show tok1 `shouldBe` "<SessionToken>"
        Txt.length (sessionTokenRawText tok1) `shouldBe` 43
        mkSessionToken (sessionTokenRawText tok1) `shouldBe` Right tok1
        mkSessionToken "too-short" `shouldSatisfy` isLeft
        mkSessionToken (Txt.replicate 43 "!") `shouldSatisfy` isLeft

        let hash1      = hashSessionToken tok1
            hash1Again = hashSessionToken tok1
            hash2      = hashSessionToken tok2
        hash1 `shouldBe` hash1Again
        hash1 `shouldNotBe` hash2
        Txt.length (toText hash1) `shouldBe` 64
        mkSessionTokenHash (toText hash1) `shouldBe` Right hash1
        mkSessionTokenHash (Txt.toUpper (toText hash1)) `shouldSatisfy` isLeft
        mkSessionTokenHash "abcd" `shouldSatisfy` isLeft

        csrf1 <- mustRight =<< generateCsrfToken
        csrf2 <- mustRight =<< generateCsrfToken
        csrf1 `shouldNotBe` csrf2
        show csrf1 `shouldBe` "<CsrfToken>"
        verifyCsrfToken csrf1 csrf1 `shouldBe` True
        verifyCsrfToken csrf1 csrf2 `shouldBe` False
        Txt.length (toText csrf1) `shouldBe` 43

        constantTimeTextEq "same-token-value" "same-token-value" `shouldBe` True
        constantTimeTextEq "same-token-value" "diff-token-value" `shouldBe` False
        constantTimeTextEq "short" "longer" `shouldBe` False

mustRight :: Show e => Either e a -> IO a
mustRight (Right val) = pure val
mustRight (Left err)  = fail ("Expected Right, got Left: " <> show err)
