module Main
    ( main
    ) where

import           ApiSpec         (apiSpec)
import           AuthApiSpec     (authApiSpec)
import           AuthCryptoSpec  (authCryptoSpec)
import           AuthStorageSpec (authStorageSpec)
import           IntegrationSpec (integrationSpec)
import           StaticSpec      (staticSpec)
import           StorageSpec     (storageSpec)
import           Test.Hspec      (Spec, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    storageSpec
    authCryptoSpec
    authStorageSpec
    authApiSpec
    apiSpec
    staticSpec
    integrationSpec
