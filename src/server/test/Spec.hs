module Main
    ( main
    ) where

import           ApiSpec         (apiSpec)
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
    apiSpec
    staticSpec
    integrationSpec
