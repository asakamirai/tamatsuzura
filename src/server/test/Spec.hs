module Main
    ( main
    ) where

import           ApiSpec         (apiSpec)
import           IntegrationSpec (integrationSpec)
import           StorageSpec     (storageSpec)
import           Test.Hspec      (Spec, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    storageSpec
    apiSpec
    integrationSpec
