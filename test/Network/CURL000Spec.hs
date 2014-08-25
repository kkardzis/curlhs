module Network.CURL000Spec where

import Network.CURL000

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "RTLD LIBCURL" $ do
    it "load/free libcurl" $ do
      pending

