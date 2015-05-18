module Network.CURL720Spec where

import Network.CURL720
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  ----------------------------
  describe "curl_version" $ do
    it "returns libcurl version string: \"libcurl/7.x.x ...\"" $ do
      curl_version >>= (`shouldContain` "libcurl/7.")

