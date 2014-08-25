module Network.CURL720Spec where

import Network.CURL720

import Text.ParserCombinators.ReadP (ReadP, readP_to_S, string)
import Text.Read.Lex (readDecP)

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = before (loadlib CURL720) $ after (freelib CURL720) $ do

  describe "curl_version" $ do
    it "returns libcurl version string: \"libcurl/7.x.x ...\"" $ do
      curl_version >>= (`shouldContain` "libcurl/7.")

    it "returns libcurl version 7.20 or higher compatible" $ do
      let readP = readP_to_S (string "libcurl/7." >> (readDecP :: ReadP Int))
      let check s = case (readP s) of [(v,_)] -> Just (v>=20); _ -> Nothing
      curl_version >>= (`shouldSatisfy` (\s -> maybe False id (check s)))

