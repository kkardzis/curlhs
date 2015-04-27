module Network.CURL730Spec where

import Network.CURL730

import Control.Exception (tryJust, ErrorCall (..))

import Text.ParserCombinators.ReadP (ReadP, readP_to_S, string)
import Text.Read.Lex (readDecP)

import Data.List (isPrefixOf)

import Test.Hspec


testlib :: LIBCURL -> IO (Either String ())
testlib lib =
  let maybeEx s = if (isPrefixOf "<curlhs>" s) then Just s else Nothing
  in  tryJust (\(ErrorCall s) -> maybeEx s) (withlib lib (return ()))


main :: IO ()
main = hspec spec

spec :: Spec
spec = runIO (testlib CURL730) >>= \x -> case x of
  Left  xs -> it "cannot test this module" (pendingWith xs)
  Right () -> before_ (loadlib CURL730) $ after_ (freelib CURL730) $ do

  ----------------------------
  describe "curl_version" $ do
    it "returns libcurl version string: \"libcurl/7.x.x ...\"" $ do
      curl_version >>= (`shouldContain` "libcurl/7.")

    it "returns libcurl version 7.30 or higher compatible" $ do
      let readP = readP_to_S (string "libcurl/7." >> (readDecP :: ReadP Int))
      let check s = case (readP s) of [(v,_)] -> Just (v>=30); _ -> Nothing
      curl_version >>= (`shouldSatisfy` (\s -> maybe False id (check s)))

