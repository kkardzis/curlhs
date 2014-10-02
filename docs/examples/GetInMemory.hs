-------------------------------------------------------------------------------
-- Based on <http://curl.haxx.se/libcurl/c/getinmemory.html>
-- Example source code to show how the callback function can be used to
-- download data into a chunk of memory instead of storing it in a file.
-------------------------------------------------------------------------------

module GetInMemory where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder    as BB

import Control.Exception
import Data.Monoid
import Data.IORef

import Network.CURL720


main = withlib CURL720 $ do
  lbs <- curlGET "http://httpbin.org/get"
  BL.putStrLn lbs


curlGET :: String -> IO BL.ByteString
curlGET url =
  bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do
    ref <- newIORef mempty
    curl_easy_setopt curl
      [ CURLOPT_URL url
      -- specify URL to get
 
      , CURLOPT_WRITEFUNCTION $ Just (memwrite ref)
      -- send all data to this function
 
      , CURLOPT_USERAGENT "libcurl-agent/1.0"
      -- some servers don't like requests that are made without a user-agent
      -- field, so we provide one
      ]
    curl_easy_perform curl
    fmap BB.toLazyByteString (readIORef ref)


memwrite :: IORef BB.Builder -> CURL_write_callback
memwrite ref bs = do
  modifyIORef ref (flip mappend (BB.byteString bs))
  return CURL_WRITEFUNC_OK

