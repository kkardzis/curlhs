-------------------------------------------------------------------------------
-- Based on <http://curl.haxx.se/libcurl/c/simple.html>
-- Shows how to get a remote web page in only four libcurl function calls.
-------------------------------------------------------------------------------

module Simple where

import Network.CURL720


main = withlib CURL720 $ do
  curl <- curl_easy_init
  curl_easy_setopt curl
    [ CURLOPT_URL "http://httpbin.org/get"
    , CURLOPT_FOLLOWLOCATION True
    ]
  curl_easy_perform curl
  curl_easy_cleanup curl

