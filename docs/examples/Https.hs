-------------------------------------------------------------------------------
-- Based on <http://curl.haxx.se/libcurl/c/https.html>
-- Gets a single HTTPS page.
-------------------------------------------------------------------------------

module Https where

import Network.CURL720


main = withlib CURL720 $ do
  curl <- curl_easy_init
  curl_easy_setopt curl
    [ CURLOPT_URL "https://httpbin.org/get"

    , CURLOPT_SSL_VERIFYPEER True
    -- If you want to connect to a site who isn't using a certificate that is
    -- signed by one of the certs in the CA bundle you have, you can skip the
    -- verification of the server's certificate. This makes the connection
    -- A LOT LESS SECURE.
    --
    -- If you have a CA cert for the server stored someplace else than in the
    -- default bundle, then the CURLOPT_CAPATH option might come handy for
    -- you.

    , CURLOPT_SSL_VERIFYHOST 2 -- (TODO: should be Bool)
    -- If the site you're connecting to uses a different host name that what
    -- they have mentioned in their server certificate's commonName (or
    -- subjectAltName) fields, libcurl will refuse to connect. You can skip
    -- this check, but this will make the connection less secure.

    ]
  curl_easy_perform curl
  curl_easy_cleanup curl

