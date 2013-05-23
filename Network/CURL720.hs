-------------------------------------------------------------------------------
-- |
-- Module      :  Network.CURL720
-- Copyright   :  Copyright © 2012-2013 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.CURL720
  ( -- * Run-Time Linking
    CURLAPI(CURL720), RTLD(..)


  -----------------------------------------------------------------------------
  -- * Global interface
  -----------------------------------------------------------------------------
 
  -- ** Version info
  , curl_version
  , curl_version_info
  , CURL_version_info (..)
  , CURL_version (..)

  -- ** Exceptions
  -- | More about error codes in libcurl on
  --   <http://curl.haxx.se/libcurl/c/libcurl-errors.html>
  , curl_easy_strerror , CURLcode   (..)
  , curl_share_strerror, CURLSHcode (..)


  -----------------------------------------------------------------------------
  -- * Easy interface
  -- | See <http://curl.haxx.se/libcurl/c/libcurl-easy.html>
  --   for easy interface overview.
  -----------------------------------------------------------------------------

  -- ** Init / Cleanup
  , CURL, withCURL
  , curl_easy_init
  , curl_easy_cleanup
  , curl_easy_reset

  -- ** Transfer
  , curl_easy_perform
  , curl_easy_recv
  , curl_easy_send

  -- ** Get info
  , curl_easy_getinfo
  , CURLinfo (..)

  -- ** Set options
  , curl_easy_setopt
  , CURLoption (..)

  -- *** Callbacks
  , CURL_write_callback, CURL_write_response (..)
  , CURL_read_callback , CURL_read_response  (..)

  -- *** Constants
  , CURLproto     (..)
  , CURLproxy     (..)
  , CURLnetrc     (..)
  , CURLauth      (..)
  , CURLtlsauth   (..)
  , CURLredir     (..)
  , CURLhttpver   (..)
  , CURLftpcreate (..)
  , CURLftpauth   (..)
  , CURLftpssl    (..)
  , CURLftpmethod (..)
  , CURLrtspreq   (..)
  , CURLtimecond  (..)
  , CURLclosepol  (..)
  , CURLipresolve (..)
  , CURLusessl    (..)
  , CURLsslver    (..)
  , CURLsslopt    (..)
  , CURLgssapi    (..)
  , CURLsshauth   (..)


  -----------------------------------------------------------------------------
  -- * Multi interface
  -- | See <http://curl.haxx.se/libcurl/c/libcurl-multi.html>
  --   for multi interface overview.
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- * Share interface
  -- | See <http://curl.haxx.se/libcurl/c/libcurl-share.html>
  --   for share interface overview.
  -----------------------------------------------------------------------------

  -- ** Init / cleanup
  , CURLSH, withCURLSH
  , curl_share_init
  , curl_share_cleanup

  -- ** Set options
  , curl_share_setopt
  , CURLSHoption (..)

  -- *** Constants
  , CURLSHlockdata (..)


  ) where

import Network.CURL000.LibHS
import Network.CURL000.LibLD
import Network.CURL000.Types

