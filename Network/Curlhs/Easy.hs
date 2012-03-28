-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Easy
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curlhs.Easy (

  -- * Global interface

  -- ** Version info
    curl_version
  , curl_version_info
  , CURL_version_info_data (..)
  , CURL_version (..)

  -- ** Error codes
  -- |  More about error codes in libcurl on
  --    <http://curl.haxx.se/libcurl/c/libcurl-errors.html>
  , curl_easy_strerror
  , CURLcode (..)

  -- * Easy interface
  -- | See <http://curl.haxx.se/libcurl/c/libcurl-easy.html>
  --   for easy interface overview.

  -- ** Init, reset, cleanup
  , curl_easy_init
  , curl_easy_reset
  , curl_easy_cleanup
  , CURL

  -- ** Transfer
  , curl_easy_perform

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
  , CURLtlsauth   (..) |7214:----|
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
  , CURLsslopt    (..) |7250:----|
  , CURLgssapi    (..) |7220:----|
  , CURLsshauth   (..)

  ) where

import Network.Curlhs.Functions
import Network.Curlhs.Types

