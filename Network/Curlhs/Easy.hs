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

module Network.Curlhs.Easy

  -- module Network.Curlhs.Functions
  ( curl_version
  , curl_version_info
  , curl_easy_strerror
  , curl_easy_init
  , curl_easy_reset
  , curl_easy_cleanup
  , curl_easy_perform
  , curl_easy_getinfo
  , curl_easy_setopt

  -- module Network.Curlhs.Types
  , CURL
  , CURLcode (..)
  , CURLauth (..)
  , CURLinfo (..)
  , CURLoption (..)
  , CURL_version_info_data (..)
  , CURL_version (..)
  , CURL_write_callback, CURL_write_response (..)
  , CURL_read_callback,  CURL_read_response (..)
  ) where

import Network.Curlhs.Functions
import Network.Curlhs.Types

