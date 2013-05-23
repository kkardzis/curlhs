-------------------------------------------------------------------------------
-- |
-- Module      :  Network.CURL730
-- Copyright   :  Copyright © 2012-2013 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.CURL730
  ( -- * Run-Time Linking
    CURLAPI(CURL730), RTLD(..)

    -- * Version info
  , curl_version
  , curl_version_info

  ) where

import Network.CURL000.LibHS
import Network.CURL000.LibLD

