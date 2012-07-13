-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
--
-- Module "Network.Curlhs" provides a mid-level interface to @libcurl@.
-- For a direct low-level bindings go to "Network.Curlhs.Base".
--
-- API of this module follows the API of @libcurl@ as defined in version
-- 7.25.0 of the library. But it also depends on the version of @libcurl@
-- that is used during compilation of the @curlhs@ package. It is possible
-- to use @curlhs@ with older versions of @libcurl@, just keep in mind
-- that some features may not be available then.
--
-- There is not much documentation here, maybe the future will change that,
-- but for now please use the original @libcurl@ documentation. API provided
-- here follows the original API, so this shouldn't be a big problem.
-- Documentation about @libcurl@ and/or its particular functions may be
-- found in manual pages, which are available among others at the @libcurl@
-- project site (please refer to <http://curl.haxx.se/libcurl/>).
-- 
-- Exposed API is still somewhat incomplete, but is usable.
-- Work on the rest are in progress.
--
-- Simple example:
--
-- > import qualified Data.ByteString.Char8 as BS
-- > import Data.IORef (newIORef, readIORef, atomicModifyIORef)
-- > import Network.Curlhs
-- > 
-- > curlGET :: BS.ByteString -> IO BS.ByteString
-- > curlGET url = withCURL $ \curl -> do
-- >   buff <- newIORef BS.empty
-- >   curl_easy_setopt curl
-- >     [ CURLOPT_URL     url
-- >     , CURLOPT_VERBOSE True
-- >     , CURLOPT_WRITEFUNCTION $ Just (memwrite buff)
-- >     ]
-- >   curl_easy_perform curl
-- >   readIORef buff
-- >
-- > memwrite :: IORef BS.ByteString -> CURL_write_callback
-- > memwrite buff newbs = atomicModifyIORef buff $ \oldbuff ->
-- >   (BS.append oldbuff newbs, CURL_WRITEFUNC_OK)
--
-------------------------------------------------------------------------------

module Network.Curlhs (

  -- * Global interface

  -- ** Init and cleanup
    withLIBCURL
  , curl_global_init
  , curl_global_cleanup
  , CURLglobal (..)

  -- ** Version info
  , curl_version
  , curl_version_info
  , CURL_version_info_data (..)
  , CURL_version (..)

  -- * Easy interface
  -- | See <http://curl.haxx.se/libcurl/c/libcurl-easy.html>
  --   for easy interface overview.

  , module Network.Curlhs.Easy

  -- * Multi interface
  -- | See <http://curl.haxx.se/libcurl/c/libcurl-multi.html>
  --   for multi interface overview.

  , module Network.Curlhs.Multi

  -- * Share interface
  -- | See <http://curl.haxx.se/libcurl/c/libcurl-share.html>
  --   for share interface overview.

  , module Network.Curlhs.Share

  ) where

import Network.Curlhs.Easy
import Network.Curlhs.Multi
import Network.Curlhs.Share

import Network.Curlhs.Core
import Network.Curlhs.Types

