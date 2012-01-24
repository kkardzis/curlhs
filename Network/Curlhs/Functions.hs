-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Functions
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curlhs.Functions where

import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Control.Applicative ((<$>))
import Control.Exception (throwIO)

import Network.Curlhs.FFI.Types
import Network.Curlhs.FFI.Functions

import Network.Curlhs.Types


-------------------------------------------------------------------------------
{-
curl_easy_getinfo_string :: CURL -> CURLinfo_S -> IO String
curl_easy_getinfo_string curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo curl (fromH info) ptr
  case code of
    CURLE_OK  -> peek ptr >>= peekCString
    otherwise -> throwIO code

curl_easy_getinfo_long :: CURL -> CURLinfo_I -> IO Int
curl_easy_getinfo_long curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo curl (fromH info) ptr
  case code of
    CURLE_OK  -> peek ptr
    otherwise -> throwIO code

curl_easy_getinfo_double :: CURL -> CURLinfo_D -> IO Double
curl_easy_getinfo_double curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo curl (fromH info) ptr
  case code of
    CURLE_OK  -> peek ptr
    otherwise -> throwIO code

curl_easy_getinfo_slist :: CURL -> CURLinfo_L -> IO CURL_slist
curl_easy_getinfo_slist curl info = undefined
-}


-------------------------------------------------------------------------------
curl_version :: IO String
curl_version = ccurl_version >>= peekCString

curl_version_info :: CURLversion -> IO CURL_version_info_data
curl_version_info v = ccurl_version_info (fromH v) >>= peekCCURL

curl_easy_init :: IO CURL
curl_easy_init = ccurl_easy_init

curl_easy_cleanup :: CURL -> IO ()
curl_easy_cleanup = ccurl_easy_cleanup

curl_easy_reset :: CURL -> IO ()
curl_easy_reset = ccurl_easy_reset

curl_easy_perform :: CURL -> IO ()
curl_easy_perform curl = do
  code <- fromC <$> ccurl_easy_perform curl
  case code of
    CURLE_OK  -> return ()
    otherwise -> throwIO code

curl_easy_strerror :: CURLcode -> IO String
curl_easy_strerror code =
  ccurl_easy_strerror (fromH code) >>= peekCString

