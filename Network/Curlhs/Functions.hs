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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Network.Curlhs.Functions
  ( curl_version
  , curl_version_info
  , curl_easy_init
  , curl_easy_cleanup
  , curl_easy_reset
  , curl_easy_perform
  , curl_easy_getinfo
  , curl_easy_strerror
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String
import Foreign.Storable

import Control.Applicative ((<$>))
import Control.Exception (throwIO)

import Network.Curlhs.FFI.Functions

import Network.Curlhs.TypesH
import Network.Curlhs.Types


-------------------------------------------------------------------------------
class CURLinfo a b where
  curl_easy_getinfo :: CURL -> a -> IO b

instance CURLinfo CURLinfo'S String where
  curl_easy_getinfo curl info = alloca $ \ptr -> do
    code <- fromC <$> ccurl_easy_getinfo'S curl (fromH info) ptr
    ifOK code $ peek ptr >>= peekCString

instance CURLinfo CURLinfo'I Int where
  curl_easy_getinfo curl info = alloca $ \ptr -> do
    code <- fromC <$> ccurl_easy_getinfo'I curl (fromH info) ptr
    ifOK code $ fromIntegral <$> peek ptr

instance CURLinfo CURLinfo'D Double where
  curl_easy_getinfo curl info = alloca $ \ptr -> do
    code <- fromC <$> ccurl_easy_getinfo'D curl (fromH info) ptr
    ifOK code $ realToFrac <$> peek ptr

--instance CURLinfo CURLinfo'L [String] where
--  curl_easy_getinfo curl info = undefined


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
  ifOK code $ return ()

curl_easy_strerror :: CURLcode -> IO String
curl_easy_strerror code =
  ccurl_easy_strerror (fromH code) >>= peekCString


-------------------------------------------------------------------------------
ifOK :: CURLcode -> IO a -> IO a
ifOK CURLE_OK action = action
ifOK code     _      = throwIO code

