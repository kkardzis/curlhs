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
  , curl_easy_strerror
  , curl_easy_init
  , curl_easy_cleanup
  , curl_easy_reset
  , curl_easy_perform
  , curl_easy_getinfo
  , curl_easy_setopt, CURLoption
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable
import Foreign.C.String
import Foreign.Ptr

import Control.Applicative ((<$>), (<*>))
import Control.Exception (throwIO)

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)

import Network.Curlhs.FFI.Functions
import Network.Curlhs.FFI.Symbols
import Network.Curlhs.FFI.Types

import Network.Curlhs.TypesH
import Network.Curlhs.Types


-------------------------------------------------------------------------------
ifOK :: CURLcode -> IO a -> IO a
ifOK CURLE_OK action = action
ifOK code     _      = throwIO code


-------------------------------------------------------------------------------
curl_version :: IO String
curl_version = ccurl_version >>= peekCString

curl_version_info :: CURLversion -> IO CURL_version_info_data
curl_version_info v = ccurl_version_info (fromH v) >>= peekCCURL

curl_easy_strerror :: CURLcode -> IO String
curl_easy_strerror code =
  ccurl_easy_strerror (fromH code) >>= peekCString

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


-------------------------------------------------------------------------------
curl_easy_getinfo :: CURL -> IO CURLinfo
curl_easy_getinfo curl = CURLinfo
  <$> curl_easy_getinfo'S curl cCURLINFO_EFFECTIVE_URL
  <*> getinfo'RespCode    curl cCURLINFO_RESPONSE_CODE
  <*> getinfo'RespCode    curl cCURLINFO_HTTP_CONNECTCODE
  <*> getinfo'FileTime    curl cCURLINFO_FILETIME
  <*> curl_easy_getinfo'D curl cCURLINFO_TOTAL_TIME
  <*> curl_easy_getinfo'D curl cCURLINFO_NAMELOOKUP_TIME
  <*> curl_easy_getinfo'D curl cCURLINFO_CONNECT_TIME
  <*> curl_easy_getinfo'D curl cCURLINFO_APPCONNECT_TIME
  <*> curl_easy_getinfo'D curl cCURLINFO_PRETRANSFER_TIME
  <*> curl_easy_getinfo'D curl cCURLINFO_STARTTRANSFER_TIME
  <*> curl_easy_getinfo'D curl cCURLINFO_REDIRECT_TIME
  <*> curl_easy_getinfo'I curl cCURLINFO_REDIRECT_COUNT
  <*> getinfo'MString     curl cCURLINFO_REDIRECT_URL
  <*> curl_easy_getinfo'D curl cCURLINFO_SIZE_UPLOAD
  <*> curl_easy_getinfo'D curl cCURLINFO_SIZE_DOWNLOAD
  <*> curl_easy_getinfo'D curl cCURLINFO_SPEED_DOWNLOAD
  <*> curl_easy_getinfo'D curl cCURLINFO_SPEED_UPLOAD
  <*> curl_easy_getinfo'I curl cCURLINFO_HEADER_SIZE
  <*> curl_easy_getinfo'I curl cCURLINFO_REQUEST_SIZE
  <*> curl_easy_getinfo'I curl cCURLINFO_SSL_VERIFYRESULT
--  <*> curl_easy_getinfo'L curl cCURLINFO_SSL_ENGINES    -- [String]
  <*> getinfo'ContentLen  curl cCURLINFO_CONTENT_LENGTH_DOWNLOAD
  <*> getinfo'ContentLen  curl cCURLINFO_CONTENT_LENGTH_UPLOAD
  <*> getinfo'MString     curl cCURLINFO_CONTENT_TYPE
--  <*> curl_easy_getinfo'S curl cCURLINFO_PRIVATE        -- String
--  <*> curl_easy_getinfo'I curl cCURLINFO_HTTPAUTH_AVAIL
--  <*> curl_easy_getinfo'I curl cCURLINFO_PROXYAUTH_AVAIL
--  <*> curl_easy_getinfo'I curl cCURLINFO_OS_ERRNO
  <*> curl_easy_getinfo'I curl cCURLINFO_NUM_CONNECTS
  <*> curl_easy_getinfo'S curl cCURLINFO_PRIMARY_IP
  <*> curl_easy_getinfo'I curl cCURLINFO_PRIMARY_PORT
  <*> curl_easy_getinfo'S curl cCURLINFO_LOCAL_IP
  <*> curl_easy_getinfo'I curl cCURLINFO_LOCAL_PORT
--  <*> curl_easy_getinfo'L curl cCURLINFO_COOKIELIST     -- [String]
--  <*> curl_easy_getinfo'I curl cCURLINFO_LASTSOCKET
  <*> getinfo'MString     curl cCURLINFO_FTP_ENTRY_PATH
--  <*> curl_easy_getinfo'L curl cCURLINFO_CERTINFO       -- [String]
  <*> getinfo'TimeCond    curl cCURLINFO_CONDITION_UNMET
  <*> getinfo'MString     curl cCURLINFO_RTSP_SESSION_ID
  <*> curl_easy_getinfo'I curl cCURLINFO_RTSP_CLIENT_CSEQ
  <*> curl_easy_getinfo'I curl cCURLINFO_RTSP_SERVER_CSEQ
  <*> curl_easy_getinfo'I curl cCURLINFO_RTSP_CSEQ_RECV

curl_easy_getinfo'S :: CURL -> CCURLinfo'S -> IO String
curl_easy_getinfo'S curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'S curl info ptr
  ifOK code (peek ptr >>= peekCString)

curl_easy_getinfo'I :: CURL -> CCURLinfo'I -> IO Int
curl_easy_getinfo'I curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'I curl info ptr
  ifOK code (fromIntegral <$> peek ptr)

curl_easy_getinfo'D :: CURL -> CCURLinfo'D -> IO Double
curl_easy_getinfo'D curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'D curl info ptr
  ifOK code (realToFrac <$> peek ptr)

--curl_easy_getinfo'L :: CURL -> CCURLinfo'L -> IO [String]
--curl_easy_getinfo'L curl info = undefined

getinfo'MString :: CURL -> CCURLinfo'S -> IO (Maybe String)
getinfo'MString curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'S curl info ptr
  ifOK code $ do
    csptr <- peek ptr
    if (csptr == nullPtr)
      then return Nothing
      else Just <$> peekCString csptr

getinfo'RespCode :: CURL -> CCURLinfo'I -> IO (Maybe Int)
getinfo'RespCode curl info = do
  v <- curl_easy_getinfo'I curl info
  return $ if (v == 0) then Nothing else Just v

getinfo'FileTime :: CURL -> CCURLinfo'I -> IO (Maybe UTCTime)
getinfo'FileTime curl info = do
  v <- curl_easy_getinfo'I curl info
  return $ if (v == (-1) || v == 0)
    then Nothing
    else Just (posixSecondsToUTCTime $ realToFrac v)

getinfo'ContentLen :: CURL -> CCURLinfo'D -> IO (Maybe Double)
getinfo'ContentLen curl info = do
  v <- curl_easy_getinfo'D curl info
  return $ if (v == (-1)) then Nothing else Just v

getinfo'TimeCond :: CURL -> CCURLinfo'I -> IO Bool
getinfo'TimeCond curl info = toEnum <$> curl_easy_getinfo'I curl info


{-
infotest0 = do
  curl <- curl_easy_init
  info <- curl_easy_getinfo curl
  curl_easy_cleanup curl
  return info

infotest1 url = do
  curl <- curl_easy_init
  curl_easy_setopt curl CURLOPT_URL url
  curl_easy_perform curl
  info <- curl_easy_getinfo curl
  curl_easy_cleanup curl
  return info
-}



-------------------------------------------------------------------------------
curl_easy_setopt :: CURLoption opt a => CURL -> opt -> a -> IO ()
curl_easy_setopt = setopt

class CURLoption opt a where
  setopt :: CURL -> opt -> a -> IO ()

instance CURLoption CURLoption'S String where
  setopt curl opt val = withCString val $ \ptr -> do
    code <- fromC <$> ccurl_easy_setopt'String curl (fromH opt) ptr
    ifOK code $ return ()


