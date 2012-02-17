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
import Foreign.Storable      (peek, sizeOf)
import Foreign.C.String      (peekCString, withCString)
import Foreign.Ptr           (Ptr, nullPtr, plusPtr)

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock       (UTCTime)
import Data.Maybe            (mapMaybe)
import Data.Bits             ((.&.))

import Control.Applicative ((<$>), (<*>))
import Control.Exception   (throwIO)

import Network.Curlhs.FFI.Functions
import Network.Curlhs.FFI.Symbols
import Network.Curlhs.FFI.Types

import Network.Curlhs.TypesH
import Network.Curlhs.Types


-------------------------------------------------------------------------------
curl_version :: IO String
curl_version = ccurl_version >>= peekCString

curl_version_info :: CURLversion -> IO CURL_version_info_data
curl_version_info v = ccurl_version_info (fromH v) >>= peekCCURL




-------------------------------------------------------------------------------
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
  <$> getinfo'String   curl cCURLINFO_EFFECTIVE_URL
  <*> getinfo'RespCode curl cCURLINFO_RESPONSE_CODE
  <*> getinfo'RespCode curl cCURLINFO_HTTP_CONNECTCODE
  <*> getinfo'FileTime curl cCURLINFO_FILETIME
  <*> getinfo'Double   curl cCURLINFO_TOTAL_TIME
  <*> getinfo'Double   curl cCURLINFO_NAMELOOKUP_TIME
  <*> getinfo'Double   curl cCURLINFO_CONNECT_TIME
  <*> getinfo'Double   curl cCURLINFO_APPCONNECT_TIME
  <*> getinfo'Double   curl cCURLINFO_PRETRANSFER_TIME
  <*> getinfo'Double   curl cCURLINFO_STARTTRANSFER_TIME
  <*> getinfo'Double   curl cCURLINFO_REDIRECT_TIME
  <*> getinfo'Int      curl cCURLINFO_REDIRECT_COUNT
  <*> getinfo'MString  curl cCURLINFO_REDIRECT_URL
  <*> getinfo'Double   curl cCURLINFO_SIZE_UPLOAD
  <*> getinfo'Double   curl cCURLINFO_SIZE_DOWNLOAD
  <*> getinfo'Double   curl cCURLINFO_SPEED_DOWNLOAD
  <*> getinfo'Double   curl cCURLINFO_SPEED_UPLOAD
  <*> getinfo'Int      curl cCURLINFO_HEADER_SIZE
  <*> getinfo'Int      curl cCURLINFO_REQUEST_SIZE
  <*> getinfo'Int      curl cCURLINFO_SSL_VERIFYRESULT
  <*> getinfo'SList    curl cCURLINFO_SSL_ENGINES
  <*> getinfo'ContentL curl cCURLINFO_CONTENT_LENGTH_DOWNLOAD
  <*> getinfo'ContentL curl cCURLINFO_CONTENT_LENGTH_UPLOAD
  <*> getinfo'MString  curl cCURLINFO_CONTENT_TYPE
--  <*> getinfo'String   curl cCURLINFO_PRIVATE
  <*> getinfo'CurlAuth curl cCURLINFO_HTTPAUTH_AVAIL
  <*> getinfo'CurlAuth curl cCURLINFO_PROXYAUTH_AVAIL
  <*> getinfo'Int      curl cCURLINFO_OS_ERRNO
  <*> getinfo'Int      curl cCURLINFO_NUM_CONNECTS
  <*> getinfo'String   curl cCURLINFO_PRIMARY_IP
  <*> getinfo'Int      curl cCURLINFO_PRIMARY_PORT
  <*> getinfo'String   curl cCURLINFO_LOCAL_IP
  <*> getinfo'Int      curl cCURLINFO_LOCAL_PORT
  <*> getinfo'SList    curl cCURLINFO_COOKIELIST
  <*> getinfo'Socket   curl cCURLINFO_LASTSOCKET
  <*> getinfo'MString  curl cCURLINFO_FTP_ENTRY_PATH
  <*> getinfo'CertInfo curl cCURLINFO_CERTINFO
  <*> getinfo'TimeCond curl cCURLINFO_CONDITION_UNMET
  <*> getinfo'MString  curl cCURLINFO_RTSP_SESSION_ID
  <*> getinfo'Int      curl cCURLINFO_RTSP_CLIENT_CSEQ
  <*> getinfo'Int      curl cCURLINFO_RTSP_SERVER_CSEQ
  <*> getinfo'Int      curl cCURLINFO_RTSP_CSEQ_RECV

getinfo'String :: CURL -> CCURLinfo'CString -> IO String
getinfo'String curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'CString curl info ptr
  ifOK code (peek ptr >>= peekCString)

getinfo'MString :: CURL -> CCURLinfo'CString -> IO (Maybe String)
getinfo'MString curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'CString curl info ptr
  ifOK code $ peek ptr >>= \csptr -> if (csptr /= nullPtr)
    then Just <$> peekCString csptr
    else return Nothing

getinfo'Double :: CURL -> CCURLinfo'CDouble -> IO Double
getinfo'Double curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'CDouble curl info ptr
  ifOK code (realToFrac <$> peek ptr)

getinfo'ContentL :: CURL -> CCURLinfo'CDouble -> IO (Maybe Double)
getinfo'ContentL curl info = getinfo'Double curl info >>= \v ->
  return $ if (v == (-1)) then Nothing else Just v

getinfo'SList :: CURL -> CCURLinfo'SList -> IO [String]
getinfo'SList curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'SList curl info ptr
  ifOK code $ peek ptr >>= \slist -> do
    strings <- peek'CCURL_slist slist
    ccurl_slist_free_all slist
    return strings

getinfo'CertInfo :: CURL -> CCURLinfo'CertI -> IO [[String]]
getinfo'CertInfo curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'CertI curl info ptr
  ifOK code (peek ptr >>= peek'CCURL_certinfo)

getinfo'Int :: CURL -> CCURLinfo'CLong -> IO Int
getinfo'Int curl info = alloca $ \ptr -> do
  code <- fromC <$> ccurl_easy_getinfo'CLong curl info ptr
  ifOK code (fromIntegral <$> peek ptr)

getinfo'RespCode :: CURL -> CCURLinfo'CLong -> IO (Maybe Int)
getinfo'RespCode curl info = getinfo'Int curl info >>= \v ->
  return $ if (v == 0) then Nothing else Just v

getinfo'FileTime :: CURL -> CCURLinfo'CLong -> IO (Maybe UTCTime)
getinfo'FileTime curl info = getinfo'Int curl info >>= \v ->
  return $ if (v == (-1) || v == 0) then Nothing
    else Just (posixSecondsToUTCTime $ realToFrac v)

getinfo'Socket :: CURL -> CCURLinfo'CLong -> IO (Maybe Int)
getinfo'Socket curl info = getinfo'Int curl info >>= \v ->
  return $ if (v == (-1)) then Nothing else Just v

getinfo'TimeCond :: CURL -> CCURLinfo'CLong -> IO Bool
getinfo'TimeCond curl info = getinfo'Int curl info >>= \v ->
  return $ if (v == 0) then False else True

getinfo'CurlAuth :: CURL -> CCURLinfo'CLong -> IO [CURLauth]
getinfo'CurlAuth curl info = do
  mask <- fromIntegral <$> getinfo'Int curl info
  return $ mapMaybe (\(v, b) -> if (mask .&. b == 0) then Nothing else Just v)
    [ (CURLAUTH_BASIC       , cCURLAUTH_BASIC       )
    , (CURLAUTH_DIGEST      , cCURLAUTH_DIGEST      )
    , (CURLAUTH_DIGEST_IE   , cCURLAUTH_DIGEST_IE   )
    , (CURLAUTH_GSSNEGOTIATE, cCURLAUTH_GSSNEGOTIATE)
    , (CURLAUTH_NTLM        , cCURLAUTH_NTLM        )
--    , (CURLAUTH_NTLM_WB     , cCURLAUTH_NTLM_WB     )
    ]


peek'CCURL_slist :: Ptr CCURL_slist -> IO [String]
peek'CCURL_slist ptr =
  if (ptr == nullPtr) then return [] else peek ptr >>= \slist -> do
    slist_head <- peekCString      $ ccurl_slist_data slist
    slist_tail <- peek'CCURL_slist $ ccurl_slist_next slist
    return (slist_head : slist_tail)

peek'CCURL_certinfo :: Ptr CCURL_certinfo -> IO [[String]]
peek'CCURL_certinfo ptr =
  if (ptr == nullPtr) then return [] else peek ptr >>= \certinfo -> do
    let numOfCerts = fromIntegral $ ccurl_certinfo_num_of_certs certinfo
    let size = sizeOf (undefined :: Ptr CCURL_slist)
    let ptr0 = ccurl_certinfo_certinfo certinfo
    let ptrs = map (\i -> plusPtr ptr0 (i * size)) [0 .. (numOfCerts - 1)]
    mapM (\sptr -> peek sptr >>= peek'CCURL_slist) ptrs



-------------------------------------------------------------------------------
curl_easy_setopt :: CURLoption opt a => CURL -> opt -> a -> IO ()
curl_easy_setopt = setopt

class CURLoption opt a where
  setopt :: CURL -> opt -> a -> IO ()

instance CURLoption CURLoption'S String where
  setopt curl opt val = withCString val $ \ptr -> do
    code <- fromC <$> ccurl_easy_setopt'String curl (fromH opt) ptr
    ifOK code $ return ()


-------------------------------------------------------------------------------
ifOK :: CURLcode -> IO a -> IO a
ifOK CURLE_OK action = action
ifOK code     _      = throwIO code



{-
-------------------------------------------------------------------------------
infotest0 = do
  curl <- curl_easy_init
  info <- curl_easy_getinfo curl
  curl_easy_cleanup curl
  return info

infotest1 url = do
  curl <- curl_easy_init
  curl_easy_setopt curl CURLOPT_URL url
  ccurl_easy_setopt'Int32 curl cCURLOPT_CERTINFO 1
  curl_easy_perform curl
  info <- curl_easy_getinfo curl
  curl_easy_cleanup curl
  return info
-}

