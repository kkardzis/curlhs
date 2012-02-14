-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Types
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}

module Network.Curlhs.Types
  ( CURL
  , CURLcode (..)
  , CURLinfo (..)
  , CURLoption'S (..)
  , CURLversion (..), CURL_version_info_data (..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Control.Exception
import Data.Typeable

import Data.Time (UTCTime)
import Data.Tuple (swap)
import Data.Bits

import Network.Curlhs.FFI.Types
import Network.Curlhs.FFI.Symbols

import Network.Curlhs.TypesH


-------------------------------------------------------------------------------
type CURL = Ptr CCURL


-------------------------------------------------------------------------------
data CURLcode
  = CURLE_OK
  | CURLE_UNSUPPORTED_PROTOCOL
  | CURLE_FAILED_INIT
  | CURLE_URL_MALFORMAT
  | CURLE_COULDNT_RESOLVE_PROXY
  | CURLE_COULDNT_RESOLVE_HOST
  | CURLE_COULDNT_CONNECT
  | CURLE_FTP_WEIRD_SERVER_REPLY
  | CURLE_REMOTE_ACCESS_DENIED
  | CURLE_FTP_WEIRD_PASS_REPLY
  | CURLE_FTP_WEIRD_PASV_REPLY
  | CURLE_FTP_WEIRD_227_FORMAT
  | CURLE_FTP_CANT_GET_HOST
  | CURLE_FTP_COULDNT_SET_TYPE
  | CURLE_PARTIAL_FILE
  | CURLE_FTP_COULDNT_RETR_FILE
  | CURLE_QUOTE_ERROR
  | CURLE_HTTP_RETURNED_ERROR
  | CURLE_WRITE_ERROR
  | CURLE_UPLOAD_FAILED
  | CURLE_READ_ERROR
  | CURLE_OUT_OF_MEMORY
  | CURLE_OPERATION_TIMEDOUT
  | CURLE_FTP_PORT_FAILED
  | CURLE_FTP_COULDNT_USE_REST
  | CURLE_RANGE_ERROR
  | CURLE_HTTP_POST_ERROR
  | CURLE_SSL_CONNECT_ERROR
  | CURLE_BAD_DOWNLOAD_RESUME
  | CURLE_FILE_COULDNT_READ_FILE
  | CURLE_LDAP_CANNOT_BIND
  | CURLE_LDAP_SEARCH_FAILED
  | CURLE_FUNCTION_NOT_FOUND
  | CURLE_ABORTED_BY_CALLBACK
  | CURLE_BAD_FUNCTION_ARGUMENT
  | CURLE_INTERFACE_FAILED
  | CURLE_TOO_MANY_REDIRECTS
  | CURLE_UNKNOWN_TELNET_OPTION
  | CURLE_TELNET_OPTION_SYNTAX
  | CURLE_PEER_FAILED_VERIFICATION
  | CURLE_GOT_NOTHING
  | CURLE_SSL_ENGINE_NOTFOUND
  | CURLE_SSL_ENGINE_SETFAILED
  | CURLE_SEND_ERROR
  | CURLE_RECV_ERROR
  | CURLE_SSL_CERTPROBLEM
  | CURLE_SSL_CIPHER
  | CURLE_SSL_CACERT
  | CURLE_BAD_CONTENT_ENCODING
  | CURLE_LDAP_INVALID_URL
  | CURLE_FILESIZE_EXCEEDED
  | CURLE_USE_SSL_FAILED
  | CURLE_SEND_FAIL_REWIND
  | CURLE_SSL_ENGINE_INITFAILED
  | CURLE_LOGIN_DENIED
  | CURLE_TFTP_NOTFOUND
  | CURLE_TFTP_PERM
  | CURLE_REMOTE_DISK_FULL
  | CURLE_TFTP_ILLEGAL
  | CURLE_TFTP_UNKNOWNID
  | CURLE_REMOTE_FILE_EXISTS
  | CURLE_TFTP_NOSUCHUSER
  | CURLE_CONV_FAILED
  | CURLE_CONV_REQD
  | CURLE_SSL_CACERT_BADFILE
  | CURLE_REMOTE_FILE_NOT_FOUND
  | CURLE_SSH
  | CURLE_SSL_SHUTDOWN_FAILED
  | CURLE_AGAIN
  | CURLE_SSL_CRL_BADFILE
  | CURLE_SSL_ISSUER_ERROR
  | CURLE_FTP_PRET_FAILED
  | CURLE_RTSP_CSEQ_ERROR
  | CURLE_RTSP_SESSION_ERROR
  | CURLE_FTP_BAD_FILE_LIST
  | CURLE_CHUNK_FAILED
  deriving (Eq, Show, Typeable)

instance Exception CURLcode

instance FromC CCURLcode CURLcode where
  fromC x = findWithDef (cError "CCURLcode") x $ map swap knownCURLcode

instance FromH CURLcode CCURLcode where
  fromH x = findWithDef (hError "CURLcode") x knownCURLcode

knownCURLcode :: [(CURLcode, CCURLcode)]
knownCURLcode =
  [ (CURLE_OK                      , cCURLE_OK                      )
  , (CURLE_UNSUPPORTED_PROTOCOL    , cCURLE_UNSUPPORTED_PROTOCOL    )
  , (CURLE_FAILED_INIT             , cCURLE_FAILED_INIT             )
  , (CURLE_URL_MALFORMAT           , cCURLE_URL_MALFORMAT           )
  , (CURLE_COULDNT_RESOLVE_PROXY   , cCURLE_COULDNT_RESOLVE_PROXY   )
  , (CURLE_COULDNT_RESOLVE_HOST    , cCURLE_COULDNT_RESOLVE_HOST    )
  , (CURLE_COULDNT_CONNECT         , cCURLE_COULDNT_CONNECT         )
  , (CURLE_FTP_WEIRD_SERVER_REPLY  , cCURLE_FTP_WEIRD_SERVER_REPLY  )
  , (CURLE_REMOTE_ACCESS_DENIED    , cCURLE_REMOTE_ACCESS_DENIED    )
  , (CURLE_FTP_WEIRD_PASS_REPLY    , cCURLE_FTP_WEIRD_PASS_REPLY    )
  , (CURLE_FTP_WEIRD_PASV_REPLY    , cCURLE_FTP_WEIRD_PASV_REPLY    )
  , (CURLE_FTP_WEIRD_227_FORMAT    , cCURLE_FTP_WEIRD_227_FORMAT    )
  , (CURLE_FTP_CANT_GET_HOST       , cCURLE_FTP_CANT_GET_HOST       )
  , (CURLE_FTP_COULDNT_SET_TYPE    , cCURLE_FTP_COULDNT_SET_TYPE    )
  , (CURLE_PARTIAL_FILE            , cCURLE_PARTIAL_FILE            )
  , (CURLE_FTP_COULDNT_RETR_FILE   , cCURLE_FTP_COULDNT_RETR_FILE   )
  , (CURLE_QUOTE_ERROR             , cCURLE_QUOTE_ERROR             )
  , (CURLE_HTTP_RETURNED_ERROR     , cCURLE_HTTP_RETURNED_ERROR     )
  , (CURLE_WRITE_ERROR             , cCURLE_WRITE_ERROR             )
  , (CURLE_UPLOAD_FAILED           , cCURLE_UPLOAD_FAILED           )
  , (CURLE_READ_ERROR              , cCURLE_READ_ERROR              )
  , (CURLE_OUT_OF_MEMORY           , cCURLE_OUT_OF_MEMORY           )
  , (CURLE_OPERATION_TIMEDOUT      , cCURLE_OPERATION_TIMEDOUT      )
  , (CURLE_FTP_PORT_FAILED         , cCURLE_FTP_PORT_FAILED         )
  , (CURLE_FTP_COULDNT_USE_REST    , cCURLE_FTP_COULDNT_USE_REST    )
  , (CURLE_RANGE_ERROR             , cCURLE_RANGE_ERROR             )
  , (CURLE_HTTP_POST_ERROR         , cCURLE_HTTP_POST_ERROR         )
  , (CURLE_SSL_CONNECT_ERROR       , cCURLE_SSL_CONNECT_ERROR       )
  , (CURLE_BAD_DOWNLOAD_RESUME     , cCURLE_BAD_DOWNLOAD_RESUME     )
  , (CURLE_FILE_COULDNT_READ_FILE  , cCURLE_FILE_COULDNT_READ_FILE  )
  , (CURLE_LDAP_CANNOT_BIND        , cCURLE_LDAP_CANNOT_BIND        )
  , (CURLE_LDAP_SEARCH_FAILED      , cCURLE_LDAP_SEARCH_FAILED      )
  , (CURLE_FUNCTION_NOT_FOUND      , cCURLE_FUNCTION_NOT_FOUND      )
  , (CURLE_ABORTED_BY_CALLBACK     , cCURLE_ABORTED_BY_CALLBACK     )
  , (CURLE_BAD_FUNCTION_ARGUMENT   , cCURLE_BAD_FUNCTION_ARGUMENT   )
  , (CURLE_INTERFACE_FAILED        , cCURLE_INTERFACE_FAILED        )
  , (CURLE_TOO_MANY_REDIRECTS      , cCURLE_TOO_MANY_REDIRECTS      )
  , (CURLE_UNKNOWN_TELNET_OPTION   , cCURLE_UNKNOWN_TELNET_OPTION   )
  , (CURLE_TELNET_OPTION_SYNTAX    , cCURLE_TELNET_OPTION_SYNTAX    )
  , (CURLE_PEER_FAILED_VERIFICATION, cCURLE_PEER_FAILED_VERIFICATION)
  , (CURLE_GOT_NOTHING             , cCURLE_GOT_NOTHING             )
  , (CURLE_SSL_ENGINE_NOTFOUND     , cCURLE_SSL_ENGINE_NOTFOUND     )
  , (CURLE_SSL_ENGINE_SETFAILED    , cCURLE_SSL_ENGINE_SETFAILED    )
  , (CURLE_SEND_ERROR              , cCURLE_SEND_ERROR              )
  , (CURLE_RECV_ERROR              , cCURLE_RECV_ERROR              )
  , (CURLE_SSL_CERTPROBLEM         , cCURLE_SSL_CERTPROBLEM         )
  , (CURLE_SSL_CIPHER              , cCURLE_SSL_CIPHER              )
  , (CURLE_SSL_CACERT              , cCURLE_SSL_CACERT              )
  , (CURLE_BAD_CONTENT_ENCODING    , cCURLE_BAD_CONTENT_ENCODING    )
  , (CURLE_LDAP_INVALID_URL        , cCURLE_LDAP_INVALID_URL        )
  , (CURLE_FILESIZE_EXCEEDED       , cCURLE_FILESIZE_EXCEEDED       )
  , (CURLE_USE_SSL_FAILED          , cCURLE_USE_SSL_FAILED          )
  , (CURLE_SEND_FAIL_REWIND        , cCURLE_SEND_FAIL_REWIND        )
  , (CURLE_SSL_ENGINE_INITFAILED   , cCURLE_SSL_ENGINE_INITFAILED   )
  , (CURLE_LOGIN_DENIED            , cCURLE_LOGIN_DENIED            )
  , (CURLE_TFTP_NOTFOUND           , cCURLE_TFTP_NOTFOUND           )
  , (CURLE_TFTP_PERM               , cCURLE_TFTP_PERM               )
  , (CURLE_REMOTE_DISK_FULL        , cCURLE_REMOTE_DISK_FULL        )
  , (CURLE_TFTP_ILLEGAL            , cCURLE_TFTP_ILLEGAL            )
  , (CURLE_TFTP_UNKNOWNID          , cCURLE_TFTP_UNKNOWNID          )
  , (CURLE_REMOTE_FILE_EXISTS      , cCURLE_REMOTE_FILE_EXISTS      )
  , (CURLE_TFTP_NOSUCHUSER         , cCURLE_TFTP_NOSUCHUSER         )
  , (CURLE_CONV_FAILED             , cCURLE_CONV_FAILED             )
  , (CURLE_CONV_REQD               , cCURLE_CONV_REQD               )
  , (CURLE_SSL_CACERT_BADFILE      , cCURLE_SSL_CACERT_BADFILE      )
  , (CURLE_REMOTE_FILE_NOT_FOUND   , cCURLE_REMOTE_FILE_NOT_FOUND   )
  , (CURLE_SSH                     , cCURLE_SSH                     )
  , (CURLE_SSL_SHUTDOWN_FAILED     , cCURLE_SSL_SHUTDOWN_FAILED     )
  , (CURLE_AGAIN                   , cCURLE_AGAIN                   )
  , (CURLE_SSL_CRL_BADFILE         , cCURLE_SSL_CRL_BADFILE         )
  , (CURLE_SSL_ISSUER_ERROR        , cCURLE_SSL_ISSUER_ERROR        )
  , (CURLE_FTP_PRET_FAILED         , cCURLE_FTP_PRET_FAILED         )
  , (CURLE_RTSP_CSEQ_ERROR         , cCURLE_RTSP_CSEQ_ERROR         )
  , (CURLE_RTSP_SESSION_ERROR      , cCURLE_RTSP_SESSION_ERROR      )
  , (CURLE_FTP_BAD_FILE_LIST       , cCURLE_FTP_BAD_FILE_LIST       )
  , (CURLE_CHUNK_FAILED            , cCURLE_CHUNK_FAILED            )
  ]



-------------------------------------------------------------------------------
data CURLoption'S
  = CURLOPT_URL
  deriving (Eq, Show)

instance FromC CCURLoption'String CURLoption'S where
  fromC x = findWithDef (cError "CCURLoption'S") x $ map swap knownCURLoption'S

instance FromH CURLoption'S CCURLoption'String where
  fromH x = findWithDef (hError "CURLoption'S") x knownCURLoption'S

knownCURLoption'S :: [(CURLoption'S, CCURLoption'String)]
knownCURLoption'S =
  [ (CURLOPT_URL  , cCURLOPT_URL  )
  ]



-------------------------------------------------------------------------------
data CURLinfo = CURLinfo
  { curlinfo_effective_url           :: String
  , curlinfo_response_code           :: Maybe Int
  , curlinfo_http_connectcode        :: Maybe Int
  , curlinfo_filetime                :: Maybe UTCTime
  , curlinfo_total_time              :: Double
  , curlinfo_namelookup_time         :: Double
  , curlinfo_connect_time            :: Double
  , curlinfo_appconnect_time         :: Double
  , curlinfo_pretransfer_time        :: Double
  , curlinfo_starttransfer_time      :: Double
  , curlinfo_redirect_time           :: Double
  , curlinfo_redirect_count          :: Int
  , curlinfo_redirect_url            :: Maybe String
  , curlinfo_size_upload             :: Double
  , curlinfo_size_download           :: Double
  , curlinfo_speed_download          :: Double
  , curlinfo_speed_upload            :: Double
  , curlinfo_header_size             :: Int  -- Int32??
  , curlinfo_request_size            :: Int  -- Int32??
  , curlinfo_ssl_verifyresult        :: Int  -- Bool??
  , curlinfo_ssl_engines             :: [String]
  , curlinfo_content_length_download :: Maybe Double
  , curlinfo_content_length_upload   :: Maybe Double
  , curlinfo_content_type            :: Maybe String
--  , curlinfo_private                 :: String   -- void??
--  , curlinfo_httpauth_avail          :: Int      -- [CURLauth]
--  , curlinfo_proxyauth_avail         :: Int      -- [CURLauth]
--  , curlinfo_os_errno                :: Int      -- Maybe Int??
  , curlinfo_num_connects            :: Int
  , curlinfo_primary_ip              :: String
  , curlinfo_primary_port            :: Int
  , curlinfo_local_ip                :: String
  , curlinfo_local_port              :: Int
  , curlinfo_cookielist              :: [String] -- Maybe [String]??
--  , curlinfo_lastsocket              :: Int      -- Maybe Int/CURLsocket??
  , curlinfo_ftp_entry_path          :: Maybe String
--  , curlinfo_certinfo                :: [String] -- curl_certinfo??
  , curlinfo_condition_unmet         :: Bool
  , curlinfo_rtsp_session_id         :: Maybe String
  , curlinfo_rtsp_client_cseq        :: Int  --Int32??
  , curlinfo_rtsp_server_cseq        :: Int  --Int32??
  , curlinfo_rtsp_cseq_recv          :: Int  --Int32??
  } deriving (Show)



-------------------------------------------------------------------------------
data CURLversion
  = CURLVERSION_FIRST
  | CURLVERSION_SECOND
  | CURLVERSION_THIRD
  | CURLVERSION_FOURTH
  | CURLVERSION_NOW
  deriving (Eq, Show)

instance FromC CCURLversion CURLversion where
  fromC x = findWithDef (cError "CCURLversion") x $ map swap knownCURLversion

instance FromH CURLversion CCURLversion where
  fromH x = findWithDef (hError "CURLversion") x knownCURLversion

knownCURLversion :: [(CURLversion, CCURLversion)]
knownCURLversion =
  [ (CURLVERSION_FIRST , cCURLVERSION_FIRST )
  , (CURLVERSION_SECOND, cCURLVERSION_SECOND)
  , (CURLVERSION_THIRD , cCURLVERSION_THIRD )
  , (CURLVERSION_FOURTH, cCURLVERSION_FOURTH)
  , (CURLVERSION_NOW   , cCURLVERSION_NOW   )
  ]


-------------------------------------------------------------------------------
data CURL_version_info_data = CURL_version_info_data
  { curl_version_info_data_age             :: CURLversion
  , curl_version_info_data_version         :: String
  , curl_version_info_data_version_num     :: (Int, Int, Int)
  , curl_version_info_data_host            :: String
  , curl_version_info_data_features        :: Int
  , curl_version_info_data_ssl_version     :: Maybe String
  , curl_version_info_data_ssl_version_num :: Int
  , curl_version_info_data_libz_version    :: Maybe String
  , curl_version_info_data_protocols       :: [String]
  , curl_version_info_data_ares            :: Maybe String
  , curl_version_info_data_ares_num        :: Int
  , curl_version_info_data_libidn          :: Maybe String
  , curl_version_info_data_iconv_ver_num   :: Int
  , curl_version_info_data_libssh_version  :: Maybe String
  } deriving (Show)

instance PeekCCURL CCURL_version_info_data CURL_version_info_data where
  peekCCURL ptr = peek ptr >>= \cval -> CURL_version_info_data
    <$> (return $ fromC   $ ccurl_version_info_data_age             cval)
    <*> (peekCString      $ ccurl_version_info_data_version         cval)
    <*> (peekCVersionNum  $ ccurl_version_info_data_version_num     cval)
    <*> (peekCString      $ ccurl_version_info_data_host            cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_features        cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_ssl_version     cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_ssl_version_num cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libz_version    cval)
    <*> (peekCStringList  $ ccurl_version_info_data_protocols       cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_ares            cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_ares_num        cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libidn          cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_iconv_ver_num   cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libssh_version  cval)

peekCVersionNum :: CUInt -> IO (Int, Int, Int)
peekCVersionNum num = return (major, minor, patch)
  where
    major = fromIntegral $ shiftR (num .&. 0xff0000) 16
    minor = fromIntegral $ shiftR (num .&. 0x00ff00) 8
    patch = fromIntegral $ shiftR (num .&. 0x0000ff) 0

peekCStringList :: Ptr CString -> IO [String]
peekCStringList ptr = do
  cstring <- peek ptr
  if (cstring == nullPtr)
    then return []
    else do
      strings <- peekCStringList (plusPtr ptr (sizeOf ptr))
      string  <- peekCString cstring
      return (string : strings)

peekCStringMaybe :: CString -> IO (Maybe String)
peekCStringMaybe ptr =
  if (ptr == nullPtr)
    then return Nothing
    else peekCString ptr >>= return . Just

peekCIntegral :: (Num h, Integral c) => c -> IO h
peekCIntegral = return . fromIntegral

