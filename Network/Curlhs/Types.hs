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

module Network.Curlhs.Types where

import Foreign.Ptr (Ptr, FunPtr)

import Data.ByteString (ByteString)
import Data.Typeable   (Typeable)
import Data.IORef      (IORef)
import Data.Tuple      (swap)
import Data.Time       (UTCTime)

import Control.Exception (Exception)

import Network.Curlhs.Base


-------------------------------------------------------------------------------
class FromC c h where
  fromC :: c -> h

class FromH h c where
  fromH :: h -> c

findWithDef :: (Eq a) => b -> a -> [(a, b)] -> b
findWithDef def key table = maybe def id $ lookup key table

cError :: String -> a
cError s = error $ "FromC "++s++": bad argument"

hError :: String -> a
hError s = error $ "FromH "++s++": incomplete lookup table"




-------------------------------------------------------------------------------
data CURL = CURL
  { ccurlptr :: Ptr CCURL
  , cb_write :: IORef (Maybe (FunPtr CCURL_write_callback))
  , cb_read  :: IORef (Maybe (FunPtr CCURL_read_callback ))
  }


-------------------------------------------------------------------------------
data CURLcode
  = CURLE_OK
  | CURLE_UNSUPPORTED_PROTOCOL
  | CURLE_FAILED_INIT
  | CURLE_URL_MALFORMAT
  | CURLE_NOT_BUILT_IN             |7215:----|
  | CURLE_COULDNT_RESOLVE_PROXY
  | CURLE_COULDNT_RESOLVE_HOST
  | CURLE_COULDNT_CONNECT
  | CURLE_FTP_WEIRD_SERVER_REPLY
  | CURLE_REMOTE_ACCESS_DENIED
  | CURLE_FTP_ACCEPT_FAILED        |7240:----|
  | CURLE_FTP_WEIRD_PASS_REPLY
  | CURLE_FTP_ACCEPT_TIMEOUT       |7240:----|
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
  | CURLE_UNKNOWN_TELNET_OPTION    |----:7214|
  | CURLE_UNKNOWN_OPTION           |7215:----|
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
  | CURLE_FTP_BAD_FILE_LIST        |7210:----|
  | CURLE_CHUNK_FAILED             |7210:----|
  deriving (Eq, Show, Typeable)

instance Exception CURLcode

instance FromC CCURLcode CURLcode where
  fromC x = findWithDef (cError "CCURLcode") x $ map swap knownCURLcode

instance FromH CURLcode CCURLcode where
  fromH x = findWithDef (hError "CURLcode") x knownCURLcode

knownCURLcode :: [(CURLcode, CCURLcode)]
knownCURLcode =
  [(CURLE_OK                      ,cCURLE_OK                      )
  ,(CURLE_UNSUPPORTED_PROTOCOL    ,cCURLE_UNSUPPORTED_PROTOCOL    )
  ,(CURLE_FAILED_INIT             ,cCURLE_FAILED_INIT             )
  ,(CURLE_URL_MALFORMAT           ,cCURLE_URL_MALFORMAT           )
  ,(CURLE_NOT_BUILT_IN            ,cCURLE_NOT_BUILT_IN            )|7215:----|
  ,(CURLE_COULDNT_RESOLVE_PROXY   ,cCURLE_COULDNT_RESOLVE_PROXY   )
  ,(CURLE_COULDNT_RESOLVE_HOST    ,cCURLE_COULDNT_RESOLVE_HOST    )
  ,(CURLE_COULDNT_CONNECT         ,cCURLE_COULDNT_CONNECT         )
  ,(CURLE_FTP_WEIRD_SERVER_REPLY  ,cCURLE_FTP_WEIRD_SERVER_REPLY  )
  ,(CURLE_REMOTE_ACCESS_DENIED    ,cCURLE_REMOTE_ACCESS_DENIED    )
  ,(CURLE_FTP_ACCEPT_FAILED       ,cCURLE_FTP_ACCEPT_FAILED       )|7240:----|
  ,(CURLE_FTP_WEIRD_PASS_REPLY    ,cCURLE_FTP_WEIRD_PASS_REPLY    )
  ,(CURLE_FTP_ACCEPT_TIMEOUT      ,cCURLE_FTP_ACCEPT_TIMEOUT      )|7240:----|
  ,(CURLE_FTP_WEIRD_PASV_REPLY    ,cCURLE_FTP_WEIRD_PASV_REPLY    )
  ,(CURLE_FTP_WEIRD_227_FORMAT    ,cCURLE_FTP_WEIRD_227_FORMAT    )
  ,(CURLE_FTP_CANT_GET_HOST       ,cCURLE_FTP_CANT_GET_HOST       )
  ,(CURLE_FTP_COULDNT_SET_TYPE    ,cCURLE_FTP_COULDNT_SET_TYPE    )
  ,(CURLE_PARTIAL_FILE            ,cCURLE_PARTIAL_FILE            )
  ,(CURLE_FTP_COULDNT_RETR_FILE   ,cCURLE_FTP_COULDNT_RETR_FILE   )
  ,(CURLE_QUOTE_ERROR             ,cCURLE_QUOTE_ERROR             )
  ,(CURLE_HTTP_RETURNED_ERROR     ,cCURLE_HTTP_RETURNED_ERROR     )
  ,(CURLE_WRITE_ERROR             ,cCURLE_WRITE_ERROR             )
  ,(CURLE_UPLOAD_FAILED           ,cCURLE_UPLOAD_FAILED           )
  ,(CURLE_READ_ERROR              ,cCURLE_READ_ERROR              )
  ,(CURLE_OUT_OF_MEMORY           ,cCURLE_OUT_OF_MEMORY           )
  ,(CURLE_OPERATION_TIMEDOUT      ,cCURLE_OPERATION_TIMEDOUT      )
  ,(CURLE_FTP_PORT_FAILED         ,cCURLE_FTP_PORT_FAILED         )
  ,(CURLE_FTP_COULDNT_USE_REST    ,cCURLE_FTP_COULDNT_USE_REST    )
  ,(CURLE_RANGE_ERROR             ,cCURLE_RANGE_ERROR             )
  ,(CURLE_HTTP_POST_ERROR         ,cCURLE_HTTP_POST_ERROR         )
  ,(CURLE_SSL_CONNECT_ERROR       ,cCURLE_SSL_CONNECT_ERROR       )
  ,(CURLE_BAD_DOWNLOAD_RESUME     ,cCURLE_BAD_DOWNLOAD_RESUME     )
  ,(CURLE_FILE_COULDNT_READ_FILE  ,cCURLE_FILE_COULDNT_READ_FILE  )
  ,(CURLE_LDAP_CANNOT_BIND        ,cCURLE_LDAP_CANNOT_BIND        )
  ,(CURLE_LDAP_SEARCH_FAILED      ,cCURLE_LDAP_SEARCH_FAILED      )
  ,(CURLE_FUNCTION_NOT_FOUND      ,cCURLE_FUNCTION_NOT_FOUND      )
  ,(CURLE_ABORTED_BY_CALLBACK     ,cCURLE_ABORTED_BY_CALLBACK     )
  ,(CURLE_BAD_FUNCTION_ARGUMENT   ,cCURLE_BAD_FUNCTION_ARGUMENT   )
  ,(CURLE_INTERFACE_FAILED        ,cCURLE_INTERFACE_FAILED        )
  ,(CURLE_TOO_MANY_REDIRECTS      ,cCURLE_TOO_MANY_REDIRECTS      )
  ,(CURLE_UNKNOWN_TELNET_OPTION   ,cCURLE_UNKNOWN_TELNET_OPTION   )|----:7214|
  ,(CURLE_UNKNOWN_OPTION          ,cCURLE_UNKNOWN_OPTION          )|7215:----|
  ,(CURLE_TELNET_OPTION_SYNTAX    ,cCURLE_TELNET_OPTION_SYNTAX    )
  ,(CURLE_PEER_FAILED_VERIFICATION,cCURLE_PEER_FAILED_VERIFICATION)
  ,(CURLE_GOT_NOTHING             ,cCURLE_GOT_NOTHING             )
  ,(CURLE_SSL_ENGINE_NOTFOUND     ,cCURLE_SSL_ENGINE_NOTFOUND     )
  ,(CURLE_SSL_ENGINE_SETFAILED    ,cCURLE_SSL_ENGINE_SETFAILED    )
  ,(CURLE_SEND_ERROR              ,cCURLE_SEND_ERROR              )
  ,(CURLE_RECV_ERROR              ,cCURLE_RECV_ERROR              )
  ,(CURLE_SSL_CERTPROBLEM         ,cCURLE_SSL_CERTPROBLEM         )
  ,(CURLE_SSL_CIPHER              ,cCURLE_SSL_CIPHER              )
  ,(CURLE_SSL_CACERT              ,cCURLE_SSL_CACERT              )
  ,(CURLE_BAD_CONTENT_ENCODING    ,cCURLE_BAD_CONTENT_ENCODING    )
  ,(CURLE_LDAP_INVALID_URL        ,cCURLE_LDAP_INVALID_URL        )
  ,(CURLE_FILESIZE_EXCEEDED       ,cCURLE_FILESIZE_EXCEEDED       )
  ,(CURLE_USE_SSL_FAILED          ,cCURLE_USE_SSL_FAILED          )
  ,(CURLE_SEND_FAIL_REWIND        ,cCURLE_SEND_FAIL_REWIND        )
  ,(CURLE_SSL_ENGINE_INITFAILED   ,cCURLE_SSL_ENGINE_INITFAILED   )
  ,(CURLE_LOGIN_DENIED            ,cCURLE_LOGIN_DENIED            )
  ,(CURLE_TFTP_NOTFOUND           ,cCURLE_TFTP_NOTFOUND           )
  ,(CURLE_TFTP_PERM               ,cCURLE_TFTP_PERM               )
  ,(CURLE_REMOTE_DISK_FULL        ,cCURLE_REMOTE_DISK_FULL        )
  ,(CURLE_TFTP_ILLEGAL            ,cCURLE_TFTP_ILLEGAL            )
  ,(CURLE_TFTP_UNKNOWNID          ,cCURLE_TFTP_UNKNOWNID          )
  ,(CURLE_REMOTE_FILE_EXISTS      ,cCURLE_REMOTE_FILE_EXISTS      )
  ,(CURLE_TFTP_NOSUCHUSER         ,cCURLE_TFTP_NOSUCHUSER         )
  ,(CURLE_CONV_FAILED             ,cCURLE_CONV_FAILED             )
  ,(CURLE_CONV_REQD               ,cCURLE_CONV_REQD               )
  ,(CURLE_SSL_CACERT_BADFILE      ,cCURLE_SSL_CACERT_BADFILE      )
  ,(CURLE_REMOTE_FILE_NOT_FOUND   ,cCURLE_REMOTE_FILE_NOT_FOUND   )
  ,(CURLE_SSH                     ,cCURLE_SSH                     )
  ,(CURLE_SSL_SHUTDOWN_FAILED     ,cCURLE_SSL_SHUTDOWN_FAILED     )
  ,(CURLE_AGAIN                   ,cCURLE_AGAIN                   )
  ,(CURLE_SSL_CRL_BADFILE         ,cCURLE_SSL_CRL_BADFILE         )
  ,(CURLE_SSL_ISSUER_ERROR        ,cCURLE_SSL_ISSUER_ERROR        )
  ,(CURLE_FTP_PRET_FAILED         ,cCURLE_FTP_PRET_FAILED         )
  ,(CURLE_RTSP_CSEQ_ERROR         ,cCURLE_RTSP_CSEQ_ERROR         )
  ,(CURLE_RTSP_SESSION_ERROR      ,cCURLE_RTSP_SESSION_ERROR      )
  ,(CURLE_FTP_BAD_FILE_LIST       ,cCURLE_FTP_BAD_FILE_LIST       )|7210:----|
  ,(CURLE_CHUNK_FAILED            ,cCURLE_CHUNK_FAILED            )|7210:----|
  ]



-------------------------------------------------------------------------------
data CURLauth
  = CURLAUTH_BASIC
  | CURLAUTH_DIGEST
  | CURLAUTH_DIGEST_IE
  | CURLAUTH_GSSNEGOTIATE
  | CURLAUTH_NTLM
  | CURLAUTH_NTLM_WB
  deriving (Eq, Show)


-------------------------------------------------------------------------------
type CURL_write_callback = ByteString -> IO CURL_write_response

data CURL_write_response
  = CURL_WRITEFUNC_OK
  | CURL_WRITEFUNC_FAIL
  | CURL_WRITEFUNC_PAUSE
  deriving (Eq)


type CURL_read_callback = Int -> IO CURL_read_response

data CURL_read_response
  = CURL_READFUNC_OK ByteString
  | CURL_READFUNC_ABORT
  | CURL_READFUNC_PAUSE
  deriving (Eq)


-------------------------------------------------------------------------------
data CURLoption
-- BEHAVIOR OPTIONS
  = CURLOPT_VERBOSE       Bool
  | CURLOPT_HEADER        Bool
  | CURLOPT_NOPROGRESS    Bool
  | CURLOPT_NOSIGNAL      Bool
  | CURLOPT_WILDCARDMATCH Bool

-- CALLBACK OPTIONS
  | CURLOPT_WRITEFUNCTION (Maybe CURL_write_callback)
  | CURLOPT_WRITEDATA
  | CURLOPT_READFUNCTION (Maybe CURL_read_callback)
  | CURLOPT_READDATA
  | CURLOPT_IOCTLFUNCTION
  | CURLOPT_IOCTLDATA
  | CURLOPT_SEEKFUNCTION
  | CURLOPT_SEEKDATA
  | CURLOPT_SOCKOPTFUNCTION
  | CURLOPT_SOCKOPTDATA
  | CURLOPT_OPENSOCKETFUNCTION
  | CURLOPT_OPENSOCKETDATA
  | CURLOPT_CLOSESOCKETFUNCTION
  | CURLOPT_CLOSESOCKETDATA
  | CURLOPT_PROGRESSFUNCTION
  | CURLOPT_PROGRESSDATA
  | CURLOPT_HEADERFUNCTION
  | CURLOPT_HEADERDATA
  | CURLOPT_DEBUGFUNCTION
  | CURLOPT_DEBUGDATA
  | CURLOPT_SSL_CTX_FUNCTION
  | CURLOPT_SSL_CTX_DATA
  | CURLOPT_CONV_TO_NETWORK_FUNCTION
  | CURLOPT_CONV_FROM_NETWORK_FUNCTION
  | CURLOPT_CONV_FROM_UTF8_FUNCTION
  | CURLOPT_INTERLEAVEFUNCTION
  | CURLOPT_INTERLEAVEDATA
  | CURLOPT_CHUNK_BGN_FUNCTION
  | CURLOPT_CHUNK_END_FUNCTION
  | CURLOPT_CHUNK_DATA
  | CURLOPT_FNMATCH_FUNCTION
  | CURLOPT_FNMATCH_DATA

-- ERROR OPTIONS

-- NETWORK OPTIONS
  | CURLOPT_URL String

-- NAMES and PASSWORDS OPTIONS (Authentication)
-- HTTP OPTIONS
-- SMTP OPTIONS
-- TFTP OPTIONS
-- FTP OPTIONS
-- RTSP OPTIONS
-- PROTOCOL OPTIONS
-- CONNECTION OPTIONS
-- SSL and SECURITY OPTIONS
-- SSH OPTIONS
-- OTHER OPTIONS
-- TELNET OPTIONS
-- SMTP OPTIONS








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
  , curlinfo_header_size             :: Int
  , curlinfo_request_size            :: Int
  , curlinfo_ssl_verifyresult        :: Int  -- Bool??
  , curlinfo_ssl_engines             :: [String]
  , curlinfo_content_length_download :: Maybe Double
  , curlinfo_content_length_upload   :: Maybe Double
  , curlinfo_content_type            :: Maybe String
--  , curlinfo_private                 :: String   -- void??
  , curlinfo_httpauth_avail          :: [CURLauth]
  , curlinfo_proxyauth_avail         :: [CURLauth]
  , curlinfo_os_errno                :: Int
  , curlinfo_num_connects            :: Int
  , curlinfo_primary_ip              :: String
  , curlinfo_primary_port            :: Int
  , curlinfo_local_ip                :: String
  , curlinfo_local_port              :: Int
  , curlinfo_cookielist              :: [String]
  , curlinfo_lastsocket              :: Maybe Int
  , curlinfo_ftp_entry_path          :: Maybe String
  , curlinfo_certinfo                :: [[String]]
  , curlinfo_condition_unmet         :: Bool
  , curlinfo_rtsp_session_id         :: Maybe String
  , curlinfo_rtsp_client_cseq        :: Int
  , curlinfo_rtsp_server_cseq        :: Int
  , curlinfo_rtsp_cseq_recv          :: Int
  } deriving (Show)



-------------------------------------------------------------------------------
data CURL_version_info_data = CURL_version_info_data
  { curl_version_info_data_version         :: String
  , curl_version_info_data_version_num     :: Int
  , curl_version_info_data_host            :: String
  , curl_version_info_data_features        :: [CURL_version]
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


-------------------------------------------------------------------------------
data CURL_version
  = CURL_VERSION_IPV6
  | CURL_VERSION_KERBEROS4
  | CURL_VERSION_SSL
  | CURL_VERSION_LIBZ
  | CURL_VERSION_NTLM
  | CURL_VERSION_GSSNEGOTIATE
  | CURL_VERSION_DEBUG
  | CURL_VERSION_ASYNCHDNS
  | CURL_VERSION_SPNEGO
  | CURL_VERSION_LARGEFILE
  | CURL_VERSION_IDN
  | CURL_VERSION_SSPI
  | CURL_VERSION_CONV
  | CURL_VERSION_CURLDEBUG
  | CURL_VERSION_TLSAUTH_SRP
  | CURL_VERSION_NTLM_WB
  deriving (Eq, Show)


