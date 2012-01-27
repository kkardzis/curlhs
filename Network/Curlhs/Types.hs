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
  , CURLinfo'S (..)
  , CURLinfo'I (..)
  , CURLinfo'D (..)
  , CURLinfo'L (..)
  , CURLversion (..), CURL_version_info_data (..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Control.Exception
import Data.Typeable

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
  | CURLE_OBSOLETE4
  | CURLE_COULDNT_RESOLVE_PROXY
  | CURLE_COULDNT_RESOLVE_HOST
  | CURLE_COULDNT_CONNECT
  | CURLE_FTP_WEIRD_SERVER_REPLY
  | CURLE_REMOTE_ACCESS_DENIED
  | CURLE_OBSOLETE10
  | CURLE_FTP_WEIRD_PASS_REPLY
  | CURLE_OBSOLETE12
  | CURLE_FTP_WEIRD_PASV_REPLY
  | CURLE_FTP_WEIRD_227_FORMAT
  | CURLE_FTP_CANT_GET_HOST
  | CURLE_OBSOLETE16
  | CURLE_FTP_COULDNT_SET_TYPE
  | CURLE_PARTIAL_FILE
  | CURLE_FTP_COULDNT_RETR_FILE
  | CURLE_OBSOLETE20
  | CURLE_QUOTE_ERROR
  | CURLE_HTTP_RETURNED_ERROR
  | CURLE_WRITE_ERROR
  | CURLE_OBSOLETE24
  | CURLE_UPLOAD_FAILED
  | CURLE_READ_ERROR
  | CURLE_OUT_OF_MEMORY
  | CURLE_OPERATION_TIMEDOUT
  | CURLE_OBSOLETE29
  | CURLE_FTP_PORT_FAILED
  | CURLE_FTP_COULDNT_USE_REST
  | CURLE_OBSOLETE32
  | CURLE_RANGE_ERROR
  | CURLE_HTTP_POST_ERROR
  | CURLE_SSL_CONNECT_ERROR
  | CURLE_BAD_DOWNLOAD_RESUME
  | CURLE_FILE_COULDNT_READ_FILE
  | CURLE_LDAP_CANNOT_BIND
  | CURLE_LDAP_SEARCH_FAILED
  | CURLE_OBSOLETE40
  | CURLE_FUNCTION_NOT_FOUND
  | CURLE_ABORTED_BY_CALLBACK
  | CURLE_BAD_FUNCTION_ARGUMENT
  | CURLE_OBSOLETE44
  | CURLE_INTERFACE_FAILED
  | CURLE_OBSOLETE46
  | CURLE_TOO_MANY_REDIRECTS
  | CURLE_UNKNOWN_TELNET_OPTION
  | CURLE_TELNET_OPTION_SYNTAX
  | CURLE_OBSOLETE50
  | CURLE_PEER_FAILED_VERIFICATION
  | CURLE_GOT_NOTHING
  | CURLE_SSL_ENGINE_NOTFOUND
  | CURLE_SSL_ENGINE_SETFAILED
  | CURLE_SEND_ERROR
  | CURLE_RECV_ERROR
  | CURLE_OBSOLETE57
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
  , (CURLE_OBSOLETE4               , cCURLE_OBSOLETE4               )
  , (CURLE_COULDNT_RESOLVE_PROXY   , cCURLE_COULDNT_RESOLVE_PROXY   )
  , (CURLE_COULDNT_RESOLVE_HOST    , cCURLE_COULDNT_RESOLVE_HOST    )
  , (CURLE_COULDNT_CONNECT         , cCURLE_COULDNT_CONNECT         )
  , (CURLE_FTP_WEIRD_SERVER_REPLY  , cCURLE_FTP_WEIRD_SERVER_REPLY  )
  , (CURLE_REMOTE_ACCESS_DENIED    , cCURLE_REMOTE_ACCESS_DENIED    )
  , (CURLE_OBSOLETE10              , cCURLE_OBSOLETE10              )
  , (CURLE_FTP_WEIRD_PASS_REPLY    , cCURLE_FTP_WEIRD_PASS_REPLY    )
  , (CURLE_OBSOLETE12              , cCURLE_OBSOLETE12              )
  , (CURLE_FTP_WEIRD_PASV_REPLY    , cCURLE_FTP_WEIRD_PASV_REPLY    )
  , (CURLE_FTP_WEIRD_227_FORMAT    , cCURLE_FTP_WEIRD_227_FORMAT    )
  , (CURLE_FTP_CANT_GET_HOST       , cCURLE_FTP_CANT_GET_HOST       )
  , (CURLE_OBSOLETE16              , cCURLE_OBSOLETE16              )
  , (CURLE_FTP_COULDNT_SET_TYPE    , cCURLE_FTP_COULDNT_SET_TYPE    )
  , (CURLE_PARTIAL_FILE            , cCURLE_PARTIAL_FILE            )
  , (CURLE_FTP_COULDNT_RETR_FILE   , cCURLE_FTP_COULDNT_RETR_FILE   )
  , (CURLE_OBSOLETE20              , cCURLE_OBSOLETE20              )
  , (CURLE_QUOTE_ERROR             , cCURLE_QUOTE_ERROR             )
  , (CURLE_HTTP_RETURNED_ERROR     , cCURLE_HTTP_RETURNED_ERROR     )
  , (CURLE_WRITE_ERROR             , cCURLE_WRITE_ERROR             )
  , (CURLE_OBSOLETE24              , cCURLE_OBSOLETE24              )
  , (CURLE_UPLOAD_FAILED           , cCURLE_UPLOAD_FAILED           )
  , (CURLE_READ_ERROR              , cCURLE_READ_ERROR              )
  , (CURLE_OUT_OF_MEMORY           , cCURLE_OUT_OF_MEMORY           )
  , (CURLE_OPERATION_TIMEDOUT      , cCURLE_OPERATION_TIMEDOUT      )
  , (CURLE_OBSOLETE29              , cCURLE_OBSOLETE29              )
  , (CURLE_FTP_PORT_FAILED         , cCURLE_FTP_PORT_FAILED         )
  , (CURLE_FTP_COULDNT_USE_REST    , cCURLE_FTP_COULDNT_USE_REST    )
  , (CURLE_OBSOLETE32              , cCURLE_OBSOLETE32              )
  , (CURLE_RANGE_ERROR             , cCURLE_RANGE_ERROR             )
  , (CURLE_HTTP_POST_ERROR         , cCURLE_HTTP_POST_ERROR         )
  , (CURLE_SSL_CONNECT_ERROR       , cCURLE_SSL_CONNECT_ERROR       )
  , (CURLE_BAD_DOWNLOAD_RESUME     , cCURLE_BAD_DOWNLOAD_RESUME     )
  , (CURLE_FILE_COULDNT_READ_FILE  , cCURLE_FILE_COULDNT_READ_FILE  )
  , (CURLE_LDAP_CANNOT_BIND        , cCURLE_LDAP_CANNOT_BIND        )
  , (CURLE_LDAP_SEARCH_FAILED      , cCURLE_LDAP_SEARCH_FAILED      )
  , (CURLE_OBSOLETE40              , cCURLE_OBSOLETE40              )
  , (CURLE_FUNCTION_NOT_FOUND      , cCURLE_FUNCTION_NOT_FOUND      )
  , (CURLE_ABORTED_BY_CALLBACK     , cCURLE_ABORTED_BY_CALLBACK     )
  , (CURLE_BAD_FUNCTION_ARGUMENT   , cCURLE_BAD_FUNCTION_ARGUMENT   )
  , (CURLE_OBSOLETE44              , cCURLE_OBSOLETE44              )
  , (CURLE_INTERFACE_FAILED        , cCURLE_INTERFACE_FAILED        )
  , (CURLE_OBSOLETE46              , cCURLE_OBSOLETE46              )
  , (CURLE_TOO_MANY_REDIRECTS      , cCURLE_TOO_MANY_REDIRECTS      )
  , (CURLE_UNKNOWN_TELNET_OPTION   , cCURLE_UNKNOWN_TELNET_OPTION   )
  , (CURLE_TELNET_OPTION_SYNTAX    , cCURLE_TELNET_OPTION_SYNTAX    )
  , (CURLE_OBSOLETE50              , cCURLE_OBSOLETE50              )
  , (CURLE_PEER_FAILED_VERIFICATION, cCURLE_PEER_FAILED_VERIFICATION)
  , (CURLE_GOT_NOTHING             , cCURLE_GOT_NOTHING             )
  , (CURLE_SSL_ENGINE_NOTFOUND     , cCURLE_SSL_ENGINE_NOTFOUND     )
  , (CURLE_SSL_ENGINE_SETFAILED    , cCURLE_SSL_ENGINE_SETFAILED    )
  , (CURLE_SEND_ERROR              , cCURLE_SEND_ERROR              )
  , (CURLE_RECV_ERROR              , cCURLE_RECV_ERROR              )
  , (CURLE_OBSOLETE57              , cCURLE_OBSOLETE57              )
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
data CURLinfo'S
  = CURLINFO_EFFECTIVE_URL
  | CURLINFO_CONTENT_TYPE
  | CURLINFO_PRIVATE
  | CURLINFO_FTP_ENTRY_PATH
  | CURLINFO_REDIRECT_URL
  | CURLINFO_PRIMARY_IP
  | CURLINFO_RTSP_SESSION_ID
  | CURLINFO_LOCAL_IP
  deriving (Eq, Show)

instance FromC CCURLinfo_S CURLinfo'S where
  fromC x = findWithDef (cError "CCURLinfo'S") x $ map swap knownCURLinfo'S

instance FromH CURLinfo'S CCURLinfo_S where
  fromH x = findWithDef (hError "CURLinfo'S") x knownCURLinfo'S

knownCURLinfo'S :: [(CURLinfo'S, CCURLinfo_S)]
knownCURLinfo'S =
  [ (CURLINFO_EFFECTIVE_URL  , cCURLINFO_EFFECTIVE_URL  )
  , (CURLINFO_CONTENT_TYPE   , cCURLINFO_CONTENT_TYPE   )
  , (CURLINFO_PRIVATE        , cCURLINFO_PRIVATE        )
  , (CURLINFO_FTP_ENTRY_PATH , cCURLINFO_FTP_ENTRY_PATH )
  , (CURLINFO_REDIRECT_URL   , cCURLINFO_REDIRECT_URL   )
  , (CURLINFO_PRIMARY_IP     , cCURLINFO_PRIMARY_IP     )
  , (CURLINFO_RTSP_SESSION_ID, cCURLINFO_RTSP_SESSION_ID)
  , (CURLINFO_LOCAL_IP       , cCURLINFO_LOCAL_IP       )
  ]


data CURLinfo'I
  = CURLINFO_RESPONSE_CODE
  | CURLINFO_HEADER_SIZE
  | CURLINFO_REQUEST_SIZE
  | CURLINFO_SSL_VERIFYRESULT
  | CURLINFO_FILETIME
  | CURLINFO_REDIRECT_COUNT
  | CURLINFO_HTTP_CONNECTCODE
  | CURLINFO_HTTPAUTH_AVAIL
  | CURLINFO_PROXYAUTH_AVAIL
  | CURLINFO_OS_ERRNO
  | CURLINFO_NUM_CONNECTS
  | CURLINFO_LASTSOCKET
  | CURLINFO_CONDITION_UNMET
  | CURLINFO_RTSP_CLIENT_CSEQ
  | CURLINFO_RTSP_SERVER_CSEQ
  | CURLINFO_RTSP_CSEQ_RECV
  | CURLINFO_PRIMARY_PORT
  | CURLINFO_LOCAL_PORT
  deriving (Eq, Show)

instance FromC CCURLinfo_I CURLinfo'I where
  fromC x = findWithDef (cError "CCURLinfo'I") x $ map swap knownCURLinfo'I

instance FromH CURLinfo'I CCURLinfo_I where
  fromH x = findWithDef (hError "CURLinfo'I") x knownCURLinfo'I

knownCURLinfo'I :: [(CURLinfo'I, CCURLinfo_I)]
knownCURLinfo'I =
  [ (CURLINFO_RESPONSE_CODE   , cCURLINFO_RESPONSE_CODE   )
  , (CURLINFO_HEADER_SIZE     , cCURLINFO_HEADER_SIZE     )
  , (CURLINFO_REQUEST_SIZE    , cCURLINFO_REQUEST_SIZE    )
  , (CURLINFO_SSL_VERIFYRESULT, cCURLINFO_SSL_VERIFYRESULT)
  , (CURLINFO_FILETIME        , cCURLINFO_FILETIME        )
  , (CURLINFO_REDIRECT_COUNT  , cCURLINFO_REDIRECT_COUNT  )
  , (CURLINFO_HTTP_CONNECTCODE, cCURLINFO_HTTP_CONNECTCODE)
  , (CURLINFO_HTTPAUTH_AVAIL  , cCURLINFO_HTTPAUTH_AVAIL  )
  , (CURLINFO_PROXYAUTH_AVAIL , cCURLINFO_PROXYAUTH_AVAIL )
  , (CURLINFO_OS_ERRNO        , cCURLINFO_OS_ERRNO        )
  , (CURLINFO_NUM_CONNECTS    , cCURLINFO_NUM_CONNECTS    )
  , (CURLINFO_LASTSOCKET      , cCURLINFO_LASTSOCKET      )
  , (CURLINFO_CONDITION_UNMET , cCURLINFO_CONDITION_UNMET )
  , (CURLINFO_RTSP_CLIENT_CSEQ, cCURLINFO_RTSP_CLIENT_CSEQ)
  , (CURLINFO_RTSP_SERVER_CSEQ, cCURLINFO_RTSP_SERVER_CSEQ)
  , (CURLINFO_RTSP_CSEQ_RECV  , cCURLINFO_RTSP_CSEQ_RECV  )
  , (CURLINFO_PRIMARY_PORT    , cCURLINFO_PRIMARY_PORT    )
  , (CURLINFO_LOCAL_PORT      , cCURLINFO_LOCAL_PORT      )
  ]


data CURLinfo'D
  = CURLINFO_TOTAL_TIME
  | CURLINFO_NAMELOOKUP_TIME
  | CURLINFO_CONNECT_TIME
  | CURLINFO_PRETRANSFER_TIME
  | CURLINFO_SIZE_UPLOAD
  | CURLINFO_SIZE_DOWNLOAD
  | CURLINFO_SPEED_DOWNLOAD
  | CURLINFO_SPEED_UPLOAD
  | CURLINFO_CONTENT_LENGTH_DOWNLOAD
  | CURLINFO_CONTENT_LENGTH_UPLOAD
  | CURLINFO_STARTTRANSFER_TIME
  | CURLINFO_REDIRECT_TIME
  | CURLINFO_APPCONNECT_TIME
  deriving (Eq, Show)

instance FromC CCURLinfo_D CURLinfo'D where
  fromC x = findWithDef (cError "CCURLinfo'D") x $ map swap knownCURLinfo'D

instance FromH CURLinfo'D CCURLinfo_D where
  fromH x = findWithDef (hError "CURLinfo'D") x knownCURLinfo'D

knownCURLinfo'D :: [(CURLinfo'D, CCURLinfo_D)]
knownCURLinfo'D =
  [ (CURLINFO_TOTAL_TIME             , cCURLINFO_TOTAL_TIME             )
  , (CURLINFO_NAMELOOKUP_TIME        , cCURLINFO_NAMELOOKUP_TIME        )
  , (CURLINFO_CONNECT_TIME           , cCURLINFO_CONNECT_TIME           )
  , (CURLINFO_PRETRANSFER_TIME       , cCURLINFO_PRETRANSFER_TIME       )
  , (CURLINFO_SIZE_UPLOAD            , cCURLINFO_SIZE_UPLOAD            )
  , (CURLINFO_SIZE_DOWNLOAD          , cCURLINFO_SIZE_DOWNLOAD          )
  , (CURLINFO_SPEED_DOWNLOAD         , cCURLINFO_SPEED_DOWNLOAD         )
  , (CURLINFO_SPEED_UPLOAD           , cCURLINFO_SPEED_UPLOAD           )
  , (CURLINFO_CONTENT_LENGTH_DOWNLOAD, cCURLINFO_CONTENT_LENGTH_DOWNLOAD)
  , (CURLINFO_CONTENT_LENGTH_UPLOAD  , cCURLINFO_CONTENT_LENGTH_UPLOAD  )
  , (CURLINFO_STARTTRANSFER_TIME     , cCURLINFO_STARTTRANSFER_TIME     )
  , (CURLINFO_REDIRECT_TIME          , cCURLINFO_REDIRECT_TIME          )
  , (CURLINFO_APPCONNECT_TIME        , cCURLINFO_APPCONNECT_TIME        )
  ]


data CURLinfo'L
  = CURLINFO_SSL_ENGINES
  | CURLINFO_COOKIELIST
  | CURLINFO_CERTINFO
  deriving (Eq, Show)

instance FromC CCURLinfo_L CURLinfo'L where
  fromC x = findWithDef (cError "CCURLinfo'L") x $ map swap knownCURLinfo'L

instance FromH CURLinfo'L CCURLinfo_L where
  fromH x = findWithDef (hError "CURLinfo'L") x knownCURLinfo'L

knownCURLinfo'L :: [(CURLinfo'L, CCURLinfo_L)]
knownCURLinfo'L =
  [ (CURLINFO_SSL_ENGINES, cCURLINFO_SSL_ENGINES)
  , (CURLINFO_COOKIELIST , cCURLINFO_COOKIELIST )
  , (CURLINFO_CERTINFO   , cCURLINFO_CERTINFO   )
  ]


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

