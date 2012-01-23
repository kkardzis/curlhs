-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.CURLcode
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Network.Curlhs.CURLcode
  ( CURLcode (..)
  ) where

import Foreign.C.Types (CInt)
import Data.Tuple (swap)

import Control.Exception
import Data.Typeable

import Network.Curlhs.CURLenum


-------------------------------------------------------------------------------
data CURLcode
  = CURLE_unknown CInt -- unknown code
  | CURLE_OK                       -- 0
  | CURLE_UNSUPPORTED_PROTOCOL     -- 1 
  | CURLE_FAILED_INIT              -- 2 
  | CURLE_URL_MALFORMAT            -- 3 
  | CURLE_OBSOLETE4                -- 4 - NOT USED 
  | CURLE_COULDNT_RESOLVE_PROXY    -- 5 
  | CURLE_COULDNT_RESOLVE_HOST     -- 6 
  | CURLE_COULDNT_CONNECT          -- 7 
  | CURLE_FTP_WEIRD_SERVER_REPLY   -- 8 
  | CURLE_REMOTE_ACCESS_DENIED     -- 9 - a service was denied by the server
                                   --     due to lack of access - when login
                                   --     fails this is not returned. 
  | CURLE_OBSOLETE10               -- 10 - NOT USED 
  | CURLE_FTP_WEIRD_PASS_REPLY     -- 11 
  | CURLE_OBSOLETE12               -- 12 - NOT USED 
  | CURLE_FTP_WEIRD_PASV_REPLY     -- 13 
  | CURLE_FTP_WEIRD_227_FORMAT     -- 14 
  | CURLE_FTP_CANT_GET_HOST        -- 15 
  | CURLE_OBSOLETE16               -- 16 - NOT USED 
  | CURLE_FTP_COULDNT_SET_TYPE     -- 17 
  | CURLE_PARTIAL_FILE             -- 18 
  | CURLE_FTP_COULDNT_RETR_FILE    -- 19 
  | CURLE_OBSOLETE20               -- 20 - NOT USED 
  | CURLE_QUOTE_ERROR              -- 21 - quote command failure 
  | CURLE_HTTP_RETURNED_ERROR      -- 22 
  | CURLE_WRITE_ERROR              -- 23 
  | CURLE_OBSOLETE24               -- 24 - NOT USED 
  | CURLE_UPLOAD_FAILED            -- 25 - failed upload "command" 
  | CURLE_READ_ERROR               -- 26 - couldn't open/read from file 
  | CURLE_OUT_OF_MEMORY            -- 27 
    -- Note: CURLE_OUT_OF_MEMORY may sometimes indicate a conversion error
    --       instead of a memory allocation error if CURL_DOES_CONVERSIONS
    --       is defined
  | CURLE_OPERATION_TIMEDOUT       -- 28 - the timeout time was reached 
  | CURLE_OBSOLETE29               -- 29 - NOT USED 
  | CURLE_FTP_PORT_FAILED          -- 30 - FTP PORT operation failed 
  | CURLE_FTP_COULDNT_USE_REST     -- 31 - the REST command failed 
  | CURLE_OBSOLETE32               -- 32 - NOT USED 
  | CURLE_RANGE_ERROR              -- 33 - RANGE "command" didn't work 
  | CURLE_HTTP_POST_ERROR          -- 34 
  | CURLE_SSL_CONNECT_ERROR        -- 35 - wrong when connecting with SSL 
  | CURLE_BAD_DOWNLOAD_RESUME      -- 36 - couldn't resume download 
  | CURLE_FILE_COULDNT_READ_FILE   -- 37 
  | CURLE_LDAP_CANNOT_BIND         -- 38 
  | CURLE_LDAP_SEARCH_FAILED       -- 39 
  | CURLE_OBSOLETE40               -- 40 - NOT USED 
  | CURLE_FUNCTION_NOT_FOUND       -- 41 
  | CURLE_ABORTED_BY_CALLBACK      -- 42 
  | CURLE_BAD_FUNCTION_ARGUMENT    -- 43 
  | CURLE_OBSOLETE44               -- 44 - NOT USED 
  | CURLE_INTERFACE_FAILED         -- 45 - CURLOPT_INTERFACE failed 
  | CURLE_OBSOLETE46               -- 46 - NOT USED 
  | CURLE_TOO_MANY_REDIRECTS       -- 47 - catch endless re-direct loops 
  | CURLE_UNKNOWN_TELNET_OPTION    -- 48 - User specified an unknown option 
  | CURLE_TELNET_OPTION_SYNTAX     -- 49 - Malformed telnet option 
  | CURLE_OBSOLETE50               -- 50 - NOT USED 
  | CURLE_PEER_FAILED_VERIFICATION -- 51 - peer's certificate or fingerprint
                                   --      wasn't verified fine 
  | CURLE_GOT_NOTHING              -- 52 - when this is a specific error 
  | CURLE_SSL_ENGINE_NOTFOUND      -- 53 - SSL crypto engine not found 
  | CURLE_SSL_ENGINE_SETFAILED     -- 54 - can not set SSL crypto engine as
                                   --      default 
  | CURLE_SEND_ERROR               -- 55 - failed sending network data 
  | CURLE_RECV_ERROR               -- 56 - failure in receiving network data 
  | CURLE_OBSOLETE57               -- 57 - NOT IN USE 
  | CURLE_SSL_CERTPROBLEM          -- 58 - problem with the local certificate 
  | CURLE_SSL_CIPHER               -- 59 - couldn't use specified cipher 
  | CURLE_SSL_CACERT               -- 60 - problem with the CA cert (path?) 
  | CURLE_BAD_CONTENT_ENCODING     -- 61 - Unrecognized transfer encoding 
  | CURLE_LDAP_INVALID_URL         -- 62 - Invalid LDAP URL 
  | CURLE_FILESIZE_EXCEEDED        -- 63 - Maximum file size exceeded 
  | CURLE_USE_SSL_FAILED           -- 64 - Requested FTP SSL level failed 
  | CURLE_SEND_FAIL_REWIND         -- 65 - Sending the data requires a rewind
                                   --      that failed 
  | CURLE_SSL_ENGINE_INITFAILED    -- 66 - failed to initialise ENGINE 
  | CURLE_LOGIN_DENIED             -- 67 - user, password or similar was not
                                   --      accepted and we failed to login 
  | CURLE_TFTP_NOTFOUND            -- 68 - file not found on server 
  | CURLE_TFTP_PERM                -- 69 - permission problem on server 
  | CURLE_REMOTE_DISK_FULL         -- 70 - out of disk space on server 
  | CURLE_TFTP_ILLEGAL             -- 71 - Illegal TFTP operation 
  | CURLE_TFTP_UNKNOWNID           -- 72 - Unknown transfer ID 
  | CURLE_REMOTE_FILE_EXISTS       -- 73 - File already exists 
  | CURLE_TFTP_NOSUCHUSER          -- 74 - No such user 
  | CURLE_CONV_FAILED              -- 75 - conversion failed 
  | CURLE_CONV_REQD                -- 76 - caller must register conversion
                                   --      callbacks using curl_easy_setopt
                                   --      options
                                   --      CURLOPT_CONV_FROM_NETWORK_FUNCTION,
                                   --      CURLOPT_CONV_TO_NETWORK_FUNCTION,
                                   --      and CURLOPT_CONV_FROM_UTF8_FUNCTION 
  | CURLE_SSL_CACERT_BADFILE       -- 77 - could not load CACERT file, missing
                                   --      or wrong format 
  | CURLE_REMOTE_FILE_NOT_FOUND    -- 78 - remote file not found 
  | CURLE_SSH                      -- 79 - error from the SSH layer, somewhat
                                   --      generic so the error message will be
                                   --      of interest when this has happened 
  | CURLE_SSL_SHUTDOWN_FAILED      -- 80 - Failed to shut down the SSL
                                   --      connection 
  | CURLE_AGAIN                    -- 81 - socket is not ready for send/recv,
                                   --      wait till it's ready and try again
                                   --      (Added in 7.18.2) 
  | CURLE_SSL_CRL_BADFILE          -- 82 - could not load CRL file, missing or
                                   --      wrong format (Added in 7.19.0) 
  | CURLE_SSL_ISSUER_ERROR         -- 83 - Issuer check failed.  (Added in
                                   --      7.19.0) 
  | CURLE_FTP_PRET_FAILED          -- 84 - a PRET command failed 
  | CURLE_RTSP_CSEQ_ERROR          -- 85 - mismatch of RTSP CSeq numbers 
  | CURLE_RTSP_SESSION_ERROR       -- 86 - mismatch of RTSP Session Identifiers 
  | CURLE_FTP_BAD_FILE_LIST        -- 87 - unable to parse FTP file list 
  | CURLE_CHUNK_FAILED             -- 88 - chunk callback reported error 
  deriving (Eq, Show, Typeable)

instance Exception CURLcode


-------------------------------------------------------------------------------
lookup_err = error "CURLenum.CURLcode: bad argument or incomplete lookup table"

lookup_key :: [(CInt, CURLcode)] -> CInt -> CURLcode
lookup_key table key = maybe (CURLE_unknown key) id $ lookup key table

lookup_val :: (Eq a) => [(CInt, a)] -> a -> CInt
lookup_val table val = maybe lookup_err id $ lookup val $ map swap table

instance CURLenum CURLcode where
  fromCURLenum = lookup_key knownCURLcode
  toCURLenum   = lookup_val knownCURLcode


-------------------------------------------------------------------------------
knownCURLcode :: [(CInt, CURLcode)]
knownCURLcode = map swap
  [ (CURLE_OK                       ,  0)
  , (CURLE_UNSUPPORTED_PROTOCOL     ,  1)
  , (CURLE_FAILED_INIT              ,  2)
  , (CURLE_URL_MALFORMAT            ,  3)
  , (CURLE_OBSOLETE4                ,  4)
  , (CURLE_COULDNT_RESOLVE_PROXY    ,  5)
  , (CURLE_COULDNT_RESOLVE_HOST     ,  6)
  , (CURLE_COULDNT_CONNECT          ,  7)
  , (CURLE_FTP_WEIRD_SERVER_REPLY   ,  8)
  , (CURLE_REMOTE_ACCESS_DENIED     ,  9)
  , (CURLE_OBSOLETE10               , 10)
  , (CURLE_FTP_WEIRD_PASS_REPLY     , 11)
  , (CURLE_OBSOLETE12               , 12)
  , (CURLE_FTP_WEIRD_PASV_REPLY     , 13)
  , (CURLE_FTP_WEIRD_227_FORMAT     , 14)
  , (CURLE_FTP_CANT_GET_HOST        , 15)
  , (CURLE_OBSOLETE16               , 16)
  , (CURLE_FTP_COULDNT_SET_TYPE     , 17)
  , (CURLE_PARTIAL_FILE             , 18)
  , (CURLE_FTP_COULDNT_RETR_FILE    , 19)
  , (CURLE_OBSOLETE20               , 20)
  , (CURLE_QUOTE_ERROR              , 21)
  , (CURLE_HTTP_RETURNED_ERROR      , 22)
  , (CURLE_WRITE_ERROR              , 23)
  , (CURLE_OBSOLETE24               , 24)
  , (CURLE_UPLOAD_FAILED            , 25)
  , (CURLE_READ_ERROR               , 26)
  , (CURLE_OUT_OF_MEMORY            , 27)
  , (CURLE_OPERATION_TIMEDOUT       , 28)
  , (CURLE_OBSOLETE29               , 29)
  , (CURLE_FTP_PORT_FAILED          , 30)
  , (CURLE_FTP_COULDNT_USE_REST     , 31)
  , (CURLE_OBSOLETE32               , 32)
  , (CURLE_RANGE_ERROR              , 33)
  , (CURLE_HTTP_POST_ERROR          , 34)
  , (CURLE_SSL_CONNECT_ERROR        , 35)
  , (CURLE_BAD_DOWNLOAD_RESUME      , 36)
  , (CURLE_FILE_COULDNT_READ_FILE   , 37)
  , (CURLE_LDAP_CANNOT_BIND         , 38)
  , (CURLE_LDAP_SEARCH_FAILED       , 39)
  , (CURLE_OBSOLETE40               , 40)
  , (CURLE_FUNCTION_NOT_FOUND       , 41)
  , (CURLE_ABORTED_BY_CALLBACK      , 42)
  , (CURLE_BAD_FUNCTION_ARGUMENT    , 43)
  , (CURLE_OBSOLETE44               , 44)
  , (CURLE_INTERFACE_FAILED         , 45)
  , (CURLE_OBSOLETE46               , 46)
  , (CURLE_TOO_MANY_REDIRECTS       , 47)
  , (CURLE_UNKNOWN_TELNET_OPTION    , 48)
  , (CURLE_TELNET_OPTION_SYNTAX     , 49)
  , (CURLE_OBSOLETE50               , 50)
  , (CURLE_PEER_FAILED_VERIFICATION , 51)
  , (CURLE_GOT_NOTHING              , 52)
  , (CURLE_SSL_ENGINE_NOTFOUND      , 53)
  , (CURLE_SSL_ENGINE_SETFAILED     , 54)
  , (CURLE_SEND_ERROR               , 55)
  , (CURLE_RECV_ERROR               , 56)
  , (CURLE_OBSOLETE57               , 57)
  , (CURLE_SSL_CERTPROBLEM          , 58)
  , (CURLE_SSL_CIPHER               , 59)
  , (CURLE_SSL_CACERT               , 60)
  , (CURLE_BAD_CONTENT_ENCODING     , 61)
  , (CURLE_LDAP_INVALID_URL         , 62)
  , (CURLE_FILESIZE_EXCEEDED        , 63)
  , (CURLE_USE_SSL_FAILED           , 64)
  , (CURLE_SEND_FAIL_REWIND         , 65)
  , (CURLE_SSL_ENGINE_INITFAILED    , 66)
  , (CURLE_LOGIN_DENIED             , 67)
  , (CURLE_TFTP_NOTFOUND            , 68)
  , (CURLE_TFTP_PERM                , 69)
  , (CURLE_REMOTE_DISK_FULL         , 70)
  , (CURLE_TFTP_ILLEGAL             , 71)
  , (CURLE_TFTP_UNKNOWNID           , 72)
  , (CURLE_REMOTE_FILE_EXISTS       , 73)
  , (CURLE_TFTP_NOSUCHUSER          , 74)
  , (CURLE_CONV_FAILED              , 75)
  , (CURLE_CONV_REQD                , 76)
  , (CURLE_SSL_CACERT_BADFILE       , 77)
  , (CURLE_REMOTE_FILE_NOT_FOUND    , 78)
  , (CURLE_SSH                      , 79)
  , (CURLE_SSL_SHUTDOWN_FAILED      , 80)
  , (CURLE_AGAIN                    , 81)
  , (CURLE_SSL_CRL_BADFILE          , 82)
  , (CURLE_SSL_ISSUER_ERROR         , 83)
  , (CURLE_FTP_PRET_FAILED          , 84)
  , (CURLE_RTSP_CSEQ_ERROR          , 85)
  , (CURLE_RTSP_SESSION_ERROR       , 86)
  , (CURLE_FTP_BAD_FILE_LIST        , 87)
  , (CURLE_CHUNK_FAILED             , 88)
  ]

