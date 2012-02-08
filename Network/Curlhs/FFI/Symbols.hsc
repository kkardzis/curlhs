-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.FFI.Symbols
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curlhs.FFI.Symbols where

import Network.Curlhs.FFI.Types
import Foreign.C.Types


-------------------------------------------------------------------------------
#define CURL_7_20_0 0x071400
#define CURL_7_20_1 0x071401
#define CURL_7_21_0 0x071500
#define CURL_7_21_1 0x071501
#define CURL_7_21_2 0x071502
#define CURL_7_21_3 0x071503
#define CURL_7_21_4 0x071504
#define CURL_7_21_5 0x071505
#define CURL_7_21_6 0x071506
#define CURL_7_21_7 0x071507
#define CURL_7_22_0 0x071600
#define CURL_7_23_0 0x071700
#define CURL_7_23_1 0x071701
#define CURL_7_24_0 0x071800

#define CURL_NO_OLDIES

#include "curl/curl.h"


-------------------------------------------------------------------------------
#define hsc_symbol(name, type) \
  printf("c" #name " :: C" #type "\n"); \
  printf("c" #name " =  C" #type " "); hsc_const(name);

#define hsc_cconst(name, type) \
  printf("c" #name " :: " #type "\n"); \
  printf("c" #name " =  "); hsc_const(name);



-------------------------------------------------------------------------------
-- from "curlver.h"
-------------------------------------------------------------------------------
libCURL_COPYRIGHT :: String
libCURL_COPYRIGHT = #{const_str LIBCURL_COPYRIGHT}

libCURL_TIMESTAMP :: String
libCURL_TIMESTAMP = #{const_str LIBCURL_TIMESTAMP}

libCURL_VERSION   :: String
libCURL_VERSION   = #{const_str LIBCURL_VERSION  }

libCURL_VERSION_NUM   :: Int
libCURL_VERSION_NUM   = #{const LIBCURL_VERSION_NUM  }

libCURL_VERSION_MAJOR :: Int
libCURL_VERSION_MAJOR = #{const LIBCURL_VERSION_MAJOR}

libCURL_VERSION_MINOR :: Int
libCURL_VERSION_MINOR = #{const LIBCURL_VERSION_MINOR}

libCURL_VERSION_PATCH :: Int
libCURL_VERSION_PATCH = #{const LIBCURL_VERSION_PATCH}


-------------------------------------------------------------------------------
-- from "curlbuild.h"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- from "curlrules.h"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- from "curl.h"
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
#if LIBCURL_VERSION_NUM >= CURL_7_20_0
#{symbol CURLE_OK                      , CURLcode}
#{symbol CURLE_UNSUPPORTED_PROTOCOL    , CURLcode}
#{symbol CURLE_FAILED_INIT             , CURLcode}
#{symbol CURLE_URL_MALFORMAT           , CURLcode}
#{symbol CURLE_COULDNT_RESOLVE_PROXY   , CURLcode}
#{symbol CURLE_COULDNT_RESOLVE_HOST    , CURLcode}
#{symbol CURLE_COULDNT_CONNECT         , CURLcode}
#{symbol CURLE_FTP_WEIRD_SERVER_REPLY  , CURLcode}
#{symbol CURLE_REMOTE_ACCESS_DENIED    , CURLcode}
#{symbol CURLE_FTP_WEIRD_PASS_REPLY    , CURLcode}
#{symbol CURLE_FTP_WEIRD_PASV_REPLY    , CURLcode}
#{symbol CURLE_FTP_WEIRD_227_FORMAT    , CURLcode}
#{symbol CURLE_FTP_CANT_GET_HOST       , CURLcode}
#{symbol CURLE_FTP_COULDNT_SET_TYPE    , CURLcode}
#{symbol CURLE_PARTIAL_FILE            , CURLcode}
#{symbol CURLE_FTP_COULDNT_RETR_FILE   , CURLcode}
#{symbol CURLE_QUOTE_ERROR             , CURLcode}
#{symbol CURLE_HTTP_RETURNED_ERROR     , CURLcode}
#{symbol CURLE_WRITE_ERROR             , CURLcode}
#{symbol CURLE_UPLOAD_FAILED           , CURLcode}
#{symbol CURLE_READ_ERROR              , CURLcode}
#{symbol CURLE_OUT_OF_MEMORY           , CURLcode}
#{symbol CURLE_OPERATION_TIMEDOUT      , CURLcode}
#{symbol CURLE_FTP_PORT_FAILED         , CURLcode}
#{symbol CURLE_FTP_COULDNT_USE_REST    , CURLcode}
#{symbol CURLE_RANGE_ERROR             , CURLcode}
#{symbol CURLE_HTTP_POST_ERROR         , CURLcode}
#{symbol CURLE_SSL_CONNECT_ERROR       , CURLcode}
#{symbol CURLE_BAD_DOWNLOAD_RESUME     , CURLcode}
#{symbol CURLE_FILE_COULDNT_READ_FILE  , CURLcode}
#{symbol CURLE_LDAP_CANNOT_BIND        , CURLcode}
#{symbol CURLE_LDAP_SEARCH_FAILED      , CURLcode}
#{symbol CURLE_FUNCTION_NOT_FOUND      , CURLcode}
#{symbol CURLE_ABORTED_BY_CALLBACK     , CURLcode}
#{symbol CURLE_BAD_FUNCTION_ARGUMENT   , CURLcode}
#{symbol CURLE_INTERFACE_FAILED        , CURLcode}
#{symbol CURLE_TOO_MANY_REDIRECTS      , CURLcode}
#if LIBCURL_VERSION_NUM < CURL_7_21_5
#{symbol CURLE_UNKNOWN_TELNET_OPTION   , CURLcode}
#endif
#{symbol CURLE_TELNET_OPTION_SYNTAX    , CURLcode}
#{symbol CURLE_PEER_FAILED_VERIFICATION, CURLcode}
#{symbol CURLE_GOT_NOTHING             , CURLcode}
#{symbol CURLE_SSL_ENGINE_NOTFOUND     , CURLcode}
#{symbol CURLE_SSL_ENGINE_SETFAILED    , CURLcode}
#{symbol CURLE_SEND_ERROR              , CURLcode}
#{symbol CURLE_RECV_ERROR              , CURLcode}
#{symbol CURLE_SSL_CERTPROBLEM         , CURLcode}
#{symbol CURLE_SSL_CIPHER              , CURLcode}
#{symbol CURLE_SSL_CACERT              , CURLcode}
#{symbol CURLE_BAD_CONTENT_ENCODING    , CURLcode}
#{symbol CURLE_LDAP_INVALID_URL        , CURLcode}
#{symbol CURLE_FILESIZE_EXCEEDED       , CURLcode}
#{symbol CURLE_USE_SSL_FAILED          , CURLcode}
#{symbol CURLE_SEND_FAIL_REWIND        , CURLcode}
#{symbol CURLE_SSL_ENGINE_INITFAILED   , CURLcode}
#{symbol CURLE_LOGIN_DENIED            , CURLcode}
#{symbol CURLE_TFTP_NOTFOUND           , CURLcode}
#{symbol CURLE_TFTP_PERM               , CURLcode}
#{symbol CURLE_REMOTE_DISK_FULL        , CURLcode}
#{symbol CURLE_TFTP_ILLEGAL            , CURLcode}
#{symbol CURLE_TFTP_UNKNOWNID          , CURLcode}
#{symbol CURLE_REMOTE_FILE_EXISTS      , CURLcode}
#{symbol CURLE_TFTP_NOSUCHUSER         , CURLcode}
#{symbol CURLE_CONV_FAILED             , CURLcode}
#{symbol CURLE_CONV_REQD               , CURLcode}
#{symbol CURLE_SSL_CACERT_BADFILE      , CURLcode}
#{symbol CURLE_REMOTE_FILE_NOT_FOUND   , CURLcode}
#{symbol CURLE_SSH                     , CURLcode}
#{symbol CURLE_SSL_SHUTDOWN_FAILED     , CURLcode}
#{symbol CURLE_AGAIN                   , CURLcode}
#{symbol CURLE_SSL_CRL_BADFILE         , CURLcode}
#{symbol CURLE_SSL_ISSUER_ERROR        , CURLcode}
#{symbol CURLE_FTP_PRET_FAILED         , CURLcode}
#{symbol CURLE_RTSP_CSEQ_ERROR         , CURLcode}
#{symbol CURLE_RTSP_SESSION_ERROR      , CURLcode}
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_0
#{symbol CURLE_FTP_BAD_FILE_LIST       , CURLcode}
#{symbol CURLE_CHUNK_FAILED            , CURLcode}
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_5
#{symbol CURLE_NOT_BUILT_IN            , CURLcode}
#{symbol CURLE_UNKNOWN_OPTION          , CURLcode}
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_24_0
#{symbol CURLE_FTP_ACCEPT_FAILED       , CURLcode}
#{symbol CURLE_FTP_ACCEPT_TIMEOUT      , CURLcode}
#endif




-------------------------------------------------------------------------------
#define hsc_curlopt(name, type) \
  printf("c" #name " :: CCURLoption'" #type "\n"); \
  printf("c" #name " =  CCURLoption'" #type " "); hsc_const(name);

#if LIBCURL_VERSION_NUM >= CURL_7_20_0
#{curlopt CURLOPT_FILE                       , File   }
#{curlopt CURLOPT_URL                        , String }
#{curlopt CURLOPT_PORT                       , Int32  }
#{curlopt CURLOPT_PROXY                      , String }
#{curlopt CURLOPT_USERPWD                    , String }
#{curlopt CURLOPT_PROXYUSERPWD               , String }
#{curlopt CURLOPT_RANGE                      , String }
#{curlopt CURLOPT_INFILE                     , File   }
#{curlopt CURLOPT_ERRORBUFFER                , String }
#{curlopt CURLOPT_WRITEFUNCTION              , FunPtr }
#{curlopt CURLOPT_READFUNCTION               , FunPtr }
#{curlopt CURLOPT_TIMEOUT                    , Int32  }
#{curlopt CURLOPT_INFILESIZE                 , Int32  }
#{curlopt CURLOPT_POSTFIELDS                 , Ptr_a  }
#{curlopt CURLOPT_REFERER                    , String }
#{curlopt CURLOPT_FTPPORT                    , String }
#{curlopt CURLOPT_USERAGENT                  , String }
#{curlopt CURLOPT_LOW_SPEED_LIMIT            , Int32  }
#{curlopt CURLOPT_LOW_SPEED_TIME             , Int32  }
#{curlopt CURLOPT_RESUME_FROM                , Int32  }
#{curlopt CURLOPT_COOKIE                     , String }
#{curlopt CURLOPT_HTTPHEADER                 , SList  }
#{curlopt CURLOPT_HTTPPOST                   , HTTPP  }
#{curlopt CURLOPT_SSLCERT                    , String }
#{curlopt CURLOPT_KEYPASSWD                  , String }
#{curlopt CURLOPT_CRLF                       , Int32  }
#{curlopt CURLOPT_QUOTE                      , SList  }
#{curlopt CURLOPT_WRITEHEADER                , Ptr_a  }
#{curlopt CURLOPT_COOKIEFILE                 , String }
#{curlopt CURLOPT_SSLVERSION                 , Int32  }
#{curlopt CURLOPT_TIMECONDITION              , Int32  }
#{curlopt CURLOPT_TIMEVALUE                  , Int32  }
#{curlopt CURLOPT_CUSTOMREQUEST              , String }
#{curlopt CURLOPT_STDERR                     , File   }
#{curlopt CURLOPT_POSTQUOTE                  , SList  }
#{curlopt CURLOPT_WRITEINFO                  , String }
#{curlopt CURLOPT_VERBOSE                    , Int32  }
#{curlopt CURLOPT_HEADER                     , Int32  }
#{curlopt CURLOPT_NOPROGRESS                 , Int32  }
#{curlopt CURLOPT_NOBODY                     , Int32  }
#{curlopt CURLOPT_FAILONERROR                , Int32  }
#{curlopt CURLOPT_UPLOAD                     , Int32  }
#{curlopt CURLOPT_POST                       , Int32  }
#{curlopt CURLOPT_DIRLISTONLY                , Int32  }
#{curlopt CURLOPT_APPEND                     , Int32  }
#{curlopt CURLOPT_NETRC                      , Int32  }
#{curlopt CURLOPT_FOLLOWLOCATION             , Int32  }
#{curlopt CURLOPT_TRANSFERTEXT               , Int32  }
#{curlopt CURLOPT_PUT                        , Int32  }
#{curlopt CURLOPT_PROGRESSFUNCTION           , FunPtr }
#{curlopt CURLOPT_PROGRESSDATA               , Ptr_a  }
#{curlopt CURLOPT_AUTOREFERER                , Int32  }
#{curlopt CURLOPT_PROXYPORT                  , Int32  }
#{curlopt CURLOPT_POSTFIELDSIZE              , Int32  }
#{curlopt CURLOPT_HTTPPROXYTUNNEL            , Int32  }
#{curlopt CURLOPT_INTERFACE                  , String }
#{curlopt CURLOPT_KRBLEVEL                   , String }
#{curlopt CURLOPT_SSL_VERIFYPEER             , Int32  }
#{curlopt CURLOPT_CAINFO                     , String }
#{curlopt CURLOPT_MAXREDIRS                  , Int32  }
#{curlopt CURLOPT_FILETIME                   , Int32  }
#{curlopt CURLOPT_TELNETOPTIONS              , SList  }
#{curlopt CURLOPT_MAXCONNECTS                , Int32  }
#{curlopt CURLOPT_CLOSEPOLICY                , Int32  }
#{curlopt CURLOPT_FRESH_CONNECT              , Int32  }
#{curlopt CURLOPT_FORBID_REUSE               , Int32  }
#{curlopt CURLOPT_RANDOM_FILE                , String }
#{curlopt CURLOPT_EGDSOCKET                  , String }
#{curlopt CURLOPT_CONNECTTIMEOUT             , Int32  }
#{curlopt CURLOPT_HEADERFUNCTION             , FunPtr }
#{curlopt CURLOPT_HTTPGET                    , Int32  }
#{curlopt CURLOPT_SSL_VERIFYHOST             , Int32  }
#{curlopt CURLOPT_COOKIEJAR                  , String }
#{curlopt CURLOPT_SSL_CIPHER_LIST            , String }
#{curlopt CURLOPT_HTTP_VERSION               , Int32  }
#{curlopt CURLOPT_FTP_USE_EPSV               , Int32  }
#{curlopt CURLOPT_SSLCERTTYPE                , String }
#{curlopt CURLOPT_SSLKEY                     , String }
#{curlopt CURLOPT_SSLKEYTYPE                 , String }
#{curlopt CURLOPT_SSLENGINE                  , String }
#{curlopt CURLOPT_SSLENGINE_DEFAULT          , Int32  }
#{curlopt CURLOPT_DNS_USE_GLOBAL_CACHE       , Int32  }
#{curlopt CURLOPT_DNS_CACHE_TIMEOUT          , Int32  }
#{curlopt CURLOPT_PREQUOTE                   , SList  }
#{curlopt CURLOPT_DEBUGFUNCTION              , FunPtr }
#{curlopt CURLOPT_DEBUGDATA                  , Ptr_a  }
#{curlopt CURLOPT_COOKIESESSION              , Int32  }
#{curlopt CURLOPT_CAPATH                     , String }
#{curlopt CURLOPT_BUFFERSIZE                 , Int32  }
#{curlopt CURLOPT_NOSIGNAL                   , Int32  }
#{curlopt CURLOPT_SHARE                      , Share  }
#{curlopt CURLOPT_PROXYTYPE                  , Int32  }
#if LIBCURL_VERSION_NUM < CURL_7_21_6
#{curlopt CURLOPT_ENCODING                   , String }
#endif
#{curlopt CURLOPT_PRIVATE                    , Ptr_a  }
#{curlopt CURLOPT_HTTP200ALIASES             , SList  }
#{curlopt CURLOPT_UNRESTRICTED_AUTH          , Int32  }
#{curlopt CURLOPT_FTP_USE_EPRT               , Int32  }
#{curlopt CURLOPT_HTTPAUTH                   , Int32  }
#{curlopt CURLOPT_SSL_CTX_FUNCTION           , FunPtr }
#{curlopt CURLOPT_SSL_CTX_DATA               , Ptr_a  }
#{curlopt CURLOPT_FTP_CREATE_MISSING_DIRS    , Int32  }
#{curlopt CURLOPT_PROXYAUTH                  , Int32  }
#{curlopt CURLOPT_FTP_RESPONSE_TIMEOUT       , Int32  }
#{curlopt CURLOPT_IPRESOLVE                  , Int32  }
#{curlopt CURLOPT_MAXFILESIZE                , Int32  }
#{curlopt CURLOPT_INFILESIZE_LARGE           , Int64  }
#{curlopt CURLOPT_RESUME_FROM_LARGE          , Int64  }
#{curlopt CURLOPT_MAXFILESIZE_LARGE          , Int64  }
#{curlopt CURLOPT_NETRC_FILE                 , String }
#{curlopt CURLOPT_USE_SSL                    , Int32  }
#{curlopt CURLOPT_POSTFIELDSIZE_LARGE        , Int64  }
#{curlopt CURLOPT_TCP_NODELAY                , Int32  }
#{curlopt CURLOPT_FTPSSLAUTH                 , Int32  }
#{curlopt CURLOPT_IOCTLFUNCTION              , FunPtr }
#{curlopt CURLOPT_IOCTLDATA                  , Ptr_a  }
#{curlopt CURLOPT_FTP_ACCOUNT                , String }
#{curlopt CURLOPT_COOKIELIST                 , String }
#{curlopt CURLOPT_IGNORE_CONTENT_LENGTH      , Int32  }
#{curlopt CURLOPT_FTP_SKIP_PASV_IP           , Int32  }
#{curlopt CURLOPT_FTP_FILEMETHOD             , Int32  }
#{curlopt CURLOPT_LOCALPORT                  , Int32  }
#{curlopt CURLOPT_LOCALPORTRANGE             , Int32  }
#{curlopt CURLOPT_CONNECT_ONLY               , Int32  }
#{curlopt CURLOPT_CONV_FROM_NETWORK_FUNCTION , FunPtr }
#{curlopt CURLOPT_CONV_TO_NETWORK_FUNCTION   , FunPtr }
#{curlopt CURLOPT_CONV_FROM_UTF8_FUNCTION    , FunPtr }
#{curlopt CURLOPT_MAX_SEND_SPEED_LARGE       , Int64  }
#{curlopt CURLOPT_MAX_RECV_SPEED_LARGE       , Int64  }
#{curlopt CURLOPT_FTP_ALTERNATIVE_TO_USER    , String }
#{curlopt CURLOPT_SOCKOPTFUNCTION            , FunPtr }
#{curlopt CURLOPT_SOCKOPTDATA                , Ptr_a  }
#{curlopt CURLOPT_SSL_SESSIONID_CACHE        , Int32  }
#{curlopt CURLOPT_SSH_AUTH_TYPES             , Int32  }
#{curlopt CURLOPT_SSH_PUBLIC_KEYFILE         , String }
#{curlopt CURLOPT_SSH_PRIVATE_KEYFILE        , String }
#{curlopt CURLOPT_FTP_SSL_CCC                , Int32  }
#{curlopt CURLOPT_TIMEOUT_MS                 , Int32  }
#{curlopt CURLOPT_CONNECTTIMEOUT_MS          , Int32  }
#{curlopt CURLOPT_HTTP_TRANSFER_DECODING     , Int32  }
#{curlopt CURLOPT_HTTP_CONTENT_DECODING      , Int32  }
#{curlopt CURLOPT_NEW_FILE_PERMS             , Int32  }
#{curlopt CURLOPT_NEW_DIRECTORY_PERMS        , Int32  }
#{curlopt CURLOPT_POSTREDIR                  , Int32  }
#{curlopt CURLOPT_SSH_HOST_PUBLIC_KEY_MD5    , String }
#{curlopt CURLOPT_OPENSOCKETFUNCTION         , FunPtr }
#{curlopt CURLOPT_OPENSOCKETDATA             , Ptr_a  }
#{curlopt CURLOPT_COPYPOSTFIELDS             , String }
#{curlopt CURLOPT_PROXY_TRANSFER_MODE        , Int32  }
#{curlopt CURLOPT_SEEKFUNCTION               , FunPtr }
#{curlopt CURLOPT_SEEKDATA                   , Ptr_a  }
#{curlopt CURLOPT_CRLFILE                    , String }
#{curlopt CURLOPT_ISSUERCERT                 , String }
#{curlopt CURLOPT_ADDRESS_SCOPE              , Int32  }
#{curlopt CURLOPT_CERTINFO                   , Int32  }
#{curlopt CURLOPT_USERNAME                   , String }
#{curlopt CURLOPT_PASSWORD                   , String }
#{curlopt CURLOPT_PROXYUSERNAME              , String }
#{curlopt CURLOPT_PROXYPASSWORD              , String }
#{curlopt CURLOPT_NOPROXY                    , String }
#{curlopt CURLOPT_TFTP_BLKSIZE               , Int32  }
#{curlopt CURLOPT_SOCKS5_GSSAPI_SERVICE      , String }
#{curlopt CURLOPT_SOCKS5_GSSAPI_NEC          , Int32  }
#{curlopt CURLOPT_PROTOCOLS                  , Int32  }
#{curlopt CURLOPT_REDIR_PROTOCOLS            , Int32  }
#{curlopt CURLOPT_SSH_KNOWNHOSTS             , String }
#{curlopt CURLOPT_SSH_KEYFUNCTION            , FunPtr }
#{curlopt CURLOPT_SSH_KEYDATA                , Ptr_a  }
#{curlopt CURLOPT_MAIL_FROM                  , String }
#{curlopt CURLOPT_MAIL_RCPT                  , SList  }
#{curlopt CURLOPT_FTP_USE_PRET               , Int32  }
#{curlopt CURLOPT_RTSP_REQUEST               , Int32  }
#{curlopt CURLOPT_RTSP_SESSION_ID            , String }
#{curlopt CURLOPT_RTSP_STREAM_URI            , String }
#{curlopt CURLOPT_RTSP_TRANSPORT             , String }
#{curlopt CURLOPT_RTSP_CLIENT_CSEQ           , Int32  }
#{curlopt CURLOPT_RTSP_SERVER_CSEQ           , Int32  }
#{curlopt CURLOPT_INTERLEAVEDATA             , Ptr_a  }
#{curlopt CURLOPT_INTERLEAVEFUNCTION         , FunPtr }
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_0
#{curlopt CURLOPT_WILDCARDMATCH              , Int32  }
#{curlopt CURLOPT_CHUNK_BGN_FUNCTION         , FunPtr }
#{curlopt CURLOPT_CHUNK_END_FUNCTION         , FunPtr }
#{curlopt CURLOPT_FNMATCH_FUNCTION           , FunPtr }
#{curlopt CURLOPT_CHUNK_DATA                 , Ptr_a  }
#{curlopt CURLOPT_FNMATCH_DATA               , Ptr_a  }
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_3
#{curlopt CURLOPT_RESOLVE                    , SList  }
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_4
#{curlopt CURLOPT_TLSAUTH_USERNAME           , String }
#{curlopt CURLOPT_TLSAUTH_PASSWORD           , String }
#{curlopt CURLOPT_TLSAUTH_TYPE               , String }
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_6
#{curlopt CURLOPT_ACCEPT_ENCODING            , String }
#{curlopt CURLOPT_TRANSFER_ENCODING          , Int32  }
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_7
#{curlopt CURLOPT_CLOSESOCKETFUNCTION        , FunPtr }
#{curlopt CURLOPT_CLOSESOCKETDATA            , Ptr_a  }
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_22_0
#{curlopt CURLOPT_GSSAPI_DELEGATION          , Int32  }
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_24_0
#{curlopt CURLOPT_DNS_SERVERS                , String }
#{curlopt CURLOPT_ACCEPTTIMEOUT_MS           , Int32  }
#endif




-------------------------------------------------------------------------------
#define hsc_curlinfo(name, type) \
  printf("c" #name " :: CCURLinfo'" #type "\n"); \
  printf("c" #name " =  CCURLinfo'" #type " "); hsc_const(name);

#if LIBCURL_VERSION_NUM >= CURL_7_20_0
#{curlinfo CURLINFO_EFFECTIVE_URL          , S}
#{curlinfo CURLINFO_RESPONSE_CODE          , I}
#{curlinfo CURLINFO_TOTAL_TIME             , D}
#{curlinfo CURLINFO_NAMELOOKUP_TIME        , D}
#{curlinfo CURLINFO_CONNECT_TIME           , D}
#{curlinfo CURLINFO_PRETRANSFER_TIME       , D}
#{curlinfo CURLINFO_SIZE_UPLOAD            , D}
#{curlinfo CURLINFO_SIZE_DOWNLOAD          , D}
#{curlinfo CURLINFO_SPEED_DOWNLOAD         , D}
#{curlinfo CURLINFO_SPEED_UPLOAD           , D}
#{curlinfo CURLINFO_HEADER_SIZE            , I}
#{curlinfo CURLINFO_REQUEST_SIZE           , I}
#{curlinfo CURLINFO_SSL_VERIFYRESULT       , I}
#{curlinfo CURLINFO_FILETIME               , I}
#{curlinfo CURLINFO_CONTENT_LENGTH_DOWNLOAD, D}
#{curlinfo CURLINFO_CONTENT_LENGTH_UPLOAD  , D}
#{curlinfo CURLINFO_STARTTRANSFER_TIME     , D}
#{curlinfo CURLINFO_CONTENT_TYPE           , S}
#{curlinfo CURLINFO_REDIRECT_TIME          , D}
#{curlinfo CURLINFO_REDIRECT_COUNT         , I}
#{curlinfo CURLINFO_PRIVATE                , S}
#{curlinfo CURLINFO_HTTP_CONNECTCODE       , I}
#{curlinfo CURLINFO_HTTPAUTH_AVAIL         , I}
#{curlinfo CURLINFO_PROXYAUTH_AVAIL        , I}
#{curlinfo CURLINFO_OS_ERRNO               , I}
#{curlinfo CURLINFO_NUM_CONNECTS           , I}
#{curlinfo CURLINFO_SSL_ENGINES            , L}
#{curlinfo CURLINFO_COOKIELIST             , L}
#{curlinfo CURLINFO_LASTSOCKET             , I}
#{curlinfo CURLINFO_FTP_ENTRY_PATH         , S}
#{curlinfo CURLINFO_REDIRECT_URL           , S}
#{curlinfo CURLINFO_PRIMARY_IP             , S}
#{curlinfo CURLINFO_APPCONNECT_TIME        , D}
#{curlinfo CURLINFO_CERTINFO               , L}
#{curlinfo CURLINFO_CONDITION_UNMET        , I}
#{curlinfo CURLINFO_RTSP_SESSION_ID        , S}
#{curlinfo CURLINFO_RTSP_CLIENT_CSEQ       , I}
#{curlinfo CURLINFO_RTSP_SERVER_CSEQ       , I}
#{curlinfo CURLINFO_RTSP_CSEQ_RECV         , I}
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_0
#{curlinfo CURLINFO_PRIMARY_PORT           , I}
#{curlinfo CURLINFO_LOCAL_IP               , S}
#{curlinfo CURLINFO_LOCAL_PORT             , I}
#endif




-------------------------------------------------------------------------------
#if LIBCURL_VERSION_NUM >= CURL_7_20_0
#{symbol CURLVERSION_FIRST , CURLversion}
#{symbol CURLVERSION_SECOND, CURLversion}
#{symbol CURLVERSION_THIRD , CURLversion}
#{symbol CURLVERSION_FOURTH, CURLversion}
#{symbol CURLVERSION_NOW   , CURLversion}
#endif


-------------------------------------------------------------------------------
#if LIBCURL_VERSION_NUM >= CURL_7_20_0
#{cconst CURL_VERSION_IPV6        , CInt}
#{cconst CURL_VERSION_KERBEROS4   , CInt}
#{cconst CURL_VERSION_SSL         , CInt}
#{cconst CURL_VERSION_LIBZ        , CInt}
#{cconst CURL_VERSION_NTLM        , CInt}
#{cconst CURL_VERSION_GSSNEGOTIATE, CInt}
#{cconst CURL_VERSION_DEBUG       , CInt}
#{cconst CURL_VERSION_ASYNCHDNS   , CInt}
#{cconst CURL_VERSION_SPNEGO      , CInt}
#{cconst CURL_VERSION_LARGEFILE   , CInt}
#{cconst CURL_VERSION_IDN         , CInt}
#{cconst CURL_VERSION_SSPI        , CInt}
#{cconst CURL_VERSION_CONV        , CInt}
#{cconst CURL_VERSION_CURLDEBUG   , CInt}
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_4
#{cconst CURL_VERSION_TLSAUTH_SRP , CInt}
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_22_0
#{cconst CURL_VERSION_NTLM_WB     , CInt}
#endif

