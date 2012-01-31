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

#{include "curl/curl.h"}


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


#{enum CCURLcode, CCURLcode
  , cCURLE_OK                       = CURLE_OK
  , cCURLE_UNSUPPORTED_PROTOCOL     = CURLE_UNSUPPORTED_PROTOCOL
  , cCURLE_FAILED_INIT              = CURLE_FAILED_INIT
  , cCURLE_URL_MALFORMAT            = CURLE_URL_MALFORMAT
  , cCURLE_OBSOLETE4                = CURLE_OBSOLETE4
  , cCURLE_COULDNT_RESOLVE_PROXY    = CURLE_COULDNT_RESOLVE_PROXY
  , cCURLE_COULDNT_RESOLVE_HOST     = CURLE_COULDNT_RESOLVE_HOST
  , cCURLE_COULDNT_CONNECT          = CURLE_COULDNT_CONNECT
  , cCURLE_FTP_WEIRD_SERVER_REPLY   = CURLE_FTP_WEIRD_SERVER_REPLY
  , cCURLE_REMOTE_ACCESS_DENIED     = CURLE_REMOTE_ACCESS_DENIED
  , cCURLE_OBSOLETE10               = CURLE_OBSOLETE10
  , cCURLE_FTP_WEIRD_PASS_REPLY     = CURLE_FTP_WEIRD_PASS_REPLY
  , cCURLE_OBSOLETE12               = CURLE_OBSOLETE12
  , cCURLE_FTP_WEIRD_PASV_REPLY     = CURLE_FTP_WEIRD_PASV_REPLY
  , cCURLE_FTP_WEIRD_227_FORMAT     = CURLE_FTP_WEIRD_227_FORMAT
  , cCURLE_FTP_CANT_GET_HOST        = CURLE_FTP_CANT_GET_HOST
  , cCURLE_OBSOLETE16               = CURLE_OBSOLETE16
  , cCURLE_FTP_COULDNT_SET_TYPE     = CURLE_FTP_COULDNT_SET_TYPE
  , cCURLE_PARTIAL_FILE             = CURLE_PARTIAL_FILE
  , cCURLE_FTP_COULDNT_RETR_FILE    = CURLE_FTP_COULDNT_RETR_FILE
  , cCURLE_OBSOLETE20               = CURLE_OBSOLETE20
  , cCURLE_QUOTE_ERROR              = CURLE_QUOTE_ERROR
  , cCURLE_HTTP_RETURNED_ERROR      = CURLE_HTTP_RETURNED_ERROR
  , cCURLE_WRITE_ERROR              = CURLE_WRITE_ERROR
  , cCURLE_OBSOLETE24               = CURLE_OBSOLETE24
  , cCURLE_UPLOAD_FAILED            = CURLE_UPLOAD_FAILED
  , cCURLE_READ_ERROR               = CURLE_READ_ERROR
  , cCURLE_OUT_OF_MEMORY            = CURLE_OUT_OF_MEMORY
  , cCURLE_OPERATION_TIMEDOUT       = CURLE_OPERATION_TIMEDOUT
  , cCURLE_OBSOLETE29               = CURLE_OBSOLETE29
  , cCURLE_FTP_PORT_FAILED          = CURLE_FTP_PORT_FAILED
  , cCURLE_FTP_COULDNT_USE_REST     = CURLE_FTP_COULDNT_USE_REST
  , cCURLE_OBSOLETE32               = CURLE_OBSOLETE32
  , cCURLE_RANGE_ERROR              = CURLE_RANGE_ERROR
  , cCURLE_HTTP_POST_ERROR          = CURLE_HTTP_POST_ERROR
  , cCURLE_SSL_CONNECT_ERROR        = CURLE_SSL_CONNECT_ERROR
  , cCURLE_BAD_DOWNLOAD_RESUME      = CURLE_BAD_DOWNLOAD_RESUME
  , cCURLE_FILE_COULDNT_READ_FILE   = CURLE_FILE_COULDNT_READ_FILE
  , cCURLE_LDAP_CANNOT_BIND         = CURLE_LDAP_CANNOT_BIND
  , cCURLE_LDAP_SEARCH_FAILED       = CURLE_LDAP_SEARCH_FAILED
  , cCURLE_OBSOLETE40               = CURLE_OBSOLETE40
  , cCURLE_FUNCTION_NOT_FOUND       = CURLE_FUNCTION_NOT_FOUND
  , cCURLE_ABORTED_BY_CALLBACK      = CURLE_ABORTED_BY_CALLBACK
  , cCURLE_BAD_FUNCTION_ARGUMENT    = CURLE_BAD_FUNCTION_ARGUMENT
  , cCURLE_OBSOLETE44               = CURLE_OBSOLETE44
  , cCURLE_INTERFACE_FAILED         = CURLE_INTERFACE_FAILED
  , cCURLE_OBSOLETE46               = CURLE_OBSOLETE46
  , cCURLE_TOO_MANY_REDIRECTS       = CURLE_TOO_MANY_REDIRECTS
  , cCURLE_UNKNOWN_TELNET_OPTION    = CURLE_UNKNOWN_TELNET_OPTION
  , cCURLE_TELNET_OPTION_SYNTAX     = CURLE_TELNET_OPTION_SYNTAX
  , cCURLE_OBSOLETE50               = CURLE_OBSOLETE50
  , cCURLE_PEER_FAILED_VERIFICATION = CURLE_PEER_FAILED_VERIFICATION
  , cCURLE_GOT_NOTHING              = CURLE_GOT_NOTHING
  , cCURLE_SSL_ENGINE_NOTFOUND      = CURLE_SSL_ENGINE_NOTFOUND
  , cCURLE_SSL_ENGINE_SETFAILED     = CURLE_SSL_ENGINE_SETFAILED
  , cCURLE_SEND_ERROR               = CURLE_SEND_ERROR
  , cCURLE_RECV_ERROR               = CURLE_RECV_ERROR
  , cCURLE_OBSOLETE57               = CURLE_OBSOLETE57
  , cCURLE_SSL_CERTPROBLEM          = CURLE_SSL_CERTPROBLEM
  , cCURLE_SSL_CIPHER               = CURLE_SSL_CIPHER
  , cCURLE_SSL_CACERT               = CURLE_SSL_CACERT
  , cCURLE_BAD_CONTENT_ENCODING     = CURLE_BAD_CONTENT_ENCODING
  , cCURLE_LDAP_INVALID_URL         = CURLE_LDAP_INVALID_URL
  , cCURLE_FILESIZE_EXCEEDED        = CURLE_FILESIZE_EXCEEDED
  , cCURLE_USE_SSL_FAILED           = CURLE_USE_SSL_FAILED
  , cCURLE_SEND_FAIL_REWIND         = CURLE_SEND_FAIL_REWIND
  , cCURLE_SSL_ENGINE_INITFAILED    = CURLE_SSL_ENGINE_INITFAILED
  , cCURLE_LOGIN_DENIED             = CURLE_LOGIN_DENIED
  , cCURLE_TFTP_NOTFOUND            = CURLE_TFTP_NOTFOUND
  , cCURLE_TFTP_PERM                = CURLE_TFTP_PERM
  , cCURLE_REMOTE_DISK_FULL         = CURLE_REMOTE_DISK_FULL
  , cCURLE_TFTP_ILLEGAL             = CURLE_TFTP_ILLEGAL
  , cCURLE_TFTP_UNKNOWNID           = CURLE_TFTP_UNKNOWNID
  , cCURLE_REMOTE_FILE_EXISTS       = CURLE_REMOTE_FILE_EXISTS
  , cCURLE_TFTP_NOSUCHUSER          = CURLE_TFTP_NOSUCHUSER
  , cCURLE_CONV_FAILED              = CURLE_CONV_FAILED
  , cCURLE_CONV_REQD                = CURLE_CONV_REQD
  , cCURLE_SSL_CACERT_BADFILE       = CURLE_SSL_CACERT_BADFILE
  , cCURLE_REMOTE_FILE_NOT_FOUND    = CURLE_REMOTE_FILE_NOT_FOUND
  , cCURLE_SSH                      = CURLE_SSH
  , cCURLE_SSL_SHUTDOWN_FAILED      = CURLE_SSL_SHUTDOWN_FAILED
  , cCURLE_AGAIN                    = CURLE_AGAIN
  , cCURLE_SSL_CRL_BADFILE          = CURLE_SSL_CRL_BADFILE
  , cCURLE_SSL_ISSUER_ERROR         = CURLE_SSL_ISSUER_ERROR
  , cCURLE_FTP_PRET_FAILED          = CURLE_FTP_PRET_FAILED
  , cCURLE_RTSP_CSEQ_ERROR          = CURLE_RTSP_CSEQ_ERROR
  , cCURLE_RTSP_SESSION_ERROR       = CURLE_RTSP_SESSION_ERROR
  , cCURLE_FTP_BAD_FILE_LIST        = CURLE_FTP_BAD_FILE_LIST
  , cCURLE_CHUNK_FAILED             = CURLE_CHUNK_FAILED
  }


#{enum CCURLoption_I, CCURLoption_I
  , cCURLOPT_PORT                    = CURLOPT_PORT
  , cCURLOPT_TIMEOUT                 = CURLOPT_TIMEOUT
  , cCURLOPT_INFILESIZE              = CURLOPT_INFILESIZE
  , cCURLOPT_LOW_SPEED_LIMIT         = CURLOPT_LOW_SPEED_LIMIT
  , cCURLOPT_LOW_SPEED_TIME          = CURLOPT_LOW_SPEED_TIME
  , cCURLOPT_RESUME_FROM             = CURLOPT_RESUME_FROM
  , cCURLOPT_CRLF                    = CURLOPT_CRLF
  , cCURLOPT_SSLVERSION              = CURLOPT_SSLVERSION
  , cCURLOPT_TIMECONDITION           = CURLOPT_TIMECONDITION
  , cCURLOPT_TIMEVALUE               = CURLOPT_TIMEVALUE
  , cCURLOPT_VERBOSE                 = CURLOPT_VERBOSE
  , cCURLOPT_HEADER                  = CURLOPT_HEADER
  , cCURLOPT_NOPROGRESS              = CURLOPT_NOPROGRESS
  , cCURLOPT_NOBODY                  = CURLOPT_NOBODY
  , cCURLOPT_FAILONERROR             = CURLOPT_FAILONERROR
  , cCURLOPT_UPLOAD                  = CURLOPT_UPLOAD
  , cCURLOPT_POST                    = CURLOPT_POST
  , cCURLOPT_DIRLISTONLY             = CURLOPT_DIRLISTONLY
  , cCURLOPT_APPEND                  = CURLOPT_APPEND
  , cCURLOPT_NETRC                   = CURLOPT_NETRC
  , cCURLOPT_FOLLOWLOCATION          = CURLOPT_FOLLOWLOCATION
  , cCURLOPT_TRANSFERTEXT            = CURLOPT_TRANSFERTEXT
  , cCURLOPT_PUT                     = CURLOPT_PUT
  , cCURLOPT_AUTOREFERER             = CURLOPT_AUTOREFERER
  , cCURLOPT_PROXYPORT               = CURLOPT_PROXYPORT
  , cCURLOPT_POSTFIELDSIZE           = CURLOPT_POSTFIELDSIZE
  , cCURLOPT_HTTPPROXYTUNNEL         = CURLOPT_HTTPPROXYTUNNEL
  , cCURLOPT_SSL_VERIFYPEER          = CURLOPT_SSL_VERIFYPEER
  , cCURLOPT_MAXREDIRS               = CURLOPT_MAXREDIRS
  , cCURLOPT_FILETIME                = CURLOPT_FILETIME
  , cCURLOPT_MAXCONNECTS             = CURLOPT_MAXCONNECTS
  , cCURLOPT_CLOSEPOLICY             = CURLOPT_CLOSEPOLICY
  , cCURLOPT_FRESH_CONNECT           = CURLOPT_FRESH_CONNECT
  , cCURLOPT_FORBID_REUSE            = CURLOPT_FORBID_REUSE
  , cCURLOPT_CONNECTTIMEOUT          = CURLOPT_CONNECTTIMEOUT
  , cCURLOPT_HTTPGET                 = CURLOPT_HTTPGET
  , cCURLOPT_SSL_VERIFYHOST          = CURLOPT_SSL_VERIFYHOST
  , cCURLOPT_HTTP_VERSION            = CURLOPT_HTTP_VERSION
  , cCURLOPT_FTP_USE_EPSV            = CURLOPT_FTP_USE_EPSV
  , cCURLOPT_SSLENGINE_DEFAULT       = CURLOPT_SSLENGINE_DEFAULT
  , cCURLOPT_DNS_USE_GLOBAL_CACHE    = CURLOPT_DNS_USE_GLOBAL_CACHE
  , cCURLOPT_DNS_CACHE_TIMEOUT       = CURLOPT_DNS_CACHE_TIMEOUT
  , cCURLOPT_COOKIESESSION           = CURLOPT_COOKIESESSION
  , cCURLOPT_BUFFERSIZE              = CURLOPT_BUFFERSIZE
  , cCURLOPT_NOSIGNAL                = CURLOPT_NOSIGNAL
  , cCURLOPT_PROXYTYPE               = CURLOPT_PROXYTYPE
  , cCURLOPT_UNRESTRICTED_AUTH       = CURLOPT_UNRESTRICTED_AUTH
  , cCURLOPT_FTP_USE_EPRT            = CURLOPT_FTP_USE_EPRT
  , cCURLOPT_HTTPAUTH                = CURLOPT_HTTPAUTH
  , cCURLOPT_FTP_CREATE_MISSING_DIRS = CURLOPT_FTP_CREATE_MISSING_DIRS
  , cCURLOPT_PROXYAUTH               = CURLOPT_PROXYAUTH
  , cCURLOPT_FTP_RESPONSE_TIMEOUT    = CURLOPT_FTP_RESPONSE_TIMEOUT
  , cCURLOPT_IPRESOLVE               = CURLOPT_IPRESOLVE
  , cCURLOPT_MAXFILESIZE             = CURLOPT_MAXFILESIZE
  , cCURLOPT_USE_SSL                 = CURLOPT_USE_SSL
  , cCURLOPT_TCP_NODELAY             = CURLOPT_TCP_NODELAY
  , cCURLOPT_FTPSSLAUTH              = CURLOPT_FTPSSLAUTH
  , cCURLOPT_IGNORE_CONTENT_LENGTH   = CURLOPT_IGNORE_CONTENT_LENGTH
  , cCURLOPT_FTP_SKIP_PASV_IP        = CURLOPT_FTP_SKIP_PASV_IP
  , cCURLOPT_FTP_FILEMETHOD          = CURLOPT_FTP_FILEMETHOD
  , cCURLOPT_LOCALPORT               = CURLOPT_LOCALPORT
  , cCURLOPT_LOCALPORTRANGE          = CURLOPT_LOCALPORTRANGE
  , cCURLOPT_CONNECT_ONLY            = CURLOPT_CONNECT_ONLY
  , cCURLOPT_SSL_SESSIONID_CACHE     = CURLOPT_SSL_SESSIONID_CACHE
  , cCURLOPT_SSH_AUTH_TYPES          = CURLOPT_SSH_AUTH_TYPES
  , cCURLOPT_FTP_SSL_CCC             = CURLOPT_FTP_SSL_CCC
  , cCURLOPT_TIMEOUT_MS              = CURLOPT_TIMEOUT_MS
  , cCURLOPT_CONNECTTIMEOUT_MS       = CURLOPT_CONNECTTIMEOUT_MS
  , cCURLOPT_HTTP_TRANSFER_DECODING  = CURLOPT_HTTP_TRANSFER_DECODING
  , cCURLOPT_HTTP_CONTENT_DECODING   = CURLOPT_HTTP_CONTENT_DECODING
  , cCURLOPT_NEW_FILE_PERMS          = CURLOPT_NEW_FILE_PERMS
  , cCURLOPT_NEW_DIRECTORY_PERMS     = CURLOPT_NEW_DIRECTORY_PERMS
  , cCURLOPT_POSTREDIR               = CURLOPT_POSTREDIR
  , cCURLOPT_PROXY_TRANSFER_MODE     = CURLOPT_PROXY_TRANSFER_MODE
  , cCURLOPT_ADDRESS_SCOPE           = CURLOPT_ADDRESS_SCOPE
  , cCURLOPT_CERTINFO                = CURLOPT_CERTINFO
  , cCURLOPT_TFTP_BLKSIZE            = CURLOPT_TFTP_BLKSIZE
  , cCURLOPT_SOCKS5_GSSAPI_NEC       = CURLOPT_SOCKS5_GSSAPI_NEC
  , cCURLOPT_PROTOCOLS               = CURLOPT_PROTOCOLS
  , cCURLOPT_REDIR_PROTOCOLS         = CURLOPT_REDIR_PROTOCOLS
  , cCURLOPT_FTP_USE_PRET            = CURLOPT_FTP_USE_PRET
  , cCURLOPT_RTSP_REQUEST            = CURLOPT_RTSP_REQUEST
  , cCURLOPT_RTSP_CLIENT_CSEQ        = CURLOPT_RTSP_CLIENT_CSEQ
  , cCURLOPT_RTSP_SERVER_CSEQ        = CURLOPT_RTSP_SERVER_CSEQ
  , cCURLOPT_WILDCARDMATCH           = CURLOPT_WILDCARDMATCH
  } 
--  , cCURLOPT_TRANSFER_ENCODING       = CURLOPT_TRANSFER_ENCODING
--  , cCURLOPT_GSSAPI_DELEGATION       = CURLOPT_GSSAPI_DELEGATION

#{enum CCURLoption_P, CCURLoption_P
  , cCURLOPT_FILE                    = CURLOPT_FILE
  , cCURLOPT_URL                     = CURLOPT_URL
  , cCURLOPT_PROXY                   = CURLOPT_PROXY
  , cCURLOPT_USERPWD                 = CURLOPT_USERPWD
  , cCURLOPT_PROXYUSERPWD            = CURLOPT_PROXYUSERPWD
  , cCURLOPT_RANGE                   = CURLOPT_RANGE
  , cCURLOPT_INFILE                  = CURLOPT_INFILE
  , cCURLOPT_ERRORBUFFER             = CURLOPT_ERRORBUFFER
  , cCURLOPT_POSTFIELDS              = CURLOPT_POSTFIELDS
  , cCURLOPT_REFERER                 = CURLOPT_REFERER
  , cCURLOPT_FTPPORT                 = CURLOPT_FTPPORT
  , cCURLOPT_USERAGENT               = CURLOPT_USERAGENT
  , cCURLOPT_COOKIE                  = CURLOPT_COOKIE
  , cCURLOPT_HTTPHEADER              = CURLOPT_HTTPHEADER
  , cCURLOPT_HTTPPOST                = CURLOPT_HTTPPOST
  , cCURLOPT_SSLCERT                 = CURLOPT_SSLCERT
  , cCURLOPT_KEYPASSWD               = CURLOPT_KEYPASSWD
  , cCURLOPT_QUOTE                   = CURLOPT_QUOTE
  , cCURLOPT_WRITEHEADER             = CURLOPT_WRITEHEADER
  , cCURLOPT_COOKIEFILE              = CURLOPT_COOKIEFILE
  , cCURLOPT_CUSTOMREQUEST           = CURLOPT_CUSTOMREQUEST
  , cCURLOPT_STDERR                  = CURLOPT_STDERR
  , cCURLOPT_POSTQUOTE               = CURLOPT_POSTQUOTE
  , cCURLOPT_WRITEINFO               = CURLOPT_WRITEINFO
  , cCURLOPT_PROGRESSDATA            = CURLOPT_PROGRESSDATA
  , cCURLOPT_INTERFACE               = CURLOPT_INTERFACE
  , cCURLOPT_KRBLEVEL                = CURLOPT_KRBLEVEL
  , cCURLOPT_CAINFO                  = CURLOPT_CAINFO
  , cCURLOPT_TELNETOPTIONS           = CURLOPT_TELNETOPTIONS
  , cCURLOPT_RANDOM_FILE             = CURLOPT_RANDOM_FILE
  , cCURLOPT_EGDSOCKET               = CURLOPT_EGDSOCKET
  , cCURLOPT_COOKIEJAR               = CURLOPT_COOKIEJAR
  , cCURLOPT_SSL_CIPHER_LIST         = CURLOPT_SSL_CIPHER_LIST
  , cCURLOPT_SSLCERTTYPE             = CURLOPT_SSLCERTTYPE
  , cCURLOPT_SSLKEY                  = CURLOPT_SSLKEY
  , cCURLOPT_SSLKEYTYPE              = CURLOPT_SSLKEYTYPE
  , cCURLOPT_SSLENGINE               = CURLOPT_SSLENGINE
  , cCURLOPT_PREQUOTE                = CURLOPT_PREQUOTE
  , cCURLOPT_DEBUGDATA               = CURLOPT_DEBUGDATA
  , cCURLOPT_CAPATH                  = CURLOPT_CAPATH
  , cCURLOPT_SHARE                   = CURLOPT_SHARE
  }
--  , cCURLOPT_ACCEPT_ENCODING         = CURLOPT_ACCEPT_ENCODING
#{enum CCURLoption_P, CCURLoption_P
  , cCURLOPT_PRIVATE                 = CURLOPT_PRIVATE
  , cCURLOPT_HTTP200ALIASES          = CURLOPT_HTTP200ALIASES
  , cCURLOPT_SSL_CTX_DATA            = CURLOPT_SSL_CTX_DATA
  , cCURLOPT_NETRC_FILE              = CURLOPT_NETRC_FILE
  , cCURLOPT_IOCTLDATA               = CURLOPT_IOCTLDATA
  , cCURLOPT_FTP_ACCOUNT             = CURLOPT_FTP_ACCOUNT
  , cCURLOPT_COOKIELIST              = CURLOPT_COOKIELIST
  , cCURLOPT_FTP_ALTERNATIVE_TO_USER = CURLOPT_FTP_ALTERNATIVE_TO_USER
  , cCURLOPT_SOCKOPTDATA             = CURLOPT_SOCKOPTDATA
  , cCURLOPT_SSH_PUBLIC_KEYFILE      = CURLOPT_SSH_PUBLIC_KEYFILE
  , cCURLOPT_SSH_PRIVATE_KEYFILE     = CURLOPT_SSH_PRIVATE_KEYFILE
  , cCURLOPT_SSH_HOST_PUBLIC_KEY_MD5 = CURLOPT_SSH_HOST_PUBLIC_KEY_MD5
  , cCURLOPT_OPENSOCKETDATA          = CURLOPT_OPENSOCKETDATA
  , cCURLOPT_COPYPOSTFIELDS          = CURLOPT_COPYPOSTFIELDS
  , cCURLOPT_SEEKDATA                = CURLOPT_SEEKDATA
  , cCURLOPT_CRLFILE                 = CURLOPT_CRLFILE
  , cCURLOPT_ISSUERCERT              = CURLOPT_ISSUERCERT
  , cCURLOPT_USERNAME                = CURLOPT_USERNAME
  , cCURLOPT_PASSWORD                = CURLOPT_PASSWORD
  , cCURLOPT_PROXYUSERNAME           = CURLOPT_PROXYUSERNAME
  , cCURLOPT_PROXYPASSWORD           = CURLOPT_PROXYPASSWORD
  , cCURLOPT_NOPROXY                 = CURLOPT_NOPROXY
  , cCURLOPT_SOCKS5_GSSAPI_SERVICE   = CURLOPT_SOCKS5_GSSAPI_SERVICE
  , cCURLOPT_SSH_KNOWNHOSTS          = CURLOPT_SSH_KNOWNHOSTS
  , cCURLOPT_SSH_KEYDATA             = CURLOPT_SSH_KEYDATA
  , cCURLOPT_MAIL_FROM               = CURLOPT_MAIL_FROM
  , cCURLOPT_MAIL_RCPT               = CURLOPT_MAIL_RCPT
  , cCURLOPT_RTSP_SESSION_ID         = CURLOPT_RTSP_SESSION_ID
  , cCURLOPT_RTSP_STREAM_URI         = CURLOPT_RTSP_STREAM_URI
  , cCURLOPT_RTSP_TRANSPORT          = CURLOPT_RTSP_TRANSPORT
  , cCURLOPT_INTERLEAVEDATA          = CURLOPT_INTERLEAVEDATA
  , cCURLOPT_CHUNK_DATA              = CURLOPT_CHUNK_DATA
  , cCURLOPT_FNMATCH_DATA            = CURLOPT_FNMATCH_DATA
  , cCURLOPT_RESOLVE                 = CURLOPT_RESOLVE
  }
--  , cCURLOPT_TLSAUTH_USERNAME        = CURLOPT_TLSAUTH_USERNAME
--  , cCURLOPT_TLSAUTH_PASSWORD        = CURLOPT_TLSAUTH_PASSWORD
--  , cCURLOPT_TLSAUTH_TYPE            = CURLOPT_TLSAUTH_TYPE
--  , cCURLOPT_CLOSESOCKETDATA         = CURLOPT_CLOSESOCKETDATA


#{enum CCURLoption_F, CCURLoption_F
  , cCURLOPT_WRITEFUNCTION              = CURLOPT_WRITEFUNCTION
  , cCURLOPT_READFUNCTION               = CURLOPT_READFUNCTION
  , cCURLOPT_PROGRESSFUNCTION           = CURLOPT_PROGRESSFUNCTION
  , cCURLOPT_HEADERFUNCTION             = CURLOPT_HEADERFUNCTION
  , cCURLOPT_DEBUGFUNCTION              = CURLOPT_DEBUGFUNCTION
  , cCURLOPT_SSL_CTX_FUNCTION           = CURLOPT_SSL_CTX_FUNCTION
  , cCURLOPT_IOCTLFUNCTION              = CURLOPT_IOCTLFUNCTION
  , cCURLOPT_CONV_FROM_NETWORK_FUNCTION = CURLOPT_CONV_FROM_NETWORK_FUNCTION
  , cCURLOPT_CONV_TO_NETWORK_FUNCTION   = CURLOPT_CONV_TO_NETWORK_FUNCTION
  , cCURLOPT_CONV_FROM_UTF8_FUNCTION    = CURLOPT_CONV_FROM_UTF8_FUNCTION
  , cCURLOPT_SOCKOPTFUNCTION            = CURLOPT_SOCKOPTFUNCTION
  , cCURLOPT_OPENSOCKETFUNCTION         = CURLOPT_OPENSOCKETFUNCTION
  , cCURLOPT_SEEKFUNCTION               = CURLOPT_SEEKFUNCTION
  , cCURLOPT_SSH_KEYFUNCTION            = CURLOPT_SSH_KEYFUNCTION
  , cCURLOPT_INTERLEAVEFUNCTION         = CURLOPT_INTERLEAVEFUNCTION
  , cCURLOPT_CHUNK_BGN_FUNCTION         = CURLOPT_CHUNK_BGN_FUNCTION
  , cCURLOPT_CHUNK_END_FUNCTION         = CURLOPT_CHUNK_END_FUNCTION
  , cCURLOPT_FNMATCH_FUNCTION           = CURLOPT_FNMATCH_FUNCTION
  }
--  , cCURLOPT_CLOSESOCKETFUNCTION        = CURLOPT_CLOSESOCKETFUNCTION


#{enum CCURLoption_O, CCURLoption_O
  , cCURLOPT_INFILESIZE_LARGE     = CURLOPT_INFILESIZE_LARGE
  , cCURLOPT_RESUME_FROM_LARGE    = CURLOPT_RESUME_FROM_LARGE
  , cCURLOPT_MAXFILESIZE_LARGE    = CURLOPT_MAXFILESIZE_LARGE
  , cCURLOPT_POSTFIELDSIZE_LARGE  = CURLOPT_POSTFIELDSIZE_LARGE
  , cCURLOPT_MAX_SEND_SPEED_LARGE = CURLOPT_MAX_SEND_SPEED_LARGE
  , cCURLOPT_MAX_RECV_SPEED_LARGE = CURLOPT_MAX_RECV_SPEED_LARGE
  }


#{enum CCURLinfo_S, CCURLinfo_S
  , cCURLINFO_EFFECTIVE_URL           = CURLINFO_EFFECTIVE_URL
  , cCURLINFO_CONTENT_TYPE            = CURLINFO_CONTENT_TYPE
  , cCURLINFO_PRIVATE                 = CURLINFO_PRIVATE
  , cCURLINFO_FTP_ENTRY_PATH          = CURLINFO_FTP_ENTRY_PATH
  , cCURLINFO_REDIRECT_URL            = CURLINFO_REDIRECT_URL
  , cCURLINFO_PRIMARY_IP              = CURLINFO_PRIMARY_IP
  , cCURLINFO_RTSP_SESSION_ID         = CURLINFO_RTSP_SESSION_ID
  , cCURLINFO_LOCAL_IP                = CURLINFO_LOCAL_IP
  }

#{enum CCURLinfo_I, CCURLinfo_I
  , cCURLINFO_RESPONSE_CODE           = CURLINFO_RESPONSE_CODE
  , cCURLINFO_HEADER_SIZE             = CURLINFO_HEADER_SIZE
  , cCURLINFO_REQUEST_SIZE            = CURLINFO_REQUEST_SIZE
  , cCURLINFO_SSL_VERIFYRESULT        = CURLINFO_SSL_VERIFYRESULT
  , cCURLINFO_FILETIME                = CURLINFO_FILETIME
  , cCURLINFO_REDIRECT_COUNT          = CURLINFO_REDIRECT_COUNT
  , cCURLINFO_HTTP_CONNECTCODE        = CURLINFO_HTTP_CONNECTCODE
  , cCURLINFO_HTTPAUTH_AVAIL          = CURLINFO_HTTPAUTH_AVAIL
  , cCURLINFO_PROXYAUTH_AVAIL         = CURLINFO_PROXYAUTH_AVAIL
  , cCURLINFO_OS_ERRNO                = CURLINFO_OS_ERRNO
  , cCURLINFO_NUM_CONNECTS            = CURLINFO_NUM_CONNECTS
  , cCURLINFO_LASTSOCKET              = CURLINFO_LASTSOCKET
  , cCURLINFO_CONDITION_UNMET         = CURLINFO_CONDITION_UNMET
  , cCURLINFO_RTSP_CLIENT_CSEQ        = CURLINFO_RTSP_CLIENT_CSEQ
  , cCURLINFO_RTSP_SERVER_CSEQ        = CURLINFO_RTSP_SERVER_CSEQ
  , cCURLINFO_RTSP_CSEQ_RECV          = CURLINFO_RTSP_CSEQ_RECV
  , cCURLINFO_PRIMARY_PORT            = CURLINFO_PRIMARY_PORT
  , cCURLINFO_LOCAL_PORT              = CURLINFO_LOCAL_PORT
  }

#{enum CCURLinfo_D, CCURLinfo_D
  , cCURLINFO_TOTAL_TIME              = CURLINFO_TOTAL_TIME
  , cCURLINFO_NAMELOOKUP_TIME         = CURLINFO_NAMELOOKUP_TIME
  , cCURLINFO_CONNECT_TIME            = CURLINFO_CONNECT_TIME
  , cCURLINFO_PRETRANSFER_TIME        = CURLINFO_PRETRANSFER_TIME
  , cCURLINFO_SIZE_UPLOAD             = CURLINFO_SIZE_UPLOAD
  , cCURLINFO_SIZE_DOWNLOAD           = CURLINFO_SIZE_DOWNLOAD
  , cCURLINFO_SPEED_DOWNLOAD          = CURLINFO_SPEED_DOWNLOAD
  , cCURLINFO_SPEED_UPLOAD            = CURLINFO_SPEED_UPLOAD
  , cCURLINFO_CONTENT_LENGTH_DOWNLOAD = CURLINFO_CONTENT_LENGTH_DOWNLOAD
  , cCURLINFO_CONTENT_LENGTH_UPLOAD   = CURLINFO_CONTENT_LENGTH_UPLOAD
  , cCURLINFO_STARTTRANSFER_TIME      = CURLINFO_STARTTRANSFER_TIME
  , cCURLINFO_REDIRECT_TIME           = CURLINFO_REDIRECT_TIME
  , cCURLINFO_APPCONNECT_TIME         = CURLINFO_APPCONNECT_TIME
  }

#{enum CCURLinfo_L, CCURLinfo_L
  , cCURLINFO_SSL_ENGINES             = CURLINFO_SSL_ENGINES
  , cCURLINFO_COOKIELIST              = CURLINFO_COOKIELIST
  , cCURLINFO_CERTINFO                = CURLINFO_CERTINFO
  }


#{enum CCURLversion, CCURLversion
  , cCURLVERSION_FIRST  = CURLVERSION_FIRST
  , cCURLVERSION_SECOND = CURLVERSION_SECOND
  , cCURLVERSION_THIRD  = CURLVERSION_THIRD
  , cCURLVERSION_FOURTH = CURLVERSION_FOURTH
  , cCURLVERSION_NOW    = CURLVERSION_NOW
  }

#{enum CInt,
  , cCURL_VERSION_IPV6         = CURL_VERSION_IPV6
  , cCURL_VERSION_KERBEROS4    = CURL_VERSION_KERBEROS4
  , cCURL_VERSION_SSL          = CURL_VERSION_SSL
  , cCURL_VERSION_LIBZ         = CURL_VERSION_LIBZ
  , cCURL_VERSION_NTLM         = CURL_VERSION_NTLM
  , cCURL_VERSION_GSSNEGOTIATE = CURL_VERSION_GSSNEGOTIATE
  , cCURL_VERSION_DEBUG        = CURL_VERSION_DEBUG
  , cCURL_VERSION_ASYNCHDNS    = CURL_VERSION_ASYNCHDNS
  , cCURL_VERSION_SPNEGO       = CURL_VERSION_SPNEGO
  , cCURL_VERSION_LARGEFILE    = CURL_VERSION_LARGEFILE
  , cCURL_VERSION_IDN          = CURL_VERSION_IDN
  , cCURL_VERSION_SSPI         = CURL_VERSION_SSPI
  , cCURL_VERSION_CONV         = CURL_VERSION_CONV
  , cCURL_VERSION_CURLDEBUG    = CURL_VERSION_CURLDEBUG
  }

