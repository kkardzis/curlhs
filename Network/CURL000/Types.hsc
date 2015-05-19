-------------------------------------------------------------------------------
-- |
-- Module      :  Network.CURL000.Types
-- Copyright   :  Copyright (c) 2012-2015 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
--
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE GADTs              #-}

module Network.CURL000.Types where

import qualified Network.CURL000.LibCC as C

import Control.Exception  (Exception)
import Control.Concurrent (MVar)

import Data.Time.Clock (UTCTime)
import Data.ByteString (ByteString)
import Data.Typeable   (Typeable, typeOf)
import Data.Maybe      (mapMaybe)
import Data.List       (foldl')
import Data.Bits       ((.|.), (.&.))
import Data.Int        (Int64)
import Data.Unique     (Unique, hashUnique)
import Data.IORef      (IORef)

import Foreign.C.Types
import Foreign.Ptr

#include "LibC0.c"


-------------------------------------------------------------------------------
class ENUM a where
  toENUM :: a -> CULong
  enumlist :: [a]

instance ENUM a => ENUM [a] where
  toENUM = foldl' (.|.) 0 . map toENUM
  enumlist = []

toCInt :: ENUM a => a -> CInt
toCInt = fromIntegral . toENUM

toCLong :: ENUM a => a -> CLong
toCLong = fromIntegral . toENUM

fromCIntMask :: ENUM a => CInt -> [a]
fromCIntMask mask =
  let checkBit x = if ((mask .&. (toCInt x)) == 0) then Nothing else Just x
  in  mapMaybe checkBit enumlist

fromCLongMask :: ENUM a => CLong -> [a]
fromCLongMask mask =
  let checkBit x = if ((mask .&. (toCLong x)) == 0) then Nothing else Just x
  in  mapMaybe checkBit enumlist

fromCInt :: (ENUM a, Typeable a) => CInt -> a
fromCInt cval =
  let enums = map (\enum -> (toCInt enum, enum)) enumlist
      enumE = error $ concat ["<curlhs> unknown constant (", v, ") -> ", t]
      (v,t) = (show cval, show $ typeOf $ snd $ head enums)
  in  maybe enumE id $ lookup cval enums



{-
-------------------------------------------------------------------------------
deriving instance Typeable CURLMcode
deriving instance Show CURLMcode
instance Exception CURLMcode

#{ENUM CURLMcode           \
, CURLM_CALL_MULTI_PERFORM \
, CURLM_BAD_HANDLE         \
, CURLM_BAD_EASY_HANDLE    \
, CURLM_OUT_OF_MEMORY      \
, CURLM_INTERNAL_ERROR     \
, CURLM_BAD_SOCKET         \
, CURLM_UNKNOWN_OPTION     }
-}


-------------------------------------------------------------------------------
deriving instance Show CURLfeature

#{ENUM CURLfeature          \
, CURL_VERSION_IPV6         \
, CURL_VERSION_KERBEROS4    \
, CURL_VERSION_SSL          \
, CURL_VERSION_LIBZ         \
, CURL_VERSION_NTLM         \
, CURL_VERSION_GSSNEGOTIATE \
, CURL_VERSION_DEBUG        \
, CURL_VERSION_CURLDEBUG    \
, CURL_VERSION_ASYNCHDNS    \
, CURL_VERSION_SPNEGO       \
, CURL_VERSION_LARGEFILE    \
, CURL_VERSION_IDN          \
, CURL_VERSION_SSPI         \
, CURL_VERSION_CONV         \
, CURL_VERSION_TLSAUTH_SRP  \
, CURL_VERSION_NTLM_WB      }


-------------------------------------------------------------------------------
#{ENUM CURLproto   \
, CURLPROTO_ALL    \
, CURLPROTO_HTTP   \
, CURLPROTO_HTTPS  \
, CURLPROTO_FTP    \
, CURLPROTO_FTPS   \
, CURLPROTO_SCP    \
, CURLPROTO_SFTP   \
, CURLPROTO_TELNET \
, CURLPROTO_LDAP   \
, CURLPROTO_LDAPS  \
, CURLPROTO_DICT   \
, CURLPROTO_FILE   \
, CURLPROTO_TFTP   \
, CURLPROTO_IMAP   \
, CURLPROTO_IMAPS  \
, CURLPROTO_POP3   \
, CURLPROTO_POP3S  \
, CURLPROTO_SMTP   \
, CURLPROTO_SMTPS  \
, CURLPROTO_RTSP   \
, CURLPROTO_RTMP   \
, CURLPROTO_RTMPT  \
, CURLPROTO_RTMPE  \
, CURLPROTO_RTMPTE \
, CURLPROTO_RTMPS  \
, CURLPROTO_RTMPTS \
, CURLPROTO_GOPHER }


-------------------------------------------------------------------------------
#{ENUM CURLproxy            \
, CURLPROXY_HTTP            \
, CURLPROXY_HTTP_1_0        \
, CURLPROXY_SOCKS4          \
, CURLPROXY_SOCKS5          \
, CURLPROXY_SOCKS4A         \
, CURLPROXY_SOCKS5_HOSTNAME }


-------------------------------------------------------------------------------
#{ENUM CURLnetrc      \
, CURL_NETRC_IGNORED  \
, CURL_NETRC_OPTIONAL \
, CURL_NETRC_REQUIRED }


-------------------------------------------------------------------------------
#{ENUM CURLauth         \
, CURLAUTH_BASIC        \
, CURLAUTH_DIGEST       \
, CURLAUTH_DIGEST_IE    \
, CURLAUTH_GSSNEGOTIATE \
, CURLAUTH_NTLM         \
, CURLAUTH_NTLM_WB      \
, CURLAUTH_ONLY         \
, CURLAUTH_ANY          \
, CURLAUTH_ANYSAFE      }


-------------------------------------------------------------------------------
#{ENUM CURLtlsauth \
, CURL_TLSAUTH_SRP }


-------------------------------------------------------------------------------
#{ENUM CURLredir      \
, CURL_REDIR_GET_ALL  \
, CURL_REDIR_POST_301 \
, CURL_REDIR_POST_302 \
, CURL_REDIR_POST_303 \
, CURL_REDIR_POST_ALL }


-------------------------------------------------------------------------------
#{ENUM CURLhttpver       \
, CURL_HTTP_VERSION_NONE \
, CURL_HTTP_VERSION_1_0  \
, CURL_HTTP_VERSION_1_1  }


-------------------------------------------------------------------------------
#{ENUM CURLftpcreate       \
, CURLFTP_CREATE_DIR_NONE  \
, CURLFTP_CREATE_DIR       \
, CURLFTP_CREATE_DIR_RETRY }


-------------------------------------------------------------------------------
#{ENUM CURLftpauth    \
, CURLFTPAUTH_DEFAULT \
, CURLFTPAUTH_SSL     \
, CURLFTPAUTH_TLS     }


-------------------------------------------------------------------------------
#{ENUM CURLftpssl        \
, CURLFTPSSL_CCC_NONE    \
, CURLFTPSSL_CCC_PASSIVE \
, CURLFTPSSL_CCC_ACTIVE  }
 

-------------------------------------------------------------------------------
#{ENUM CURLftpmethod      \
, CURLFTPMETHOD_DEFAULT   \
, CURLFTPMETHOD_MULTICWD  \
, CURLFTPMETHOD_NOCWD     \
, CURLFTPMETHOD_SINGLECWD }


-------------------------------------------------------------------------------
#{ENUM CURLrtspreq           \
, CURL_RTSPREQ_OPTIONS       \
, CURL_RTSPREQ_DESCRIBE      \
, CURL_RTSPREQ_ANNOUNCE      \
, CURL_RTSPREQ_SETUP         \
, CURL_RTSPREQ_PLAY          \
, CURL_RTSPREQ_PAUSE         \
, CURL_RTSPREQ_TEARDOWN      \
, CURL_RTSPREQ_GET_PARAMETER \
, CURL_RTSPREQ_SET_PARAMETER \
, CURL_RTSPREQ_RECORD        \
, CURL_RTSPREQ_RECEIVE       }


-------------------------------------------------------------------------------
#{ENUM CURLtimecond          \
, CURL_TIMECOND_NONE         \
, CURL_TIMECOND_IFMODSINCE   \
, CURL_TIMECOND_IFUNMODSINCE \
, CURL_TIMECOND_LASTMOD      }


{-
-------------------------------------------------------------------------------
#{ENUM CURLclosepol                   \
, CURLCLOSEPOLICY_NONE                \
, CURLCLOSEPOLICY_OLDEST              \
, CURLCLOSEPOLICY_LEAST_RECENTLY_USED \
, CURLCLOSEPOLICY_LEAST_TRAFFIC       \
, CURLCLOSEPOLICY_SLOWEST             \
, CURLCLOSEPOLICY_CALLBACK            }
-}


-------------------------------------------------------------------------------
#{ENUM CURLipresolve      \
, CURL_IPRESOLVE_WHATEVER \
, CURL_IPRESOLVE_V4       \
, CURL_IPRESOLVE_V6       }


-------------------------------------------------------------------------------
#{ENUM CURLusessl    \
, CURLUSESSL_NONE    \
, CURLUSESSL_TRY     \
, CURLUSESSL_CONTROL \
, CURLUSESSL_ALL     }


-------------------------------------------------------------------------------
#{ENUM CURLsslver         \
, CURL_SSLVERSION_DEFAULT \
, CURL_SSLVERSION_TLSv1   \
, CURL_SSLVERSION_SSLv2   \
, CURL_SSLVERSION_SSLv3   }


-------------------------------------------------------------------------------
#{ENUM CURLsslopt        \
, CURLSSLOPT_ALLOW_BEAST }


-------------------------------------------------------------------------------
#{ENUM CURLgssapi                   \
, CURLGSSAPI_DELEGATION_NONE        \
, CURLGSSAPI_DELEGATION_POLICY_FLAG \
, CURLGSSAPI_DELEGATION_FLAG        }


-------------------------------------------------------------------------------
#{ENUM CURLsshauth       \
, CURLSSH_AUTH_ANY       \
, CURLSSH_AUTH_NONE      \
, CURLSSH_AUTH_PUBLICKEY \
, CURLSSH_AUTH_PASSWORD  \
, CURLSSH_AUTH_HOST      \
, CURLSSH_AUTH_KEYBOARD  \
, CURLSSH_AUTH_AGENT     \
, CURLSSH_AUTH_DEFAULT   }


-------------------------------------------------------------------------------
data CURLSH = CURLSH Unique (MVar ((Ptr C.CURLSH),(FunPtr ()),(FunPtr ())))

instance Show CURLSH where
  showsPrec p (CURLSH u _) = showString "CURLSH#" . showsPrec p (hashUnique u)

instance Eq CURLSH where
  (CURLSH u1 _) == (CURLSH u2 _) = u1 == u2


-------------------------------------------------------------------------------
data CURLSHE = CURLSHE CURLSH String String CURLSHC
  deriving (Eq, Typeable)

instance Exception CURLSHE

instance Show CURLSHE where
  showsPrec p (CURLSHE curlsh func desc code) =
    showsPrec p code . showString " (" . showString func . showString " "
    . showsPrec p curlsh . showString ") " . showString desc


-------------------------------------------------------------------------------
data CURLSHC
  = CURLSHE_FAILED_INIT
  | CURLSHE_BAD_OPTION
  | CURLSHE_IN_USE
  | CURLSHE_INVALID
  | CURLSHE_NOMEM
  | CURLSHE_NOT_BUILT_IN
  | CURLSHE_UNKNOWN_ERROR
  deriving (Show, Eq)

toCURLSHC :: CInt -> CURLSHC
toCURLSHC x = case x of
  #{const CURLSHE_BAD_OPTION  } -> CURLSHE_BAD_OPTION
  #{const CURLSHE_IN_USE      } -> CURLSHE_IN_USE
  #{const CURLSHE_INVALID     } -> CURLSHE_INVALID
  #{const CURLSHE_NOMEM       } -> CURLSHE_NOMEM
  #{const CURLSHE_NOT_BUILT_IN} -> CURLSHE_NOT_BUILT_IN
  _                             -> CURLSHE_UNKNOWN_ERROR


-------------------------------------------------------------------------------
data CURLSHoption
  = CURLSHOPT_SHARE   CURLSHlockdata
  | CURLSHOPT_UNSHARE CURLSHlockdata


-------------------------------------------------------------------------------
#{ENUM CURLSHlockdata        \
, CURL_LOCK_DATA_COOKIE      \
, CURL_LOCK_DATA_DNS         \
, CURL_LOCK_DATA_SSL_SESSION }


-------------------------------------------------------------------------------
data CURL_version_info = CURL_version_info
  { curl_version_info_version         :: String
  , curl_version_info_version_num     :: Int
  , curl_version_info_host            :: String
  , curl_version_info_features        :: [CURLfeature]
  , curl_version_info_ssl_version     :: String
  , curl_version_info_ssl_version_num :: Int
  , curl_version_info_libz_version    :: String
  , curl_version_info_protocols       :: [String]
  , curl_version_info_ares            :: String
  , curl_version_info_ares_num        :: Int
  , curl_version_info_libidn          :: String
  , curl_version_info_iconv_ver_num   :: Int
  , curl_version_info_libssh_version  :: String
  } deriving (Show)


-------------------------------------------------------------------------------
data CURLinfo a where
  CURLINFO_EFFECTIVE_URL           :: CURLinfo String     -- Ptr CChar
  CURLINFO_RESPONSE_CODE           :: CURLinfo CLong      -- CLong
  CURLINFO_HTTP_CONNECTCODE        :: CURLinfo CLong      -- CLong
  CURLINFO_FILETIME                :: CURLinfo (Maybe UTCTime) -- CLong
  CURLINFO_TOTAL_TIME              :: CURLinfo Double     -- CDouble
  CURLINFO_NAMELOOKUP_TIME         :: CURLinfo Double     -- CDouble
  CURLINFO_CONNECT_TIME            :: CURLinfo Double     -- CDouble
  CURLINFO_APPCONNECT_TIME         :: CURLinfo Double     -- CDouble
  CURLINFO_PRETRANSFER_TIME        :: CURLinfo Double     -- CDouble
  CURLINFO_STARTTRANSFER_TIME      :: CURLinfo Double     -- CDouble
  CURLINFO_REDIRECT_TIME           :: CURLinfo Double     -- CDouble
  CURLINFO_REDIRECT_COUNT          :: CURLinfo CLong      -- CLong
  CURLINFO_REDIRECT_URL            :: CURLinfo String     -- Ptr CChar
  CURLINFO_SIZE_UPLOAD             :: CURLinfo Double     -- CDouble
  CURLINFO_SIZE_DOWNLOAD           :: CURLinfo Double     -- CDouble
  CURLINFO_SPEED_DOWNLOAD          :: CURLinfo Double     -- CDouble
  CURLINFO_SPEED_UPLOAD            :: CURLinfo Double     -- CDouble
  CURLINFO_HEADER_SIZE             :: CURLinfo CLong      -- CLong
  CURLINFO_REQUEST_SIZE            :: CURLinfo CLong      -- CLong
  CURLINFO_SSL_VERIFYRESULT        :: CURLinfo CLong      -- CLong
  CURLINFO_SSL_ENGINES             :: CURLinfo [String]   -- Ptr CURLslist
  CURLINFO_CONTENT_LENGTH_DOWNLOAD :: CURLinfo Double     -- CDouble
  CURLINFO_CONTENT_LENGTH_UPLOAD   :: CURLinfo Double     -- CDouble
  CURLINFO_CONTENT_TYPE            :: CURLinfo String     -- Ptr CChar
  -- CURLINFO_PRIVATE
  CURLINFO_HTTPAUTH_AVAIL          :: CURLinfo [CURLauth] -- CLong
  CURLINFO_PROXYAUTH_AVAIL         :: CURLinfo [CURLauth] -- CLong
  CURLINFO_OS_ERRNO                :: CURLinfo CLong      -- CLong
  CURLINFO_NUM_CONNECTS            :: CURLinfo CLong      -- CLong
  CURLINFO_PRIMARY_IP              :: CURLinfo String     -- Ptr CChar
  CURLINFO_PRIMARY_PORT            :: CURLinfo CLong      -- CLong
  CURLINFO_LOCAL_IP                :: CURLinfo String     -- Ptr CChar
  CURLINFO_LOCAL_PORT              :: CURLinfo CLong      -- CLong
  CURLINFO_COOKIELIST              :: CURLinfo [String]   -- Ptr CURLslist
  CURLINFO_LASTSOCKET              :: CURLinfo CLong      -- CLong
  CURLINFO_FTP_ENTRY_PATH          :: CURLinfo String     -- Ptr CChar
  CURLINFO_CERTINFO                :: CURLinfo [[String]] -- Ptr CURLcertinfo
  CURLINFO_CONDITION_UNMET         :: CURLinfo Bool       -- CLong
  CURLINFO_RTSP_SESSION_ID         :: CURLinfo String     -- Ptr CChar
  CURLINFO_RTSP_CLIENT_CSEQ        :: CURLinfo CLong      -- CLong
  CURLINFO_RTSP_SERVER_CSEQ        :: CURLinfo CLong      -- CLong
  CURLINFO_RTSP_CSEQ_RECV          :: CURLinfo CLong      -- CLong


-------------------------------------------------------------------------------
data CURLoption
  ---- BEHAVIOR OPTIONS -------------------------------------------------------
  = CURLOPT_VERBOSE                 Bool
  | CURLOPT_HEADER                  Bool
  | CURLOPT_NOPROGRESS              Bool
  | CURLOPT_NOSIGNAL                Bool
  | CURLOPT_WILDCARDMATCH           Bool

  ---- CALLBACK OPTIONS -------------------------------------------------------
  | CURLOPT_WRITEFUNCTION           (Maybe CURL_write_callback)
 -- CURLOPT_WRITEDATA
  | CURLOPT_READFUNCTION            (Maybe CURL_read_callback)
 -- CURLOPT_READDATA
 -- CURLOPT_IOCTLFUNCTION
 -- CURLOPT_IOCTLDATA
 -- CURLOPT_SEEKFUNCTION
 -- CURLOPT_SEEKDATA
 -- CURLOPT_SOCKOPTFUNCTION
 -- CURLOPT_SOCKOPTDATA
 -- CURLOPT_OPENSOCKETFUNCTION
 -- CURLOPT_OPENSOCKETDATA
 -- CURLOPT_CLOSESOCKETFUNCTION
 -- CURLOPT_CLOSESOCKETDATA
 -- CURLOPT_PROGRESSFUNCTION
 -- CURLOPT_PROGRESSDATA
  | CURLOPT_HEADERFUNCTION          (Maybe CURL_header_callback)
 -- CURLOPT_HEADERDATA
 -- CURLOPT_DEBUGFUNCTION
 -- CURLOPT_DEBUGDATA
 -- CURLOPT_SSL_CTX_FUNCTION
 -- CURLOPT_SSL_CTX_DATA
 -- CURLOPT_CONV_TO_NETWORK_FUNCTION
 -- CURLOPT_CONV_FROM_NETWORK_FUNCTION
 -- CURLOPT_CONV_FROM_UTF8_FUNCTION
 -- CURLOPT_INTERLEAVEFUNCTION
 -- CURLOPT_INTERLEAVEDATA
 -- CURLOPT_CHUNK_BGN_FUNCTION
 -- CURLOPT_CHUNK_END_FUNCTION
 -- CURLOPT_CHUNK_DATA
 -- CURLOPT_FNMATCH_FUNCTION
 -- CURLOPT_FNMATCH_DATA

  ---- ERROR OPTIONS ----------------------------------------------------------
 -- CURLOPT_ERRORBUFFER             ?
 -- CURLOPT_STDERR                  ?
  | CURLOPT_FAILONERROR             Bool

  ---- NETWORK OPTIONS --------------------------------------------------------
  | CURLOPT_URL                     String
  | CURLOPT_PROTOCOLS               [CURLproto]
  | CURLOPT_REDIR_PROTOCOLS         [CURLproto]
  | CURLOPT_PROXY                   String
  | CURLOPT_PROXYPORT               CLong
  | CURLOPT_PROXYTYPE               CURLproxy
  | CURLOPT_NOPROXY                 String -- or better [String]
  | CURLOPT_HTTPPROXYTUNNEL         Bool
  | CURLOPT_SOCKS5_GSSAPI_SERVICE   String
  | CURLOPT_SOCKS5_GSSAPI_NEC       Bool
  | CURLOPT_INTERFACE               String
  | CURLOPT_LOCALPORT               CLong
  | CURLOPT_LOCALPORTRANGE          CLong
  | CURLOPT_DNS_CACHE_TIMEOUT       CLong
 -- CURLOPT_DNS_USE_GLOBAL_CACHE    Bool
  | CURLOPT_BUFFERSIZE              CLong
  | CURLOPT_PORT                    CLong
  | CURLOPT_TCP_NODELAY             Bool
  | CURLOPT_ADDRESS_SCOPE           CLong
  | CURLOPT_TCP_KEEPALIVE           Bool
  | CURLOPT_TCP_KEEPIDLE            CLong
  | CURLOPT_TCP_KEEPINTVL           CLong

  ---- NAMES and PASSWORDS OPTIONS (Authentication) ---------------------------
  | CURLOPT_NETRC                   CURLnetrc
  | CURLOPT_NETRC_FILE              FilePath
  | CURLOPT_USERPWD                 String
  | CURLOPT_PROXYUSERPWD            String
  | CURLOPT_USERNAME                String
  | CURLOPT_PASSWORD                String
  | CURLOPT_PROXYUSERNAME           String
  | CURLOPT_PROXYPASSWORD           String
  | CURLOPT_HTTPAUTH                [CURLauth]
  | CURLOPT_TLSAUTH_TYPE            String -- should be [CURLtlsauth]
  | CURLOPT_TLSAUTH_USERNAME        String
  | CURLOPT_TLSAUTH_PASSWORD        String
  | CURLOPT_PROXYAUTH               [CURLauth]

  ---- HTTP OPTIONS -----------------------------------------------------------
  | CURLOPT_AUTOREFERER             Bool
  | CURLOPT_ACCEPT_ENCODING         String
  | CURLOPT_TRANSFER_ENCODING       Bool
  | CURLOPT_FOLLOWLOCATION          Bool
  | CURLOPT_UNRESTRICTED_AUTH       Bool
  | CURLOPT_MAXREDIRS               CLong
  | CURLOPT_POSTREDIR               [CURLredir]
  | CURLOPT_PUT                     Bool
  | CURLOPT_POST                    Bool
 -- CURLOPT_POSTFIELDS              String -- not copied
  | CURLOPT_POSTFIELDSIZE           CLong
  | CURLOPT_POSTFIELDSIZE_LARGE     Int64
  | CURLOPT_COPYPOSTFIELDS          String
 -- CURLOPT_HTTPPOST                [CURL_httppost]
  | CURLOPT_REFERER                 String
  | CURLOPT_USERAGENT               String
  | CURLOPT_HTTPHEADER              [String]
  | CURLOPT_HTTP200ALIASES          [String]
  | CURLOPT_COOKIE                  String -- or better [String]
  | CURLOPT_COOKIEFILE              FilePath
  | CURLOPT_COOKIEJAR               FilePath
  | CURLOPT_COOKIESESSION           Bool
  | CURLOPT_COOKIELIST              String
  | CURLOPT_HTTPGET                 Bool
  | CURLOPT_HTTP_VERSION            CURLhttpver
  | CURLOPT_IGNORE_CONTENT_LENGTH   Bool
  | CURLOPT_HTTP_CONTENT_DECODING   Bool
  | CURLOPT_HTTP_TRANSFER_DECODING  Bool

  ---- SMTP OPTIONS -----------------------------------------------------------
  | CURLOPT_MAIL_FROM               String
  | CURLOPT_MAIL_RCPT               [String]
  | CURLOPT_MAIL_AUTH               String

  ---- TFTP OPTIONS -----------------------------------------------------------
  | CURLOPT_TFTP_BLKSIZE            CLong

  ---- FTP OPTIONS ------------------------------------------------------------
  | CURLOPT_FTPPORT                 String
  | CURLOPT_QUOTE                   [String]
  | CURLOPT_POSTQUOTE               [String]
  | CURLOPT_PREQUOTE                [String]
  | CURLOPT_DIRLISTONLY             Bool
  | CURLOPT_APPEND                  Bool
  | CURLOPT_FTP_USE_EPRT            Bool
  | CURLOPT_FTP_USE_EPSV            Bool
  | CURLOPT_FTP_USE_PRET            Bool
  | CURLOPT_FTP_CREATE_MISSING_DIRS CURLftpcreate
  | CURLOPT_FTP_RESPONSE_TIMEOUT    CLong
  | CURLOPT_FTP_ALTERNATIVE_TO_USER String
  | CURLOPT_FTP_SKIP_PASV_IP        Bool
  | CURLOPT_FTPSSLAUTH              CURLftpauth
  | CURLOPT_FTP_SSL_CCC             CURLftpssl
  | CURLOPT_FTP_ACCOUNT             String
  | CURLOPT_FTP_FILEMETHOD          CURLftpmethod

  ---- RTSP OPTIONS -----------------------------------------------------------
  | CURLOPT_RTSP_REQUEST            CURLrtspreq
  | CURLOPT_RTSP_SESSION_ID         String
  | CURLOPT_RTSP_STREAM_URI         String
  | CURLOPT_RTSP_TRANSPORT          String
  | CURLOPT_RTSP_HEADER             [String]
  | CURLOPT_RTSP_CLIENT_CSEQ        CLong
  | CURLOPT_RTSP_SERVER_CSEQ        CLong

  ---- PROTOCOL OPTIONS -------------------------------------------------------
  | CURLOPT_TRANSFERTEXT            Bool
  | CURLOPT_PROXY_TRANSFER_MODE     Bool
  | CURLOPT_CRLF                    Bool
  | CURLOPT_RANGE                   String
  | CURLOPT_RESUME_FROM             CLong
  | CURLOPT_RESUME_FROM_LARGE       Int64
  | CURLOPT_CUSTOMREQUEST           String
  | CURLOPT_FILETIME                Bool
  | CURLOPT_NOBODY                  Bool
  | CURLOPT_INFILESIZE              CLong
  | CURLOPT_INFILESIZE_LARGE        Int64
  | CURLOPT_UPLOAD                  Bool
  | CURLOPT_MAXFILESIZE             CLong
  | CURLOPT_MAXFILESIZE_LARGE       Int64
  | CURLOPT_TIMECONDITION           CURLtimecond
  | CURLOPT_TIMEVALUE               UTCTime

  ---- CONNECTION OPTIONS -----------------------------------------------------
  | CURLOPT_TIMEOUT                 CLong
  | CURLOPT_TIMEOUT_MS              CLong
  | CURLOPT_LOW_SPEED_LIMIT         CLong
  | CURLOPT_LOW_SPEED_TIME          CLong
  | CURLOPT_MAX_SEND_SPEED_LARGE    Int64
  | CURLOPT_MAX_RECV_SPEED_LARGE    Int64
  | CURLOPT_MAXCONNECTS             CLong
 -- CURLOPT_CLOSEPOLICY             CURLclosepol
  | CURLOPT_FRESH_CONNECT           Bool
  | CURLOPT_FORBID_REUSE            Bool
  | CURLOPT_CONNECTTIMEOUT          CLong
  | CURLOPT_CONNECTTIMEOUT_MS       CLong
  | CURLOPT_IPRESOLVE               CURLipresolve
  | CURLOPT_CONNECT_ONLY            Bool
  | CURLOPT_USE_SSL                 CURLusessl
  | CURLOPT_RESOLVE                 [String]
  | CURLOPT_DNS_SERVERS             String -- or better [String]
  | CURLOPT_ACCEPTTIMEOUT_MS        CLong

  ---- SSL and SECURITY OPTIONS -----------------------------------------------
  | CURLOPT_SSLCERT                 FilePath
  | CURLOPT_SSLCERTTYPE             String
  | CURLOPT_SSLKEY                  FilePath
  | CURLOPT_SSLKEYTYPE              String
  | CURLOPT_KEYPASSWD               String
  | CURLOPT_SSLENGINE               String
  | CURLOPT_SSLENGINE_DEFAULT       Bool
  | CURLOPT_SSLVERSION              CURLsslver
  | CURLOPT_SSL_VERIFYPEER          Bool
  | CURLOPT_CAINFO                  FilePath
  | CURLOPT_ISSUERCERT              FilePath
  | CURLOPT_CAPATH                  FilePath
  | CURLOPT_CRLFILE                 FilePath
  | CURLOPT_SSL_VERIFYHOST          Bool
  | CURLOPT_CERTINFO                Bool
  | CURLOPT_RANDOM_FILE             FilePath
  | CURLOPT_EGDSOCKET               String
  | CURLOPT_SSL_CIPHER_LIST         String -- or better [String]
  | CURLOPT_SSL_SESSIONID_CACHE     Bool
  | CURLOPT_SSL_OPTIONS             [CURLsslopt]
  | CURLOPT_KRBLEVEL                String
  | CURLOPT_GSSAPI_DELEGATION       CURLgssapi

  ---- SSH OPTIONS ------------------------------------------------------------
  | CURLOPT_SSH_AUTH_TYPES          [CURLsshauth]
  | CURLOPT_SSH_HOST_PUBLIC_KEY_MD5 String
  | CURLOPT_SSH_PUBLIC_KEYFILE      FilePath
  | CURLOPT_SSH_PRIVATE_KEYFILE     FilePath
  | CURLOPT_SSH_KNOWNHOSTS          FilePath
 -- CURLOPT_SSH_KEYFUNCTION         (Maybe CURL_sshkey_callback)
 -- CURLOPT_SSH_KEYDATA             -- ?

  ---- OTHER OPTIONS ----------------------------------------------------------
 -- CURLOPT_PRIVATE                 -- ?
  | CURLOPT_SHARE                   (Maybe CURLSH)
  | CURLOPT_NEW_FILE_PERMS          CLong
  | CURLOPT_NEW_DIRECTORY_PERMS     CLong

  ---- TELNET OPTIONS ---------------------------------------------------------
  | CURLOPT_TELNETOPTIONS           [String]


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


type CURL_header_callback = ByteString -> IO CURL_header_response

data CURL_header_response
  = CURL_HEADERFUNC_OK
  | CURL_HEADERFUNC_FAIL
  deriving (Eq)



-------------------------------------------------------------------------------
data CURLC
  = CURLE_UNSUPPORTED_PROTOCOL
  | CURLE_FAILED_INIT
  | CURLE_URL_MALFORMAT
  | CURLE_NOT_BUILT_IN
  | CURLE_COULDNT_RESOLVE_PROXY
  | CURLE_COULDNT_RESOLVE_HOST
  | CURLE_COULDNT_CONNECT
  | CURLE_FTP_WEIRD_SERVER_REPLY
  | CURLE_REMOTE_ACCESS_DENIED
  | CURLE_FTP_ACCEPT_FAILED
  | CURLE_FTP_WEIRD_PASS_REPLY
  | CURLE_FTP_ACCEPT_TIMEOUT
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
  | CURLE_UNKNOWN_OPTION
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
  | CURLE_UNKNOWN_ERROR
  deriving (Show, Eq)

toCURLC :: CInt -> CURLC
toCURLC x = case x of
  #{const CURLE_UNSUPPORTED_PROTOCOL    } -> CURLE_UNSUPPORTED_PROTOCOL
  #{const CURLE_FAILED_INIT             } -> CURLE_FAILED_INIT
  #{const CURLE_URL_MALFORMAT           } -> CURLE_URL_MALFORMAT
  #{const CURLE_NOT_BUILT_IN            } -> CURLE_NOT_BUILT_IN
  #{const CURLE_COULDNT_RESOLVE_PROXY   } -> CURLE_COULDNT_RESOLVE_PROXY
  #{const CURLE_COULDNT_RESOLVE_HOST    } -> CURLE_COULDNT_RESOLVE_HOST
  #{const CURLE_COULDNT_CONNECT         } -> CURLE_COULDNT_CONNECT
  #{const CURLE_FTP_WEIRD_SERVER_REPLY  } -> CURLE_FTP_WEIRD_SERVER_REPLY
  #{const CURLE_REMOTE_ACCESS_DENIED    } -> CURLE_REMOTE_ACCESS_DENIED
  #{const CURLE_FTP_ACCEPT_FAILED       } -> CURLE_FTP_ACCEPT_FAILED
  #{const CURLE_FTP_WEIRD_PASS_REPLY    } -> CURLE_FTP_WEIRD_PASS_REPLY
  #{const CURLE_FTP_ACCEPT_TIMEOUT      } -> CURLE_FTP_ACCEPT_TIMEOUT
  #{const CURLE_FTP_WEIRD_PASV_REPLY    } -> CURLE_FTP_WEIRD_PASV_REPLY
  #{const CURLE_FTP_WEIRD_227_FORMAT    } -> CURLE_FTP_WEIRD_227_FORMAT
  #{const CURLE_FTP_CANT_GET_HOST       } -> CURLE_FTP_CANT_GET_HOST
  #{const CURLE_FTP_COULDNT_SET_TYPE    } -> CURLE_FTP_COULDNT_SET_TYPE
  #{const CURLE_PARTIAL_FILE            } -> CURLE_PARTIAL_FILE
  #{const CURLE_FTP_COULDNT_RETR_FILE   } -> CURLE_FTP_COULDNT_RETR_FILE
  #{const CURLE_QUOTE_ERROR             } -> CURLE_QUOTE_ERROR
  #{const CURLE_HTTP_RETURNED_ERROR     } -> CURLE_HTTP_RETURNED_ERROR
  #{const CURLE_WRITE_ERROR             } -> CURLE_WRITE_ERROR
  #{const CURLE_UPLOAD_FAILED           } -> CURLE_UPLOAD_FAILED
  #{const CURLE_READ_ERROR              } -> CURLE_READ_ERROR
  #{const CURLE_OUT_OF_MEMORY           } -> CURLE_OUT_OF_MEMORY
  #{const CURLE_OPERATION_TIMEDOUT      } -> CURLE_OPERATION_TIMEDOUT
  #{const CURLE_FTP_PORT_FAILED         } -> CURLE_FTP_PORT_FAILED
  #{const CURLE_FTP_COULDNT_USE_REST    } -> CURLE_FTP_COULDNT_USE_REST
  #{const CURLE_RANGE_ERROR             } -> CURLE_RANGE_ERROR
  #{const CURLE_HTTP_POST_ERROR         } -> CURLE_HTTP_POST_ERROR
  #{const CURLE_SSL_CONNECT_ERROR       } -> CURLE_SSL_CONNECT_ERROR
  #{const CURLE_BAD_DOWNLOAD_RESUME     } -> CURLE_BAD_DOWNLOAD_RESUME
  #{const CURLE_FILE_COULDNT_READ_FILE  } -> CURLE_FILE_COULDNT_READ_FILE
  #{const CURLE_LDAP_CANNOT_BIND        } -> CURLE_LDAP_CANNOT_BIND
  #{const CURLE_LDAP_SEARCH_FAILED      } -> CURLE_LDAP_SEARCH_FAILED
  #{const CURLE_FUNCTION_NOT_FOUND      } -> CURLE_FUNCTION_NOT_FOUND
  #{const CURLE_ABORTED_BY_CALLBACK     } -> CURLE_ABORTED_BY_CALLBACK
  #{const CURLE_BAD_FUNCTION_ARGUMENT   } -> CURLE_BAD_FUNCTION_ARGUMENT
  #{const CURLE_INTERFACE_FAILED        } -> CURLE_INTERFACE_FAILED
  #{const CURLE_TOO_MANY_REDIRECTS      } -> CURLE_TOO_MANY_REDIRECTS
  #{const CURLE_UNKNOWN_OPTION          } -> CURLE_UNKNOWN_OPTION
  #{const CURLE_TELNET_OPTION_SYNTAX    } -> CURLE_TELNET_OPTION_SYNTAX
  #{const CURLE_PEER_FAILED_VERIFICATION} -> CURLE_PEER_FAILED_VERIFICATION
  #{const CURLE_GOT_NOTHING             } -> CURLE_GOT_NOTHING
  #{const CURLE_SSL_ENGINE_NOTFOUND     } -> CURLE_SSL_ENGINE_NOTFOUND
  #{const CURLE_SSL_ENGINE_SETFAILED    } -> CURLE_SSL_ENGINE_SETFAILED
  #{const CURLE_SEND_ERROR              } -> CURLE_SEND_ERROR
  #{const CURLE_RECV_ERROR              } -> CURLE_RECV_ERROR
  #{const CURLE_SSL_CERTPROBLEM         } -> CURLE_SSL_CERTPROBLEM
  #{const CURLE_SSL_CIPHER              } -> CURLE_SSL_CIPHER
  #{const CURLE_SSL_CACERT              } -> CURLE_SSL_CACERT
  #{const CURLE_BAD_CONTENT_ENCODING    } -> CURLE_BAD_CONTENT_ENCODING
  #{const CURLE_LDAP_INVALID_URL        } -> CURLE_LDAP_INVALID_URL
  #{const CURLE_FILESIZE_EXCEEDED       } -> CURLE_FILESIZE_EXCEEDED
  #{const CURLE_USE_SSL_FAILED          } -> CURLE_USE_SSL_FAILED
  #{const CURLE_SEND_FAIL_REWIND        } -> CURLE_SEND_FAIL_REWIND
  #{const CURLE_SSL_ENGINE_INITFAILED   } -> CURLE_SSL_ENGINE_INITFAILED
  #{const CURLE_LOGIN_DENIED            } -> CURLE_LOGIN_DENIED
  #{const CURLE_TFTP_NOTFOUND           } -> CURLE_TFTP_NOTFOUND
  #{const CURLE_TFTP_PERM               } -> CURLE_TFTP_PERM
  #{const CURLE_REMOTE_DISK_FULL        } -> CURLE_REMOTE_DISK_FULL
  #{const CURLE_TFTP_ILLEGAL            } -> CURLE_TFTP_ILLEGAL
  #{const CURLE_TFTP_UNKNOWNID          } -> CURLE_TFTP_UNKNOWNID
  #{const CURLE_REMOTE_FILE_EXISTS      } -> CURLE_REMOTE_FILE_EXISTS
  #{const CURLE_TFTP_NOSUCHUSER         } -> CURLE_TFTP_NOSUCHUSER
  #{const CURLE_CONV_FAILED             } -> CURLE_CONV_FAILED
  #{const CURLE_CONV_REQD               } -> CURLE_CONV_REQD
  #{const CURLE_SSL_CACERT_BADFILE      } -> CURLE_SSL_CACERT_BADFILE
  #{const CURLE_REMOTE_FILE_NOT_FOUND   } -> CURLE_REMOTE_FILE_NOT_FOUND
  #{const CURLE_SSH                     } -> CURLE_SSH
  #{const CURLE_SSL_SHUTDOWN_FAILED     } -> CURLE_SSL_SHUTDOWN_FAILED
  #{const CURLE_AGAIN                   } -> CURLE_AGAIN
  #{const CURLE_SSL_CRL_BADFILE         } -> CURLE_SSL_CRL_BADFILE
  #{const CURLE_SSL_ISSUER_ERROR        } -> CURLE_SSL_ISSUER_ERROR
  #{const CURLE_FTP_PRET_FAILED         } -> CURLE_FTP_PRET_FAILED
  #{const CURLE_RTSP_CSEQ_ERROR         } -> CURLE_RTSP_CSEQ_ERROR
  #{const CURLE_RTSP_SESSION_ERROR      } -> CURLE_RTSP_SESSION_ERROR
  #{const CURLE_FTP_BAD_FILE_LIST       } -> CURLE_FTP_BAD_FILE_LIST
  #{const CURLE_CHUNK_FAILED            } -> CURLE_CHUNK_FAILED
  _                                       -> CURLE_UNKNOWN_ERROR


-------------------------------------------------------------------------------
data CURLCB where
  CURLCB :: CURLCBT a -> FunPtr () -> CURLCB

data CURLCBT a where
  FWRITE  :: CURLCBT CURL_write_callback
  FREAD   :: CURLCBT CURL_read_callback
  FHEADER :: CURLCBT CURL_header_callback

type CURLSL = (CInt, Ptr C.CURLslist)


-------------------------------------------------------------------------------
data CURL = CURL Unique (MVar (Ptr C.CURL)) (IORef [CURLCB]) (IORef [CURLSL])

instance Show CURL where
  showsPrec p (CURL u _ _ _) = showString "CURL#" . showsPrec p (hashUnique u)

instance Eq CURL where
  (CURL u1 _ _ _) == (CURL u2 _ _ _) = u1 == u2


-------------------------------------------------------------------------------
data CURLE = CURLE CURL String String CURLC
  deriving (Eq, Typeable)

instance Exception CURLE

instance Show CURLE where
  showsPrec p (CURLE curl func desc code) =
    showString "\n<curlhs> " . showString desc
    . showString "\n<curlhs> " . showsPrec p code . showString " ("
    . showString func . showString " <" . showsPrec p curl . showString ">)"


