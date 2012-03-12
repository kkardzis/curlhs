-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Base
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}

module Network.Curlhs.Base where

import Foreign.C.Types  (CChar, CInt, CUInt, CLong, CDouble)
import Foreign.C.Types  (CSize, CFile, CTime)
import Foreign.Ptr      (Ptr, FunPtr, castPtr)
import Foreign.Storable (Storable (..))

import Control.Applicative ((<$>), (<*>))


-------------------------------------------------------------------------------
#{let alignof type = "(%ld)", (long) offsetof (struct {char x; type y;}, y)}

#define CURL_NO_OLDIES
#include "curl/curl.h"

#define hsc_symbol(name, type) \
  printf("c" #name " :: C" #type "\n"); \
  printf("c" #name " =  C" #type " "); hsc_const(name);

#define hsc_cconst(name, type) \
  printf("c" #name " :: " #type "\n"); \
  printf("c" #name " =  "); hsc_const(name);


-- ----------------------------------------------------------------------------
-- #include "curlver.h"
-- 
-- ----------------------------------------------------------------------------
-- #include "curlbuild.h"
-- 
-- ----------------------------------------------------------------------------
-- #include "curlrules.h"



-------------------------------------------------------------------------------
-- from "curl.h"
-------------------------------------------------------------------------------
data CCURL


-- ----------------------------------------------------------------------------
-- #ifndef curl_socket_typedef
-- /* socket typedef */
-- #if defined(WIN32) && !defined(__LWIP_OPT_H__)
-- typedef SOCKET curl_socket_t;
-- #define CURL_SOCKET_BAD INVALID_SOCKET
-- #else
-- typedef int curl_socket_t;
-- #define CURL_SOCKET_BAD -1
-- #endif
-- #define curl_socket_typedef
-- #endif /* curl_socket_typedef */
-- 
-- ----------------------------------------------------------------------------
-- struct curl_httppost {
--   struct curl_httppost *next;
--   char *name;
--   long namelength;
--   char *contents;
--   long contentslength;
--   char *buffer;
--   long bufferlength;
--   char *contenttype;
--   struct curl_slist* contentheader;
--   struct curl_httppost *more;
--   long flags;
--   char *showfilename;
--   void *userp;
-- };
-- 
-- #define HTTPPOST_FILENAME (1<<0)
-- #define HTTPPOST_READFILE (1<<1)
-- #define HTTPPOST_PTRNAME (1<<2)
-- #define HTTPPOST_PTRCONTENTS (1<<3)
-- #define HTTPPOST_BUFFER (1<<4)
-- #define HTTPPOST_PTRBUFFER (1<<5)
-- #define HTTPPOST_CALLBACK (1<<6)
-- 
-- ----------------------------------------------------------------------------
-- typedef int (*curl_progress_callback)(void *clientp,
--                                       double dltotal,
--                                       double dlnow,
--                                       double ultotal,
--                                       double ulnow);



-------------------------------------------------------------------------------
#{cconst CURL_MAX_WRITE_SIZE , CSize}
#{cconst CURL_MAX_HTTP_HEADER, CSize}
#{cconst CURL_WRITEFUNC_PAUSE, CSize}

type CCURL_write_callback
  = Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall "wrapper"
  wrap_ccurl_write_callback
    :: CCURL_write_callback
    -> IO (FunPtr CCURL_write_callback)



-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLFILETYPE_FILE = 0,
--   CURLFILETYPE_DIRECTORY,
--   CURLFILETYPE_SYMLINK,
--   CURLFILETYPE_DEVICE_BLOCK,
--   CURLFILETYPE_DEVICE_CHAR,
--   CURLFILETYPE_NAMEDPIPE,
--   CURLFILETYPE_SOCKET,
--   CURLFILETYPE_DOOR,
--   CURLFILETYPE_UNKNOWN
-- } curlfiletype;
-- 
-- #define CURLFINFOFLAG_KNOWN_FILENAME    (1<<0)
-- #define CURLFINFOFLAG_KNOWN_FILETYPE    (1<<1)
-- #define CURLFINFOFLAG_KNOWN_TIME        (1<<2)
-- #define CURLFINFOFLAG_KNOWN_PERM        (1<<3)
-- #define CURLFINFOFLAG_KNOWN_UID         (1<<4)
-- #define CURLFINFOFLAG_KNOWN_GID         (1<<5)
-- #define CURLFINFOFLAG_KNOWN_SIZE        (1<<6)
-- #define CURLFINFOFLAG_KNOWN_HLINKCOUNT  (1<<7)
-- 
-- struct curl_fileinfo {
--   char *filename;
--   curlfiletype filetype;
--   time_t time;
--   unsigned int perm;
--   int uid;
--   int gid;
--   curl_off_t size;
--   long int hardlinks;
--   struct {
--     char *time;
--     char *perm;
--     char *user;
--     char *group;
--     char *target;
--   } strings;
--   unsigned int flags;
--   char * b_data;
--   size_t b_size;
--   size_t b_used;
-- };
-- 
-- ----------------------------------------------------------------------------
-- #define CURL_CHUNK_BGN_FUNC_OK      0
-- #define CURL_CHUNK_BGN_FUNC_FAIL    1
-- #define CURL_CHUNK_BGN_FUNC_SKIP    2
-- 
-- typedef long (*curl_chunk_bgn_callback)(const void *transfer_info,
--                                         void *ptr,
--                                         int remains);
-- 
-- ----------------------------------------------------------------------------
-- #define CURL_CHUNK_END_FUNC_OK      0
-- #define CURL_CHUNK_END_FUNC_FAIL    1
-- 
-- typedef long (*curl_chunk_end_callback)(void *ptr);
-- 
-- ----------------------------------------------------------------------------
-- #define CURL_FNMATCHFUNC_MATCH    0
-- #define CURL_FNMATCHFUNC_NOMATCH  1
-- #define CURL_FNMATCHFUNC_FAIL     2
-- 
-- typedef int (*curl_fnmatch_callback)(void *ptr,
--                                      const char *pattern,
--                                      const char *string);
-- 
-- ----------------------------------------------------------------------------
-- #define CURL_SEEKFUNC_OK       0
-- #define CURL_SEEKFUNC_FAIL     1
-- #define CURL_SEEKFUNC_CANTSEEK 2
-- 
-- typedef int (*curl_seek_callback)(void *instream,
--                                   curl_off_t offset,
--                                   int origin);



-------------------------------------------------------------------------------
#{cconst CURL_READFUNC_ABORT, CSize}
#{cconst CURL_READFUNC_PAUSE, CSize}

type CCURL_read_callback
  = Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall "wrapper"
  wrap_ccurl_read_callback
    :: CCURL_read_callback
    -> IO (FunPtr CCURL_read_callback)


 
-- ----------------------------------------------------------------------------
-- typedef enum  {
--   CURLSOCKTYPE_IPCXN,
--   CURLSOCKTYPE_LAST
-- } curlsocktype;
-- 
-- #define CURL_SOCKOPT_OK 0
-- #define CURL_SOCKOPT_ERROR 1
-- #define CURL_SOCKOPT_ALREADY_CONNECTED 2
-- 
-- typedef int (*curl_sockopt_callback)(void *clientp,
--                                      curl_socket_t curlfd,
--                                      curlsocktype purpose);
-- 
-- ----------------------------------------------------------------------------
-- struct curl_sockaddr {
--   int family;
--   int socktype;
--   int protocol;
--   unsigned int addrlen;
--   struct sockaddr addr;
-- };
-- 
-- typedef curl_socket_t (*curl_opensocket_callback)(void *clientp,
--                             curlsocktype purpose,
--                             struct curl_sockaddr *address);
-- 
-- typedef int (*curl_closesocket_callback)(void *clientp, curl_socket_t item);
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLIOE_OK,
--   CURLIOE_UNKNOWNCMD,
--   CURLIOE_FAILRESTART,
--   CURLIOE_LAST
-- } curlioerr;
-- 
-- typedef enum  {
--   CURLIOCMD_NOP,
--   CURLIOCMD_RESTARTREAD,
--   CURLIOCMD_LAST
-- } curliocmd;
-- 
-- typedef curlioerr (*curl_ioctl_callback)(CURL *handle,
--                                          int cmd,
--                                          void *clientp);
-- 
-- ----------------------------------------------------------------------------
-- typedef void *(*curl_malloc_callback)(size_t size);
-- typedef void (*curl_free_callback)(void *ptr);
-- typedef void *(*curl_realloc_callback)(void *ptr, size_t size);
-- typedef char *(*curl_strdup_callback)(const char *str);
-- typedef void *(*curl_calloc_callback)(size_t nmemb, size_t size);
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLINFO_TEXT = 0,
--   CURLINFO_HEADER_IN,
--   CURLINFO_HEADER_OUT,
--   CURLINFO_DATA_IN,
--   CURLINFO_DATA_OUT,
--   CURLINFO_SSL_DATA_IN,
--   CURLINFO_SSL_DATA_OUT,
--   CURLINFO_END
-- } curl_infotype;
-- 
-- typedef int (*curl_debug_callback)
--        (CURL *handle,
--         curl_infotype type,
--         char *data,
--         size_t size,
--         void *userptr);


-------------------------------------------------------------------------------
newtype CCURLcode = CCURLcode CInt deriving (Eq, Show)

#{symbol CURLE_OK                      , CURLcode}
#{symbol CURLE_UNSUPPORTED_PROTOCOL    , CURLcode}
#{symbol CURLE_FAILED_INIT             , CURLcode}
#{symbol CURLE_URL_MALFORMAT           , CURLcode}
#{symbol CURLE_NOT_BUILT_IN            , CURLcode} |7215:----|
#{symbol CURLE_COULDNT_RESOLVE_PROXY   , CURLcode}
#{symbol CURLE_COULDNT_RESOLVE_HOST    , CURLcode}
#{symbol CURLE_COULDNT_CONNECT         , CURLcode}
#{symbol CURLE_FTP_WEIRD_SERVER_REPLY  , CURLcode}
#{symbol CURLE_REMOTE_ACCESS_DENIED    , CURLcode}
#{symbol CURLE_FTP_ACCEPT_FAILED       , CURLcode} |7240:----|
#{symbol CURLE_FTP_WEIRD_PASS_REPLY    , CURLcode}
#{symbol CURLE_FTP_ACCEPT_TIMEOUT      , CURLcode} |7240:----|
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
#{symbol CURLE_UNKNOWN_TELNET_OPTION   , CURLcode} |----:7214|
#{symbol CURLE_UNKNOWN_OPTION          , CURLcode} |7215:----|
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
#{symbol CURLE_FTP_BAD_FILE_LIST       , CURLcode} |7210:----|
#{symbol CURLE_CHUNK_FAILED            , CURLcode} |7210:----|



-- ----------------------------------------------------------------------------
-- typedef CURLcode (*curl_conv_callback)(char *buffer, size_t length);
-- 
-- typedef CURLcode (*curl_ssl_ctx_callback)(CURL *curl,
--                                           void *ssl_ctx,
--                                           void *userptr);
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLPROXY_HTTP = 0,
--   CURLPROXY_HTTP_1_0 = 1,
--   CURLPROXY_SOCKS4 = 4,
--   CURLPROXY_SOCKS5 = 5,
--   CURLPROXY_SOCKS4A = 6,
--   CURLPROXY_SOCKS5_HOSTNAME = 7
-- } curl_proxytype;



-------------------------------------------------------------------------------
#{cconst CURLAUTH_NONE        , CLong}
#{cconst CURLAUTH_BASIC       , CLong}
#{cconst CURLAUTH_DIGEST      , CLong}
#{cconst CURLAUTH_GSSNEGOTIATE, CLong}
#{cconst CURLAUTH_NTLM        , CLong}
#{cconst CURLAUTH_DIGEST_IE   , CLong}
#{cconst CURLAUTH_NTLM_WB     , CLong} |7220:----|
#{cconst CURLAUTH_ONLY        , CLong} |7213:----|
#{cconst CURLAUTH_ANY         , CLong}
#{cconst CURLAUTH_ANYSAFE     , CLong}



 
-- ----------------------------------------------------------------------------
-- #define CURLSSH_AUTH_ANY       ~0
-- #define CURLSSH_AUTH_NONE      0
-- #define CURLSSH_AUTH_PUBLICKEY (1<<0)
-- #define CURLSSH_AUTH_PASSWORD  (1<<1)
-- #define CURLSSH_AUTH_HOST      (1<<2)
-- #define CURLSSH_AUTH_KEYBOARD  (1<<3)
-- #define CURLSSH_AUTH_DEFAULT CURLSSH_AUTH_ANY
-- 
-- ----------------------------------------------------------------------------
-- #define CURLGSSAPI_DELEGATION_NONE        0
-- #define CURLGSSAPI_DELEGATION_POLICY_FLAG (1<<0)
-- #define CURLGSSAPI_DELEGATION_FLAG        (1<<1)
-- 
-- ----------------------------------------------------------------------------
-- #define CURL_ERROR_SIZE 256
-- 
-- ----------------------------------------------------------------------------
-- struct curl_khkey {
--   const char *key;
--   size_t len;
--   enum type {
--     CURLKHTYPE_UNKNOWN,
--     CURLKHTYPE_RSA1,
--     CURLKHTYPE_RSA,
--     CURLKHTYPE_DSS
--   } keytype;
-- };
-- 
-- enum curl_khstat {
--   CURLKHSTAT_FINE_ADD_TO_FILE,
--   CURLKHSTAT_FINE,
--   CURLKHSTAT_REJECT,
--   CURLKHSTAT_DEFER,
--   CURLKHSTAT_LAST
-- };
-- 
-- enum curl_khmatch {
--   CURLKHMATCH_OK,
--   CURLKHMATCH_MISMATCH,
--   CURLKHMATCH_MISSING,
--   CURLKHMATCH_LAST
-- };
-- 
-- typedef int
--   (*curl_sshkeycallback) (CURL *easy,
--                           const struct curl_khkey *knownkey,
--                           const struct curl_khkey *foundkey,
--                           enum curl_khmatch,
--                           void *clientp);
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLUSESSL_NONE,
--   CURLUSESSL_TRY,
--   CURLUSESSL_CONTROL,
--   CURLUSESSL_ALL,
--   CURLUSESSL_LAST
-- } curl_usessl;
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLFTPSSL_CCC_NONE,
--   CURLFTPSSL_CCC_PASSIVE,
--   CURLFTPSSL_CCC_ACTIVE,
--   CURLFTPSSL_CCC_LAST
-- } curl_ftpccc;
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLFTPAUTH_DEFAULT,
--   CURLFTPAUTH_SSL,
--   CURLFTPAUTH_TLS,
--   CURLFTPAUTH_LAST
-- } curl_ftpauth;
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLFTP_CREATE_DIR_NONE,
--   CURLFTP_CREATE_DIR,
--   CURLFTP_CREATE_DIR_RETRY,
--   CURLFTP_CREATE_DIR_LAST
-- } curl_ftpcreatedir;
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLFTPMETHOD_DEFAULT,
--   CURLFTPMETHOD_MULTICWD,
--   CURLFTPMETHOD_NOCWD,
--   CURLFTPMETHOD_SINGLECWD,
--   CURLFTPMETHOD_LAST
-- } curl_ftpmethod;
-- 
-- ----------------------------------------------------------------------------
-- #define CURLPROTO_HTTP   (1<<0)
-- #define CURLPROTO_HTTPS  (1<<1)
-- #define CURLPROTO_FTP    (1<<2)
-- #define CURLPROTO_FTPS   (1<<3)
-- #define CURLPROTO_SCP    (1<<4)
-- #define CURLPROTO_SFTP   (1<<5)
-- #define CURLPROTO_TELNET (1<<6)
-- #define CURLPROTO_LDAP   (1<<7)
-- #define CURLPROTO_LDAPS  (1<<8)
-- #define CURLPROTO_DICT   (1<<9)
-- #define CURLPROTO_FILE   (1<<10)
-- #define CURLPROTO_TFTP   (1<<11)
-- #define CURLPROTO_IMAP   (1<<12)
-- #define CURLPROTO_IMAPS  (1<<13)
-- #define CURLPROTO_POP3   (1<<14)
-- #define CURLPROTO_POP3S  (1<<15)
-- #define CURLPROTO_SMTP   (1<<16)
-- #define CURLPROTO_SMTPS  (1<<17)
-- #define CURLPROTO_RTSP   (1<<18)
-- #define CURLPROTO_RTMP   (1<<19)
-- #define CURLPROTO_RTMPT  (1<<20)
-- #define CURLPROTO_RTMPE  (1<<21)
-- #define CURLPROTO_RTMPTE (1<<22)
-- #define CURLPROTO_RTMPS  (1<<23)
-- #define CURLPROTO_RTMPTS (1<<24)
-- #define CURLPROTO_GOPHER (1<<25)
-- #define CURLPROTO_ALL    (~0)



-------------------------------------------------------------------------------
newtype CCURLoption'Int32  = CCURLoption'Int32  CInt deriving (Eq, Show)
newtype CCURLoption'Int64  = CCURLoption'Int64  CInt deriving (Eq, Show)
newtype CCURLoption'String = CCURLoption'String CInt deriving (Eq, Show)
newtype CCURLoption'SList  = CCURLoption'SList  CInt deriving (Eq, Show)
newtype CCURLoption'HTTPP  = CCURLoption'HTTPP  CInt deriving (Eq, Show)
newtype CCURLoption'File   = CCURLoption'File   CInt deriving (Eq, Show)
newtype CCURLoption'Share  = CCURLoption'Share  CInt deriving (Eq, Show)
newtype CCURLoption'Ptr_a  = CCURLoption'Ptr_a  CInt deriving (Eq, Show)
newtype CCURLoption'FunPtr = CCURLoption'FunPtr CInt deriving (Eq, Show)
newtype CCURLoption'FWRITE = CCURLoption'FWRITE CInt deriving (Eq, Show)
newtype CCURLoption'FREAD  = CCURLoption'FREAD  CInt deriving (Eq, Show)

#define hsc_curlopt(name, type) \
  printf("c" #name " :: CCURLoption'" #type "\n"); \
  printf("c" #name " =  CCURLoption'" #type " "); hsc_const(name);

#{curlopt CURLOPT_FILE                       , File   }
#{curlopt CURLOPT_URL                        , String }
#{curlopt CURLOPT_PORT                       , Int32  }
#{curlopt CURLOPT_PROXY                      , String }
#{curlopt CURLOPT_USERPWD                    , String }
#{curlopt CURLOPT_PROXYUSERPWD               , String }
#{curlopt CURLOPT_RANGE                      , String }
#{curlopt CURLOPT_INFILE                     , File   }
#{curlopt CURLOPT_ERRORBUFFER                , String }
#{curlopt CURLOPT_WRITEFUNCTION              , FWRITE }
#{curlopt CURLOPT_READFUNCTION               , FREAD  }
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
#{curlopt CURLOPT_ENCODING                   , String } |----:7215|
#{curlopt CURLOPT_ACCEPT_ENCODING            , String } |7216:----|
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
#{curlopt CURLOPT_WILDCARDMATCH              , Int32  } |7210:----|
#{curlopt CURLOPT_CHUNK_BGN_FUNCTION         , FunPtr } |7210:----|
#{curlopt CURLOPT_CHUNK_END_FUNCTION         , FunPtr } |7210:----|
#{curlopt CURLOPT_FNMATCH_FUNCTION           , FunPtr } |7210:----|
#{curlopt CURLOPT_CHUNK_DATA                 , Ptr_a  } |7210:----|
#{curlopt CURLOPT_FNMATCH_DATA               , Ptr_a  } |7210:----|
#{curlopt CURLOPT_RESOLVE                    , SList  } |7213:----|
#{curlopt CURLOPT_TLSAUTH_USERNAME           , String } |7214:----|
#{curlopt CURLOPT_TLSAUTH_PASSWORD           , String } |7214:----|
#{curlopt CURLOPT_TLSAUTH_TYPE               , String } |7214:----|
#{curlopt CURLOPT_TRANSFER_ENCODING          , Int32  } |7216:----|
#{curlopt CURLOPT_CLOSESOCKETFUNCTION        , FunPtr } |7217:----|
#{curlopt CURLOPT_CLOSESOCKETDATA            , Ptr_a  } |7217:----|
#{curlopt CURLOPT_GSSAPI_DELEGATION          , Int32  } |7220:----|
#{curlopt CURLOPT_DNS_SERVERS                , String } |7240:----|
#{curlopt CURLOPT_ACCEPTTIMEOUT_MS           , Int32  } |7240:----|




-- ----------------------------------------------------------------------------
-- #define CURL_IPRESOLVE_WHATEVER 0
-- #define CURL_IPRESOLVE_V4       1
-- #define CURL_IPRESOLVE_V6       2
-- 
-- ----------------------------------------------------------------------------
-- #define CURLOPT_WRITEDATA CURLOPT_FILE
-- #define CURLOPT_READDATA  CURLOPT_INFILE
-- #define CURLOPT_HEADERDATA CURLOPT_WRITEHEADER
-- #define CURLOPT_RTSPHEADER CURLOPT_HTTPHEADER
-- 
-- ----------------------------------------------------------------------------
-- enum {
--   CURL_HTTP_VERSION_NONE,
--   CURL_HTTP_VERSION_1_0,
--   CURL_HTTP_VERSION_1_1,
--   CURL_HTTP_VERSION_LAST
-- };
-- 
-- ----------------------------------------------------------------------------
-- enum {
--     CURL_RTSPREQ_NONE,
--     CURL_RTSPREQ_OPTIONS,
--     CURL_RTSPREQ_DESCRIBE,
--     CURL_RTSPREQ_ANNOUNCE,
--     CURL_RTSPREQ_SETUP,
--     CURL_RTSPREQ_PLAY,
--     CURL_RTSPREQ_PAUSE,
--     CURL_RTSPREQ_TEARDOWN,
--     CURL_RTSPREQ_GET_PARAMETER,
--     CURL_RTSPREQ_SET_PARAMETER,
--     CURL_RTSPREQ_RECORD,
--     CURL_RTSPREQ_RECEIVE,
--     CURL_RTSPREQ_LAST
-- };
-- 
-- ----------------------------------------------------------------------------
-- enum CURL_NETRC_OPTION {
--   CURL_NETRC_IGNORED,
--   CURL_NETRC_OPTIONAL,
--   CURL_NETRC_REQUIRED,
--   CURL_NETRC_LAST
-- };
-- 
-- ----------------------------------------------------------------------------
-- enum {
--   CURL_SSLVERSION_DEFAULT,
--   CURL_SSLVERSION_TLSv1,
--   CURL_SSLVERSION_SSLv2,
--   CURL_SSLVERSION_SSLv3,
--   CURL_SSLVERSION_LAST
-- };
-- 
-- ----------------------------------------------------------------------------
-- enum CURL_TLSAUTH {
--   CURL_TLSAUTH_NONE,
--   CURL_TLSAUTH_SRP,
--   CURL_TLSAUTH_LAST
-- };
-- 
-- ----------------------------------------------------------------------------
-- #define CURL_REDIR_GET_ALL  0
-- #define CURL_REDIR_POST_301 1
-- #define CURL_REDIR_POST_302 2
-- #define CURL_REDIR_POST_ALL (CURL_REDIR_POST_301|CURL_REDIR_POST_302)
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURL_TIMECOND_NONE,
--   CURL_TIMECOND_IFMODSINCE,
--   CURL_TIMECOND_IFUNMODSINCE,
--   CURL_TIMECOND_LASTMOD,
--   CURL_TIMECOND_LAST
-- } curl_TimeCond;



-------------------------------------------------------------------------------
foreign import ccall "curl_strequal"   -- deprecated
  ccurl_strequal
    :: Ptr CChar
    -> Ptr CChar
    -> IO CInt


-------------------------------------------------------------------------------
foreign import ccall "curl_strnequal"  -- deprecated
  ccurl_strnequal
    :: Ptr CChar
    -> Ptr CChar
    -> CSize
    -> IO CInt


 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CFINIT(NOTHING),
--   CFINIT(COPYNAME),
--   CFINIT(PTRNAME),
--   CFINIT(NAMELENGTH),
--   CFINIT(COPYCONTENTS),
--   CFINIT(PTRCONTENTS),
--   CFINIT(CONTENTSLENGTH),
--   CFINIT(FILECONTENT),
--   CFINIT(ARRAY),
--   CFINIT(OBSOLETE),
--   CFINIT(FILE),
--   CFINIT(BUFFER),
--   CFINIT(BUFFERPTR),
--   CFINIT(BUFFERLENGTH),
--   CFINIT(CONTENTTYPE),
--   CFINIT(CONTENTHEADER),
--   CFINIT(FILENAME),
--   CFINIT(END),
--   CFINIT(OBSOLETE2),
--   CFINIT(STREAM),
--   CURLFORM_LASTENTRY
-- } CURLformoption;
-- 
-- ----------------------------------------------------------------------------
-- struct curl_forms {
--   CURLformoption option;
--   const char     *value;
-- };
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURL_FORMADD_OK,
--   CURL_FORMADD_MEMORY,
--   CURL_FORMADD_OPTION_TWICE,
--   CURL_FORMADD_NULL,
--   CURL_FORMADD_UNKNOWN_OPTION,
--   CURL_FORMADD_INCOMPLETE,
--   CURL_FORMADD_ILLEGAL_ARRAY,
--   CURL_FORMADD_DISABLED,
--   CURL_FORMADD_LAST
-- } CURLFORMcode;
-- 
-- ----------------------------------------------------------------------------
-- CURL_EXTERN CURLFORMcode curl_formadd(struct curl_httppost **httppost,
--                                       struct curl_httppost **last_post,
--                                       ...);
-- 
-- ----------------------------------------------------------------------------
-- typedef size_t (*curl_formget_callback)(void *arg, const char *buf,
--                                         size_t len);
-- 
-- ----------------------------------------------------------------------------
-- CURL_EXTERN int curl_formget(struct curl_httppost *form, void *arg,
--                              curl_formget_callback append);
-- 
-- ----------------------------------------------------------------------------
-- CURL_EXTERN void curl_formfree(struct curl_httppost *form);



-------------------------------------------------------------------------------
foreign import ccall "curl_getenv"  -- deprecated
  ccurl_getenv
    :: Ptr CChar
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_version"
  ccurl_version
    :: IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_escape"
  ccurl_easy_escape
    :: Ptr CCURL
    -> Ptr CChar
    -> CInt
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_escape"
  ccurl_escape
    :: Ptr CChar
    -> CInt
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_unescape"
  ccurl_easy_unescape
    :: Ptr CCURL
    -> Ptr CChar
    -> CInt
    -> Ptr CInt
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_unescape"
  ccurl_unescape
    :: Ptr CChar
    -> CInt
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_free"
  ccurl_free
    :: Ptr a
    -> IO ()


-------------------------------------------------------------------------------
foreign import ccall "curl_global_init"
  ccurl_global_init
    :: CLong
    -> IO CCURLcode


-------------------------------------------------------------------------------
-- foreign import ccall "curl_global_init_mem"
--   ccurl_global_init_mem
--     :: CLong
--     -> FunPtr CCURL_malloc_callback
--     -> FunPtr CCURL_free_callback
--     -> FunPtr CCURL_realloc_callback
--     -> FunPtr CCURL_strdup_callback
--     -> FunPtr CCURL_calloc_callback
--     -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_global_cleanup"
  ccurl_global_cleanup
    :: IO ()


-------------------------------------------------------------------------------
data CCURL_slist = CCURL_slist
  { ccurl_slist_data :: Ptr CChar
  , ccurl_slist_next :: Ptr CCURL_slist
  } deriving (Show)

instance Storable CCURL_slist where
  sizeOf _    = #{size    struct curl_slist}
  alignment _ = #{alignof struct curl_slist}
  poke _ _    = undefined
  peek ptr    = CCURL_slist
    <$> #{peek struct curl_slist, data} ptr
    <*> #{peek struct curl_slist, next} ptr

 
-------------------------------------------------------------------------------
foreign import ccall "curl_slist_append"
  ccurl_slist_append
    :: Ptr CCURL_slist
    -> Ptr CChar
    -> IO (Ptr CCURL_slist)


-------------------------------------------------------------------------------
foreign import ccall "curl_slist_free_all"
  ccurl_slist_free_all
    :: Ptr CCURL_slist
    -> IO ()

 
-------------------------------------------------------------------------------
foreign import ccall "curl_getdate"
  ccurl_getdate
    :: Ptr CChar
    -> Ptr CTime
    -> IO CTime


-------------------------------------------------------------------------------
data CCURL_certinfo = CCURL_certinfo
  { ccurl_certinfo_num_of_certs :: CInt
  , ccurl_certinfo_certinfo     :: Ptr (Ptr CCURL_slist)
  } deriving (Show)

instance Storable CCURL_certinfo where
  sizeOf _    = #{size    struct curl_certinfo}
  alignment _ = #{alignof struct curl_certinfo}
  poke _ _    = undefined
  peek ptr    = CCURL_certinfo
    <$> #{peek struct curl_certinfo, num_of_certs} ptr
    <*> #{peek struct curl_certinfo, certinfo    } ptr


-------------------------------------------------------------------------------
newtype CCURLinfo'CString = CCURLinfo'CString CInt deriving (Eq, Show)
newtype CCURLinfo'CDouble = CCURLinfo'CDouble CInt deriving (Eq, Show)
newtype CCURLinfo'CLong   = CCURLinfo'CLong   CInt deriving (Eq, Show)
newtype CCURLinfo'SList   = CCURLinfo'SList   CInt deriving (Eq, Show)
newtype CCURLinfo'CertI   = CCURLinfo'CertI   CInt deriving (Eq, Show)

#define hsc_curlinfo(name, type) \
  printf("c" #name " :: CCURLinfo'" #type "\n"); \
  printf("c" #name " =  CCURLinfo'" #type " "); hsc_const(name);

#{curlinfo CURLINFO_EFFECTIVE_URL          , CString}
#{curlinfo CURLINFO_RESPONSE_CODE          , CLong  }
#{curlinfo CURLINFO_TOTAL_TIME             , CDouble}
#{curlinfo CURLINFO_NAMELOOKUP_TIME        , CDouble}
#{curlinfo CURLINFO_CONNECT_TIME           , CDouble}
#{curlinfo CURLINFO_PRETRANSFER_TIME       , CDouble}
#{curlinfo CURLINFO_SIZE_UPLOAD            , CDouble}
#{curlinfo CURLINFO_SIZE_DOWNLOAD          , CDouble}
#{curlinfo CURLINFO_SPEED_DOWNLOAD         , CDouble}
#{curlinfo CURLINFO_SPEED_UPLOAD           , CDouble}
#{curlinfo CURLINFO_HEADER_SIZE            , CLong  }
#{curlinfo CURLINFO_REQUEST_SIZE           , CLong  }
#{curlinfo CURLINFO_SSL_VERIFYRESULT       , CLong  }
#{curlinfo CURLINFO_FILETIME               , CLong  }
#{curlinfo CURLINFO_CONTENT_LENGTH_DOWNLOAD, CDouble}
#{curlinfo CURLINFO_CONTENT_LENGTH_UPLOAD  , CDouble}
#{curlinfo CURLINFO_STARTTRANSFER_TIME     , CDouble}
#{curlinfo CURLINFO_CONTENT_TYPE           , CString}
#{curlinfo CURLINFO_REDIRECT_TIME          , CDouble}
#{curlinfo CURLINFO_REDIRECT_COUNT         , CLong  }
#{curlinfo CURLINFO_PRIVATE                , CString}
#{curlinfo CURLINFO_HTTP_CONNECTCODE       , CLong  }
#{curlinfo CURLINFO_HTTPAUTH_AVAIL         , CLong  }
#{curlinfo CURLINFO_PROXYAUTH_AVAIL        , CLong  }
#{curlinfo CURLINFO_OS_ERRNO               , CLong  }
#{curlinfo CURLINFO_NUM_CONNECTS           , CLong  }
#{curlinfo CURLINFO_SSL_ENGINES            , SList  }
#{curlinfo CURLINFO_COOKIELIST             , SList  }
#{curlinfo CURLINFO_LASTSOCKET             , CLong  }
#{curlinfo CURLINFO_FTP_ENTRY_PATH         , CString}
#{curlinfo CURLINFO_REDIRECT_URL           , CString}
#{curlinfo CURLINFO_PRIMARY_IP             , CString}
#{curlinfo CURLINFO_APPCONNECT_TIME        , CDouble}
#{curlinfo CURLINFO_CERTINFO               , CertI  }
#{curlinfo CURLINFO_CONDITION_UNMET        , CLong  }
#{curlinfo CURLINFO_RTSP_SESSION_ID        , CString}
#{curlinfo CURLINFO_RTSP_CLIENT_CSEQ       , CLong  }
#{curlinfo CURLINFO_RTSP_SERVER_CSEQ       , CLong  }
#{curlinfo CURLINFO_RTSP_CSEQ_RECV         , CLong  }
#{curlinfo CURLINFO_PRIMARY_PORT           , CLong  } |7210:----|
#{curlinfo CURLINFO_LOCAL_IP               , CString} |7210:----|
#{curlinfo CURLINFO_LOCAL_PORT             , CLong  } |7210:----|



-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLCLOSEPOLICY_NONE,
--   CURLCLOSEPOLICY_OLDEST,
--   CURLCLOSEPOLICY_LEAST_RECENTLY_USED,
--   CURLCLOSEPOLICY_LEAST_TRAFFIC,
--   CURLCLOSEPOLICY_SLOWEST,
--   CURLCLOSEPOLICY_CALLBACK,
--   CURLCLOSEPOLICY_LAST
-- } curl_closepolicy;
-- 
-- ----------------------------------------------------------------------------
-- #define CURL_GLOBAL_SSL (1<<0)
-- #define CURL_GLOBAL_WIN32 (1<<1)
-- #define CURL_GLOBAL_ALL (CURL_GLOBAL_SSL|CURL_GLOBAL_WIN32)
-- #define CURL_GLOBAL_NOTHING 0
-- #define CURL_GLOBAL_DEFAULT CURL_GLOBAL_ALL
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURL_LOCK_DATA_NONE = 0,
--   CURL_LOCK_DATA_SHARE,
--   CURL_LOCK_DATA_COOKIE,
--   CURL_LOCK_DATA_DNS,
--   CURL_LOCK_DATA_SSL_SESSION,
--   CURL_LOCK_DATA_CONNECT,
--   CURL_LOCK_DATA_LAST
-- } curl_lock_data;
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURL_LOCK_ACCESS_NONE = 0,
--   CURL_LOCK_ACCESS_SHARED = 1,
--   CURL_LOCK_ACCESS_SINGLE = 2,
--   CURL_LOCK_ACCESS_LAST
-- } curl_lock_access;
-- 
-- ----------------------------------------------------------------------------
-- typedef void (*curl_lock_function)(CURL *handle,
--                                    curl_lock_data data,
--                                    curl_lock_access locktype,
--                                    void *userptr);
-- 
-- ----------------------------------------------------------------------------
-- typedef void (*curl_unlock_function)(CURL *handle,
--                                      curl_lock_data data,
--                                      void *userptr);
-- 
-- ----------------------------------------------------------------------------
-- typedef void CURLSH;
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLSHE_OK,
--   CURLSHE_BAD_OPTION,
--   CURLSHE_IN_USE,
--   CURLSHE_INVALID,
--   CURLSHE_NOMEM,
--   CURLSHE_NOT_BUILT_IN,
--   CURLSHE_LAST
-- } CURLSHcode;
-- 
-- ----------------------------------------------------------------------------
-- typedef enum {
--   CURLSHOPT_NONE,
--   CURLSHOPT_SHARE,
--   CURLSHOPT_UNSHARE,
--   CURLSHOPT_LOCKFUNC,
--   CURLSHOPT_UNLOCKFUNC,
--   CURLSHOPT_USERDATA,
--   CURLSHOPT_LAST
-- } CURLSHoption;
-- 
-- ----------------------------------------------------------------------------
-- CURL_EXTERN CURLSH *curl_share_init(void);
-- CURL_EXTERN CURLSHcode curl_share_setopt(CURLSH *, CURLSHoption option, ...);
-- CURL_EXTERN CURLSHcode curl_share_cleanup(CURLSH *);



-------------------------------------------------------------------------------
newtype CCURLversion = CCURLversion CInt deriving (Eq, Show)

instance Storable CCURLversion where
  sizeOf _    = #{size    CURLversion}
  alignment _ = #{alignof CURLversion}
  poke _ _    = undefined
  peek ptr    = CCURLversion <$> peek (castPtr ptr)

#{symbol CURLVERSION_FIRST , CURLversion}
#{symbol CURLVERSION_SECOND, CURLversion}
#{symbol CURLVERSION_THIRD , CURLversion}
#{symbol CURLVERSION_FOURTH, CURLversion}
#{symbol CURLVERSION_NOW   , CURLversion}


-------------------------------------------------------------------------------
data CCURL_version_info_data = CCURL_version_info_data
  { ccurl_version_info_data_age             :: CCURLversion
  , ccurl_version_info_data_version         :: Ptr CChar
  , ccurl_version_info_data_version_num     :: CUInt
  , ccurl_version_info_data_host            :: Ptr CChar
  , ccurl_version_info_data_features        :: CInt
  , ccurl_version_info_data_ssl_version     :: Ptr CChar
  , ccurl_version_info_data_ssl_version_num :: CLong
  , ccurl_version_info_data_libz_version    :: Ptr CChar
  , ccurl_version_info_data_protocols       :: Ptr (Ptr CChar)
  , ccurl_version_info_data_ares            :: Ptr CChar
  , ccurl_version_info_data_ares_num        :: CInt
  , ccurl_version_info_data_libidn          :: Ptr CChar
  , ccurl_version_info_data_iconv_ver_num   :: CInt
  , ccurl_version_info_data_libssh_version  :: Ptr CChar
  } deriving (Show)

instance Storable CCURL_version_info_data where
  sizeOf _    = #{size    curl_version_info_data}
  alignment _ = #{alignof curl_version_info_data}
  poke _ _    = undefined
  peek ptr    = CCURL_version_info_data
    <$> #{peek curl_version_info_data, age            } ptr
    <*> #{peek curl_version_info_data, version        } ptr
    <*> #{peek curl_version_info_data, version_num    } ptr
    <*> #{peek curl_version_info_data, host           } ptr
    <*> #{peek curl_version_info_data, features       } ptr
    <*> #{peek curl_version_info_data, ssl_version    } ptr
    <*> #{peek curl_version_info_data, ssl_version_num} ptr
    <*> #{peek curl_version_info_data, libz_version   } ptr
    <*> #{peek curl_version_info_data, protocols      } ptr
    <*> #{peek curl_version_info_data, ares           } ptr
    <*> #{peek curl_version_info_data, ares_num       } ptr
    <*> #{peek curl_version_info_data, libidn         } ptr
    <*> #{peek curl_version_info_data, iconv_ver_num  } ptr
    <*> #{peek curl_version_info_data, libssh_version } ptr


-------------------------------------------------------------------------------
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
#{cconst CURL_VERSION_TLSAUTH_SRP , CInt} |7214:----|
#{cconst CURL_VERSION_NTLM_WB     , CInt} |7220:----|

 
-------------------------------------------------------------------------------
foreign import ccall "curl_version_info"
  ccurl_version_info
    :: CCURLversion
    -> IO (Ptr CCURL_version_info_data)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_strerror"
  ccurl_easy_strerror
    :: CCURLcode
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
-- foreign import ccall "curl_share_strerror"
--   ccurl_share_strerror
--     :: CCURLSHcode
--     -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_pause"
  ccurl_easy_pause
    :: Ptr CCURL
    -> CInt
    -> IO CCURLcode

 
-- ----------------------------------------------------------------------------
-- #define CURLPAUSE_RECV      (1<<0)
-- #define CURLPAUSE_RECV_CONT (0)
-- 
-- #define CURLPAUSE_SEND      (1<<2)
-- #define CURLPAUSE_SEND_CONT (0)
-- 
-- #define CURLPAUSE_ALL       (CURLPAUSE_RECV|CURLPAUSE_SEND)
-- #define CURLPAUSE_CONT      (CURLPAUSE_RECV_CONT|CURLPAUSE_SEND_CONT)



-------------------------------------------------------------------------------
-- from "easy.h"
-------------------------------------------------------------------------------
foreign import ccall "curl_easy_init"
  ccurl_easy_init
    :: IO (Ptr CCURL)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'Int32
    :: Ptr CCURL
    -> CCURLoption'Int32
    -> CLong
    -> IO CCURLcode

-- foreign import ccall "curl_easy_setopt"
--   ccurl_easy_setopt'Int64
--     :: Ptr CCURL
--     -> CCURLoption'Int64
--     -> CCURL_off_t
--     -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'String
    :: Ptr CCURL
    -> CCURLoption'String
    -> Ptr CChar
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'SList
    :: Ptr CCURL
    -> CCURLoption'SList
    -> Ptr CCURL_slist
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'File
    :: Ptr CCURL
    -> CCURLoption'File
    -> Ptr CFile
    -> IO CCURLcode

-- foreign import ccall "curl_easy_setopt"
--   ccurl_easy_setopt'Ptr_a
--     :: Ptr CCURL
--     -> CCURLoption'Ptr_a
--     -> Ptr a
--     -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'FWRITE
    :: Ptr CCURL
    -> CCURLoption'FWRITE
    -> FunPtr CCURL_write_callback
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'FREAD
    :: Ptr CCURL
    -> CCURLoption'FREAD
    -> FunPtr CCURL_read_callback
    -> IO CCURLcode

-- foreign import ccall "curl_easy_setopt"
--   ccurl_easy_setopt'FunPtr
--     :: Ptr CCURL
--     -> CCURLoption'FunPtr
--     -> FunPtr a
--     -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_perform"
  ccurl_easy_perform
    :: Ptr CCURL
    -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_cleanup"
  ccurl_easy_cleanup
    :: Ptr CCURL
    -> IO ()


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'CString
    :: Ptr CCURL
    -> CCURLinfo'CString
    -> Ptr (Ptr CChar)
    -> IO CCURLcode

foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'CDouble
    :: Ptr CCURL
    -> CCURLinfo'CDouble
    -> Ptr CDouble
    -> IO CCURLcode

foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'CLong
    :: Ptr CCURL
    -> CCURLinfo'CLong
    -> Ptr CLong
    -> IO CCURLcode

foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'SList
    :: Ptr CCURL
    -> CCURLinfo'SList
    -> Ptr (Ptr CCURL_slist)
    -> IO CCURLcode

foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'CertI
    :: Ptr CCURL
    -> CCURLinfo'CertI
    -> Ptr (Ptr CCURL_certinfo)
    -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_duphandle"
  ccurl_easy_duphandle
    :: Ptr CCURL
    -> IO (Ptr CCURL)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_reset"
  ccurl_easy_reset
    :: Ptr CCURL
    -> IO ()


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_recv"
  ccurl_easy_recv
    :: Ptr CCURL
    -> Ptr a
    -> CSize
    -> Ptr CSize
    -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_send"
  ccurl_easy_send
    :: Ptr CCURL
    -> Ptr a
    -> CSize
    -> Ptr CSize
    -> IO CCURLcode

 
-------------------------------------------------------------------------------
-- from "multi.h"
-------------------------------------------------------------------------------

