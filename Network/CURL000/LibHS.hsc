-------------------------------------------------------------------------------
-- |
-- Module      :  Network.CURL000.LibHS
-- Copyright   :  Copyright © 2012-2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}

module Network.CURL000.LibHS
  ( curl_easy_cleanup
 -- curl_easy_duphandle
 -- curl_easy_escape
  , curl_easy_getinfo
  , curl_easy_init
 -- curl_easy_pause
  , curl_easy_perform
  , curl_easy_recv
  , curl_easy_reset
  , curl_easy_send
  , curl_easy_setopt
  , curl_easy_strerror
 -- curl_easy_unescape
 -- curl_escape
 -- curl_formadd
 -- curl_formfree
 -- curl_formget
 -- curl_free
 -- curl_getdate
 -- curl_getenv
  , curl_global_cleanup
  , curl_global_init
 -- curl_global_init_mem
 -- curl_maprintf
 -- curl_mfprintf
 -- curl_mprintf
 -- curl_msnprintf
 -- curl_msprintf
 -- curl_multi_add_handle
 -- curl_multi_assign
 -- curl_multi_cleanup
 -- curl_multi_fdset
 -- curl_multi_info_read
 -- curl_multi_init
 -- curl_multi_perform
 -- curl_multi_remove_handle
 -- curl_multi_setopt
 -- curl_multi_socket
 -- curl_multi_socket_action
 -- curl_multi_socket_all
  , curl_multi_strerror
 -- curl_multi_timeout
 -- curl_multi_wait
 -- curl_mvaprintf
 -- curl_mvfprintf
 -- curl_mvprintf
 -- curl_mvsnprintf
 -- curl_mvsprintf
  , curl_share_cleanup
  , curl_share_init
  , curl_share_setopt
  , curl_share_strerror
 -- curl_slist_append
 -- curl_slist_free_all
 -- curl_strequal
 -- curl_strnequal
 -- curl_unescape
  , curl_version
  , curl_version_info

  , CURL
  , CURLSH
  , withCURL
  , withCURLSH

  ) where

import qualified Network.CURL000.LibCC as C

import Network.CURL000.Types

import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString        (ByteString, packCStringLen)
import Data.IORef             (IORef, newIORef, atomicModifyIORef)
import Data.Time.Clock        (UTCTime)
import Data.Time.Clock.POSIX  (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.List              (partition)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent  (MVar, newMVar, takeMVar, tryPutMVar, modifyMVar)
import Control.Exception   (throwIO, bracket, onException)
import Control.Monad       (when, forM_, foldM)

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#include "LibC0.c"


-------------------------------------------------------------------------------
-- | Global libcurl initialisation
--   (<http://curl.haxx.se/libcurl/c/curl_global_init.html>).
-------------------------------------------------------------------------------
curl_global_init :: IO ()
curl_global_init = withCURLE $ C.curl_global_init #{const CURL_GLOBAL_ALL}


-------------------------------------------------------------------------------
-- | Global libcurl cleanup
--   (<http://curl.haxx.se/libcurl/c/curl_global_cleanup.html>).
-------------------------------------------------------------------------------
curl_global_cleanup :: IO ()
curl_global_cleanup = C.curl_global_cleanup


-------------------------------------------------------------------------------
-- | Returns the libcurl version string
--   (<http://curl.haxx.se/libcurl/c/curl_version.html>).
-------------------------------------------------------------------------------
curl_version :: IO String
curl_version = C.curl_version >>= peekCAString


-------------------------------------------------------------------------------
-- | Returns run-time libcurl version info
--   (<http://curl.haxx.se/libcurl/c/curl_version_info.html>).
-------------------------------------------------------------------------------
curl_version_info :: IO CURL_version_info
curl_version_info =
  C.curl_version_info #{const CURLVERSION_NOW} >>= \ptr -> CURL_version_info
    <$> (#{peek curl_version_info_data, version        } ptr >>= peekCAString)
    <*> (#{peek curl_version_info_data, version_num    } ptr >>= peekCUInt)
    <*> (#{peek curl_version_info_data, host           } ptr >>= peekCAString)
    <*> (#{peek curl_version_info_data, features       } ptr >>= peekCFeatures)
    <*> (#{peek curl_version_info_data, ssl_version    } ptr >>= peekCString0)
    <*> (#{peek curl_version_info_data, ssl_version_num} ptr >>= peekCLong)
    <*> (#{peek curl_version_info_data, libz_version   } ptr >>= peekCString0)
    <*> (#{peek curl_version_info_data, protocols      } ptr >>= peekCStringL)
    <*> (#{peek curl_version_info_data, ares           } ptr >>= peekCString0)
    <*> (#{peek curl_version_info_data, ares_num       } ptr >>= peekCInt)
    <*> (#{peek curl_version_info_data, libidn         } ptr >>= peekCString0)
    <*> (#{peek curl_version_info_data, iconv_ver_num  } ptr >>= peekCInt)
    <*> (#{peek curl_version_info_data, libssh_version } ptr >>= peekCString0)

peekCString0 :: Ptr CChar -> IO String
peekCString0 ptr = if (ptr==nullPtr) then return [] else peekCAString ptr

peekCStringL :: Ptr (Ptr CChar) -> IO [String]
peekCStringL ptr = peekArray0 nullPtr ptr >>= mapM peekCAString

peekCFeatures :: CInt -> IO [CURLfeature]
peekCFeatures = return . fromCIntMask

peekCUInt :: CUInt -> IO Int
peekCUInt = return . fromIntegral

peekCInt :: CInt -> IO Int
peekCInt = return . fromIntegral

peekCLong :: CLong -> IO Int
peekCLong = return . fromIntegral


-------------------------------------------------------------------------------
-- | Returns a string describing error code
--   (<http://curl.haxx.se/libcurl/c/curl_easy_strerror.html>).
-------------------------------------------------------------------------------
curl_easy_strerror :: CURLcode -> IO String
curl_easy_strerror code = C.curl_easy_strerror (toCInt code) >>= peekCAString

withCURLE :: IO CInt -> IO ()
withCURLE io = io >>= \x -> when (x/=0) (throwIO (fromCInt x :: CURLcode))


-------------------------------------------------------------------------------
-- | Returns a string describing error code
--   (<http://curl.haxx.se/libcurl/c/curl_multi_strerror.html>).
-------------------------------------------------------------------------------
curl_multi_strerror :: CURLMcode -> IO String
curl_multi_strerror code = C.curl_multi_strerror (toCInt code) >>= peekCAString

-- withCURLME :: IO CInt -> IO ()
-- withCURLME io = io >>= \x -> when (x/=0) (throwIO (fromCInt x :: CURLMcode))


-------------------------------------------------------------------------------
-- | Returns a string describing error code
--   (<http://curl.haxx.se/libcurl/c/curl_share_strerror.html>).
-------------------------------------------------------------------------------
curl_share_strerror :: CURLSHcode -> IO String
curl_share_strerror code = C.curl_share_strerror (toCInt code) >>= peekCAString

withCURLSHE :: IO CInt -> IO ()
withCURLSHE io = io >>= \x -> when (x/=0) (throwIO (fromCInt x :: CURLSHcode))


-------------------------------------------------------------------------------
-- | Start a libcurl easy session
--   (<http://curl.haxx.se/libcurl/c/curl_easy_init.html>).
-------------------------------------------------------------------------------
curl_easy_init :: IO CURL
curl_easy_init = CURL
  <$> (C.curl_easy_init >>= checkNULL)
  <*> (newIORef [])
  <*> (newIORef [])

checkNULL :: Ptr a -> IO (Ptr a)
checkNULL x = if (x==nullPtr) then throwIO CURLE_FAILED_INIT else return x

withCURL :: (CURL -> IO a) -> IO a
withCURL = bracket curl_easy_init curl_easy_cleanup

data CURL = CURL (Ptr C.CURL) (IORef [CURLCB]) (IORef [CURLSL])

freeCURL :: CURL -> IO ()
freeCURL (CURL _ cbref slref) = do
  fcbs <- atomicModifyIORef cbref (\cbs -> ([], cbs))
  fsls <- atomicModifyIORef slref (\sls -> ([], sls))
  mapM_ (\(CURLCB _ fp) -> freeHaskellFunPtr fp) fcbs
  mapM_ (C.curl_slist_free_all . snd) fsls


-------------------------------------------------------------------------------
-- | Reset all options of a libcurl session handle
--   (<http://curl.haxx.se/libcurl/c/curl_easy_reset.html>).
-------------------------------------------------------------------------------
curl_easy_reset :: CURL -> IO ()
curl_easy_reset curl@(CURL ccurl _ _) =
  C.curl_easy_reset ccurl >> freeCURL curl


-------------------------------------------------------------------------------
-- | End a libcurl easy session
--   (<http://curl.haxx.se/libcurl/c/curl_easy_cleanup.html>).
-------------------------------------------------------------------------------
curl_easy_cleanup :: CURL -> IO ()
curl_easy_cleanup curl@(CURL ccurl _ _) =
  C.curl_easy_cleanup ccurl >> freeCURL curl


-------------------------------------------------------------------------------
-- | Perform a file transfer
--   (<http://curl.haxx.se/libcurl/c/curl_easy_perform.html>).
-------------------------------------------------------------------------------
curl_easy_perform :: CURL -> IO ()
curl_easy_perform (CURL ccurl _ _) = withCURLE $ C.curl_easy_perform ccurl


-------------------------------------------------------------------------------
-- | Receives raw data on an @easy@ connection
--   (<http://curl.haxx.se/libcurl/c/curl_easy_recv.html>).
-------------------------------------------------------------------------------
curl_easy_recv :: CURL -> Int -> IO ByteString
curl_easy_recv (CURL ccurl _ _) len =
  alloca $ \nptr -> allocaBytes len $ \buff -> do
    withCURLE $ C.curl_easy_recv ccurl buff (fromIntegral len) nptr
    n <- fmap fromIntegral (peek nptr)
    packCStringLen (castPtr buff, n)


-------------------------------------------------------------------------------
-- | Sends raw data over an @easy@ connection
--   (<http://curl.haxx.se/libcurl/c/curl_easy_send.html>).
-------------------------------------------------------------------------------
curl_easy_send :: CURL -> ByteString -> IO Int
curl_easy_send (CURL ccurl _ _) bs =
  alloca $ \nptr -> do
    withCURLE $ unsafeUseAsCStringLen bs $ \(cs, cl) ->
      C.curl_easy_send ccurl (castPtr cs) (fromIntegral cl) nptr
    fmap fromIntegral (peek nptr)


-------------------------------------------------------------------------------
-- | Extract information from a curl handle
--   (<http://curl.haxx.se/libcurl/c/curl_easy_getinfo.html>).
-------------------------------------------------------------------------------
#define hsc_getopt(opt, foo) printf(#opt " -> getopt %d " #foo, opt)
curl_easy_getinfo :: CURL -> CURLinfo a -> IO a
curl_easy_getinfo (CURL ccurl _ _) opt =
  let getopt :: Storable x => CInt -> (Ptr x -> IO a) -> IO a
      getopt x fpeek = alloca $ \ptr -> getptr x (castPtr ptr) >> fpeek ptr
      getptr x ptr = withCURLE $ C.curl_easy_getinfo ccurl x ptr
  in case opt of
    #{getopt CURLINFO_EFFECTIVE_URL          , getString}
    #{getopt CURLINFO_RESPONSE_CODE          , getCLong }
    #{getopt CURLINFO_HTTP_CONNECTCODE       , getCLong }
    #{getopt CURLINFO_FILETIME               , getTime  }
    #{getopt CURLINFO_TOTAL_TIME             , getDouble}
    #{getopt CURLINFO_NAMELOOKUP_TIME        , getDouble}
    #{getopt CURLINFO_CONNECT_TIME           , getDouble}
    #{getopt CURLINFO_APPCONNECT_TIME        , getDouble}
    #{getopt CURLINFO_PRETRANSFER_TIME       , getDouble}
    #{getopt CURLINFO_STARTTRANSFER_TIME     , getDouble}
    #{getopt CURLINFO_REDIRECT_TIME          , getDouble}
    #{getopt CURLINFO_REDIRECT_COUNT         , getCLong }
    #{getopt CURLINFO_REDIRECT_URL           , getString}
    #{getopt CURLINFO_SIZE_UPLOAD            , getDouble}
    #{getopt CURLINFO_SIZE_DOWNLOAD          , getDouble}
    #{getopt CURLINFO_SPEED_DOWNLOAD         , getDouble}
    #{getopt CURLINFO_SPEED_UPLOAD           , getDouble}
    #{getopt CURLINFO_HEADER_SIZE            , getCLong }
    #{getopt CURLINFO_REQUEST_SIZE           , getCLong }
    #{getopt CURLINFO_SSL_VERIFYRESULT       , getCLong }
    #{getopt CURLINFO_SSL_ENGINES            , getSList }
    #{getopt CURLINFO_CONTENT_LENGTH_DOWNLOAD, getDouble}
    #{getopt CURLINFO_CONTENT_LENGTH_UPLOAD  , getDouble}
    #{getopt CURLINFO_CONTENT_TYPE           , getString}
    #{getopt CURLINFO_HTTPAUTH_AVAIL         , getAuth  }
    #{getopt CURLINFO_PROXYAUTH_AVAIL        , getAuth  }
    #{getopt CURLINFO_OS_ERRNO               , getCLong }
    #{getopt CURLINFO_NUM_CONNECTS           , getCLong }
    #{getopt CURLINFO_PRIMARY_IP             , getString}
    #{getopt CURLINFO_PRIMARY_PORT           , getCLong }
    #{getopt CURLINFO_LOCAL_IP               , getString}
    #{getopt CURLINFO_LOCAL_PORT             , getCLong }
    #{getopt CURLINFO_COOKIELIST             , getSList }
    #{getopt CURLINFO_LASTSOCKET             , getCLong }
    #{getopt CURLINFO_FTP_ENTRY_PATH         , getString}
    #{getopt CURLINFO_CERTINFO               , getCerts }
    #{getopt CURLINFO_CONDITION_UNMET        , getBool  }
    #{getopt CURLINFO_RTSP_SESSION_ID        , getString}
    #{getopt CURLINFO_RTSP_CLIENT_CSEQ       , getCLong }
    #{getopt CURLINFO_RTSP_SERVER_CSEQ       , getCLong }
    #{getopt CURLINFO_RTSP_CSEQ_RECV         , getCLong }

getString :: Ptr (Ptr CChar) -> IO String
getString ptr = peek ptr >>= peekCString0

getDouble :: Ptr CDouble -> IO Double
getDouble = fmap realToFrac . peek

getCLong :: Ptr CLong -> IO CLong
getCLong = peek

getBool :: Ptr CLong -> IO Bool
getBool = fmap toBool . peek

getAuth :: Ptr CLong -> IO [CURLauth]
getAuth = fmap fromCLongMask . peek

getTime :: Ptr CLong -> IO (Maybe UTCTime)
getTime =
  let mconv f x = if (x==(-1) || x==0) then Nothing else Just (f x)
  in  fmap (mconv (posixSecondsToUTCTime . realToFrac)) . peek

getCerts :: Ptr (Ptr C.CURLcerts) -> IO [[String]]
getCerts ptr =
  peek ptr >>= \certs -> if (certs==nullPtr) then return [] else
    peek certs >>= \(C.CURLcerts len tab) ->
      peekArray (fromIntegral len) tab >>= mapM peekCURLslist

getSList :: Ptr (Ptr C.CURLslist) -> IO [String]
getSList ptr =
  peek ptr >>= \slist -> do
    strings <- peekCURLslist slist
    C.curl_slist_free_all slist
    return strings

peekCURLslist :: Ptr C.CURLslist -> IO [String]
peekCURLslist ptr =
  if (ptr==nullPtr) then return [] else
    peek ptr >>= \(C.CURLslist cs csl) -> do
      slist_head <- peekCAString cs
      slist_tail <- peekCURLslist csl
      return (slist_head : slist_tail)


-------------------------------------------------------------------------------
-- | Set options for a curl easy handle
--   (<http://curl.haxx.se/libcurl/c/curl_easy_setopt.html>).
-------------------------------------------------------------------------------
#define hsc_setopt(opt, foo) printf(#opt " x -> " #foo " %d x", opt)
curl_easy_setopt :: CURL -> [CURLoption] -> IO ()
curl_easy_setopt curl@(CURL ccurl _ _) opts = forM_ opts $ \opt -> case opt of

  ---- CALLBACK OPTIONS -------------------------------------------------------
  #{setopt CURLOPT_WRITEFUNCTION          , curlcb FWRITE}
  #{setopt CURLOPT_READFUNCTION           , curlcb FREAD }

  ---- BEHAVIOR OPTIONS -------------------------------------------------------
  #{setopt CURLOPT_VERBOSE                , bool     }
  #{setopt CURLOPT_HEADER                 , bool     }
  #{setopt CURLOPT_NOPROGRESS             , bool     }
  #{setopt CURLOPT_NOSIGNAL               , bool     }
  #{setopt CURLOPT_WILDCARDMATCH          , bool     }

  ---- ERROR OPTIONS ----------------------------------------------------------
  -- CURLOPT_ERRORBUFFER
  -- CURLOPT_STDERR
  #{setopt CURLOPT_FAILONERROR            , bool     }

  ---- NETWORK OPTIONS --------------------------------------------------------
  #{setopt CURLOPT_URL                    , string   }
  #{setopt CURLOPT_PROTOCOLS              , enum     }
  #{setopt CURLOPT_REDIR_PROTOCOLS        , enum     }
  #{setopt CURLOPT_PROXY                  , string   }
  #{setopt CURLOPT_PROXYPORT              , clong    }
  #{setopt CURLOPT_PROXYTYPE              , enum     }
  #{setopt CURLOPT_NOPROXY                , string   }
  #{setopt CURLOPT_HTTPPROXYTUNNEL        , bool     }
  #{setopt CURLOPT_SOCKS5_GSSAPI_SERVICE  , string   }
  #{setopt CURLOPT_SOCKS5_GSSAPI_NEC      , bool     }
  #{setopt CURLOPT_INTERFACE              , string   }
  #{setopt CURLOPT_LOCALPORT              , clong    }
  #{setopt CURLOPT_LOCALPORTRANGE         , clong    }
  #{setopt CURLOPT_DNS_CACHE_TIMEOUT      , clong    }
  -- #{setopt CURLOPT_DNS_USE_GLOBAL_CACHE   , bool     }
  #{setopt CURLOPT_BUFFERSIZE             , clong    }
  #{setopt CURLOPT_PORT                   , clong    }
  #{setopt CURLOPT_TCP_NODELAY            , bool     }
  #{setopt CURLOPT_ADDRESS_SCOPE          , clong    }
  #{setopt CURLOPT_TCP_KEEPALIVE          , bool     }
  #{setopt CURLOPT_TCP_KEEPIDLE           , clong    }
  #{setopt CURLOPT_TCP_KEEPINTVL          , clong    }

  ---- NAMES and PASSWORDS OPTIONS (Authentication) ---------------------------
  #{setopt CURLOPT_NETRC                  , enum     }
  #{setopt CURLOPT_NETRC_FILE             , string   }
  #{setopt CURLOPT_USERPWD                , string   }
  #{setopt CURLOPT_PROXYUSERPWD           , string   }
  #{setopt CURLOPT_USERNAME               , string   }
  #{setopt CURLOPT_PASSWORD               , string   }
  #{setopt CURLOPT_PROXYUSERNAME          , string   }
  #{setopt CURLOPT_PROXYPASSWORD          , string   }
  #{setopt CURLOPT_HTTPAUTH               , enum     }
  #{setopt CURLOPT_TLSAUTH_TYPE           , string   }
  #{setopt CURLOPT_TLSAUTH_USERNAME       , string   }
  #{setopt CURLOPT_TLSAUTH_PASSWORD       , string   }
  #{setopt CURLOPT_PROXYAUTH              , enum     }

  ---- HTTP OPTIONS -----------------------------------------------------------
  #{setopt CURLOPT_AUTOREFERER            , bool     }
  #{setopt CURLOPT_ACCEPT_ENCODING        , string   }
  #{setopt CURLOPT_TRANSFER_ENCODING      , bool     }
  #{setopt CURLOPT_FOLLOWLOCATION         , bool     }
  #{setopt CURLOPT_UNRESTRICTED_AUTH      , bool     }
  #{setopt CURLOPT_MAXREDIRS              , clong    }
  #{setopt CURLOPT_POSTREDIR              , enum     }
  #{setopt CURLOPT_PUT                    , bool     }
  #{setopt CURLOPT_POST                   , bool     }
  -- #{setopt CURLOPT_POSTFIELDS             , buffer   }
  #{setopt CURLOPT_POSTFIELDSIZE          , clong    }
  #{setopt CURLOPT_POSTFIELDSIZE_LARGE    , int64    }
  #{setopt CURLOPT_COPYPOSTFIELDS         , string   }
  -- CURLOPT_HTTPPOST
  #{setopt CURLOPT_REFERER                , string   }
  #{setopt CURLOPT_USERAGENT              , string   }
  #{setopt CURLOPT_HTTPHEADER             , slist    }
  #{setopt CURLOPT_HTTP200ALIASES         , slist    }
  #{setopt CURLOPT_COOKIE                 , string   }
  #{setopt CURLOPT_COOKIEFILE             , string   }
  #{setopt CURLOPT_COOKIEJAR              , string   }
  #{setopt CURLOPT_COOKIESESSION          , bool     }
  #{setopt CURLOPT_COOKIELIST             , string   }
  #{setopt CURLOPT_HTTPGET                , bool     }
  #{setopt CURLOPT_HTTP_VERSION           , enum     }
  #{setopt CURLOPT_IGNORE_CONTENT_LENGTH  , bool     }
  #{setopt CURLOPT_HTTP_CONTENT_DECODING  , bool     }
  #{setopt CURLOPT_HTTP_TRANSFER_DECODING , bool     }

  ---- SMTP OPTIONS -----------------------------------------------------------
  #{setopt CURLOPT_MAIL_FROM              , string   }
  #{setopt CURLOPT_MAIL_RCPT              , slist    }
  #{setopt CURLOPT_MAIL_AUTH              , string   }

  ---- TFTP OPTIONS -----------------------------------------------------------
  #{setopt CURLOPT_TFTP_BLKSIZE           , clong    }

  ---- FTP OPTIONS ------------------------------------------------------------
  #{setopt CURLOPT_FTPPORT                , string   }
  #{setopt CURLOPT_QUOTE                  , slist    }
  #{setopt CURLOPT_POSTQUOTE              , slist    }
  #{setopt CURLOPT_PREQUOTE               , slist    }
  #{setopt CURLOPT_DIRLISTONLY            , bool     }
  #{setopt CURLOPT_APPEND                 , bool     }
  #{setopt CURLOPT_FTP_USE_EPRT           , bool     }
  #{setopt CURLOPT_FTP_USE_EPSV           , bool     }
  #{setopt CURLOPT_FTP_USE_PRET           , bool     }
  #{setopt CURLOPT_FTP_CREATE_MISSING_DIRS, enum     }
  #{setopt CURLOPT_FTP_RESPONSE_TIMEOUT   , clong    }
  #{setopt CURLOPT_FTP_ALTERNATIVE_TO_USER, string   }
  #{setopt CURLOPT_FTP_SKIP_PASV_IP       , bool     }
  #{setopt CURLOPT_FTPSSLAUTH             , enum     }
  #{setopt CURLOPT_FTP_SSL_CCC            , enum     }
  #{setopt CURLOPT_FTP_ACCOUNT            , string   }
  #{setopt CURLOPT_FTP_FILEMETHOD         , enum     }

  ---- RTSP OPTIONS -----------------------------------------------------------
  #define CURLOPT_RTSP_HEADER CURLOPT_RTSPHEADER
  #{setopt CURLOPT_RTSP_REQUEST           , enum     }
  #{setopt CURLOPT_RTSP_SESSION_ID        , string   }
  #{setopt CURLOPT_RTSP_STREAM_URI        , string   }
  #{setopt CURLOPT_RTSP_TRANSPORT         , string   }
  #{setopt CURLOPT_RTSP_HEADER            , slist    }
  #{setopt CURLOPT_RTSP_CLIENT_CSEQ       , clong    }
  #{setopt CURLOPT_RTSP_SERVER_CSEQ       , clong    }

  ---- PROTOCOL OPTIONS -------------------------------------------------------
  #{setopt CURLOPT_TRANSFERTEXT           , bool     }
  #{setopt CURLOPT_PROXY_TRANSFER_MODE    , bool     }
  #{setopt CURLOPT_CRLF                   , bool     }
  #{setopt CURLOPT_RANGE                  , string   }
  #{setopt CURLOPT_RESUME_FROM            , clong    }
  #{setopt CURLOPT_RESUME_FROM_LARGE      , int64    }
  #{setopt CURLOPT_CUSTOMREQUEST          , string   }
  #{setopt CURLOPT_FILETIME               , bool     }
  #{setopt CURLOPT_NOBODY                 , bool     }
  #{setopt CURLOPT_INFILESIZE             , clong    }
  #{setopt CURLOPT_INFILESIZE_LARGE       , int64    }
  #{setopt CURLOPT_UPLOAD                 , bool     }
  #{setopt CURLOPT_MAXFILESIZE            , clong    }
  #{setopt CURLOPT_MAXFILESIZE_LARGE      , int64    }
  #{setopt CURLOPT_TIMECONDITION          , enum     }
  #{setopt CURLOPT_TIMEVALUE              , time     }

  ---- CONNECTION OPTIONS -----------------------------------------------------
  #{setopt CURLOPT_TIMEOUT                , clong    }
  #{setopt CURLOPT_TIMEOUT_MS             , clong    }
  #{setopt CURLOPT_LOW_SPEED_LIMIT        , clong    }
  #{setopt CURLOPT_LOW_SPEED_TIME         , clong    }
  #{setopt CURLOPT_MAX_SEND_SPEED_LARGE   , int64    }
  #{setopt CURLOPT_MAX_RECV_SPEED_LARGE   , int64    }
  #{setopt CURLOPT_MAXCONNECTS            , clong    }
  -- #{setopt CURLOPT_CLOSEPOLICY            , enum     }
  #{setopt CURLOPT_FRESH_CONNECT          , bool     }
  #{setopt CURLOPT_FORBID_REUSE           , bool     }
  #{setopt CURLOPT_CONNECTTIMEOUT         , clong    }
  #{setopt CURLOPT_CONNECTTIMEOUT_MS      , clong    }
  #{setopt CURLOPT_IPRESOLVE              , enum     }
  #{setopt CURLOPT_CONNECT_ONLY           , bool     }
  #{setopt CURLOPT_USE_SSL                , enum     }
  #{setopt CURLOPT_RESOLVE                , slist    }
  #{setopt CURLOPT_DNS_SERVERS            , string   }
  #{setopt CURLOPT_ACCEPTTIMEOUT_MS       , clong    }

  ---- SSL and SECURITY OPTIONS -----------------------------------------------
  #{setopt CURLOPT_SSLCERT                , string   }
  #{setopt CURLOPT_SSLCERTTYPE            , string   }
  #{setopt CURLOPT_SSLKEY                 , string   }
  #{setopt CURLOPT_SSLKEYTYPE             , string   }
  #{setopt CURLOPT_KEYPASSWD              , string   }
  #{setopt CURLOPT_SSLENGINE              , string   }
  #{setopt CURLOPT_SSLENGINE_DEFAULT      , bool     }
  #{setopt CURLOPT_SSLVERSION             , enum     }
  #{setopt CURLOPT_SSL_VERIFYPEER         , bool     }
  #{setopt CURLOPT_CAINFO                 , string   }
  #{setopt CURLOPT_ISSUERCERT             , string   }
  #{setopt CURLOPT_CAPATH                 , string   }
  #{setopt CURLOPT_CRLFILE                , string   }
  #{setopt CURLOPT_SSL_VERIFYHOST         , clong    }
  #{setopt CURLOPT_CERTINFO               , bool     }
  #{setopt CURLOPT_RANDOM_FILE            , string   }
  #{setopt CURLOPT_EGDSOCKET              , string   }
  #{setopt CURLOPT_SSL_CIPHER_LIST        , string   }
  #{setopt CURLOPT_SSL_SESSIONID_CACHE    , bool     }
  #{setopt CURLOPT_SSL_OPTIONS            , enum     }
  #{setopt CURLOPT_KRBLEVEL               , string   }
  #{setopt CURLOPT_GSSAPI_DELEGATION      , enum     }

  ---- SSH OPTIONS ------------------------------------------------------------
  #{setopt CURLOPT_SSH_AUTH_TYPES         , enum     }
  #{setopt CURLOPT_SSH_HOST_PUBLIC_KEY_MD5, string   }
  #{setopt CURLOPT_SSH_PUBLIC_KEYFILE     , string   }
  #{setopt CURLOPT_SSH_PRIVATE_KEYFILE    , string   }
  #{setopt CURLOPT_SSH_KNOWNHOSTS         , string   }
  -- CURLOPT_SSH_KEYFUNCTION
  -- CURLOPT_SSH_KEYDATA

  ---- OTHER OPTIONS ----------------------------------------------------------
  -- CURLOPT_PRIVATE
  #{setopt CURLOPT_SHARE                  , curlsh   }
  #{setopt CURLOPT_NEW_FILE_PERMS         , clong    }
  #{setopt CURLOPT_NEW_DIRECTORY_PERMS    , clong    }

  ---- TELNET OPTIONS ---------------------------------------------------------
  #{setopt CURLOPT_TELNETOPTIONS          , slist    }

  -----------------------------------------------------------------------------
  where
    curlcb :: CURLCBT a -> CInt -> Maybe a -> IO ()
    curlcb cbt copt x = makeCB curl copt x cbt
    enum :: CURLENUM a => CInt -> a -> IO ()
    enum   copt x = setopt'Long ccurl copt (toCLong      x)
    bool   copt x = setopt'Long ccurl copt (fromBool     x)
    time   copt x = setopt'Long ccurl copt (fromUTCTime  x)
    clong  copt x = setopt'Long ccurl copt (fromIntegral x)
    int64  copt x = setopt'COff ccurl copt (fromIntegral x)
    slist  copt x = makeSL curl copt x
    string copt x = withCAString x $ \p -> setopt'DPtr ccurl copt (castPtr p)
    fromUTCTime = truncate . utcTimeToPOSIXSeconds
    curlsh copt (Just (CURLSH ccurlsh _ _)) = setopt'DPtr ccurl copt (castPtr ccurlsh)
    curlsh copt Nothing = setopt'DPtr ccurl copt nullPtr


-------------------------------------------------------------------------------
setopt'Long :: Ptr C.CURL -> CInt -> CLong -> IO ()
setopt'Long a b c = withCURLE $ C.curl_easy_setopt'Long a b c

setopt'COff :: Ptr C.CURL -> CInt -> C.CURL_off_t -> IO ()
setopt'COff a b c = withCURLE $ C.curl_easy_setopt'COff a b c

setopt'DPtr :: Ptr C.CURL -> CInt -> Ptr () -> IO ()
setopt'DPtr a b c = withCURLE $ C.curl_easy_setopt'DPtr a b c

setopt'FPtr :: Ptr C.CURL -> CInt -> FunPtr () -> IO ()
setopt'FPtr a b c = withCURLE $ C.curl_easy_setopt'FPtr a b c


-------------------------------------------------------------------------------
type CURLSL = (CInt, Ptr C.CURLslist)

wrapSL :: Ptr C.CURLslist -> String -> IO (Ptr C.CURLslist)
wrapSL sl xs = do
  nsl <- withCAString xs (C.curl_slist_append sl)
  when (nsl==nullPtr) (C.curl_slist_free_all sl)
  return nsl

keepSL :: CURLSL -> [CURLSL] -> ([CURLSL], [Ptr C.CURLslist])
keepSL sl@(copt, ptr) sls = (tokeep, tofree) where
  (tokeep0, tofree0) = partition ((==copt) . fst) sls
  tokeep = if (ptr==nullPtr) then tokeep0 else (sl:tokeep0)
  tofree = map snd tofree0

makeSL :: CURL -> CInt -> [String] -> IO ()
makeSL (CURL ccurl _ slref) copt xs = do
  sl <- foldM wrapSL nullPtr xs
  setopt'DPtr ccurl copt (castPtr sl) `onException` (C.curl_slist_free_all sl)
  fsls <- atomicModifyIORef slref (keepSL (copt, sl))
  mapM_ C.curl_slist_free_all fsls


-------------------------------------------------------------------------------
data CURLCB where
  CURLCB :: CURLCBT a -> FunPtr () -> CURLCB

data CURLCBT a where
  FWRITE :: CURLCBT CURL_write_callback
  FREAD  :: CURLCBT CURL_read_callback

wrapCB :: CURLCBT a -> a -> IO (FunPtr ())
wrapCB cbt = case cbt of
  FWRITE -> fmap castFunPtr . C.wrapCURL_write_callback . write_callback
  FREAD  -> fmap castFunPtr . C.wrapCURL_read_callback  . read_callback

compCB :: CURLCBT a -> CURLCBT b -> Bool
compCB FWRITE FWRITE = True
compCB FREAD  FREAD  = True
compCB _ _ = False

keepCB :: CURLCB -> [CURLCB] -> ([CURLCB], [FunPtr ()])
keepCB cb@(CURLCB cbt fp) cbs = (tokeep, tofree) where
  (tokeep0, tofree0) = partition (\(CURLCB t _) -> (compCB t cbt)) cbs
  tokeep = if (fp==nullFunPtr) then tokeep0 else (cb:tokeep0)
  tofree = map (\(CURLCB _ p) -> castFunPtr p) tofree0

makeCB :: CURL -> CInt -> Maybe a -> CURLCBT a -> IO ()
makeCB (CURL ccurl cbref _) copt mcb cbt = do
  fp <- maybe (return nullFunPtr) (wrapCB cbt) mcb
  let freefp = when (fp/=nullFunPtr) (freeHaskellFunPtr fp)
  setopt'FPtr ccurl copt fp `onException` freefp
  ffps <- atomicModifyIORef cbref (keepCB (CURLCB cbt fp))
  mapM_ freeHaskellFunPtr ffps


-------------------------------------------------------------------------------
write_callback :: CURL_write_callback -> C.CURL_write_callback
write_callback fwrite ptr size nmemb _ = do
  stat <- packCStringLen (ptr, fromIntegral (size * nmemb)) >>= fwrite
  return $ case stat of
    CURL_WRITEFUNC_OK    -> (size * nmemb)
    CURL_WRITEFUNC_FAIL  -> 0
    CURL_WRITEFUNC_PAUSE -> #{const CURL_WRITEFUNC_PAUSE}


-------------------------------------------------------------------------------
read_callback :: CURL_read_callback -> C.CURL_read_callback
read_callback fread buff size nmemb _ = do
  let buffLen = fromIntegral (size * nmemb)
  stat <- fread buffLen
  case stat of
    CURL_READFUNC_PAUSE -> return #{const CURL_READFUNC_PAUSE}
    CURL_READFUNC_ABORT -> return #{const CURL_READFUNC_ABORT}
    CURL_READFUNC_OK bs -> unsafeUseAsCStringLen (BS.take buffLen bs)
      (\(cs, cl) -> copyBytes buff cs cl >> return (fromIntegral cl))



-------------------------------------------------------------------------------
-- | Create a shared object
--   (<http://curl.haxx.se/libcurl/c/curl_share_init.html>).
-------------------------------------------------------------------------------
curl_share_init :: IO CURLSH
curl_share_init = do
  shlocks <- newSHLocks
  ccurlsh <- C.curl_share_init >>= checkNULL
  f1 <- fmap castFunPtr $ C.wrapCURL_lock_function   (lock_function   shlocks)
  f2 <- fmap castFunPtr $ C.wrapCURL_unlock_function (unlock_function shlocks)
  setshopt'FPtr ccurlsh #{const CURLSHOPT_LOCKFUNC  } f1
  setshopt'FPtr ccurlsh #{const CURLSHOPT_UNLOCKFUNC} f2
  return (CURLSH ccurlsh f1 f2)

withCURLSH :: (CURLSH -> IO a) -> IO a
withCURLSH = bracket curl_share_init curl_share_cleanup


-------------------------------------------------------------------------------
-- | Clean up a shared object
--   (<http://curl.haxx.se/libcurl/c/curl_share_cleanup.html>).
-------------------------------------------------------------------------------
curl_share_cleanup :: CURLSH -> IO ()
curl_share_cleanup (CURLSH ccurlsh flock funlock) = do
  withCURLSHE $ C.curl_share_cleanup ccurlsh
  freeHaskellFunPtr flock
  freeHaskellFunPtr funlock


-------------------------------------------------------------------------------
-- | Set options for a shared object
--   (<http://curl.haxx.se/libcurl/c/curl_share_setopt.html>).
-------------------------------------------------------------------------------
curl_share_setopt :: CURLSH -> [CURLSHoption] -> IO ()
curl_share_setopt (CURLSH ccurlsh _ _) opts =
  let enum :: CURLENUM a => CInt -> a -> IO ()
      enum copt x = setshopt'Long ccurlsh copt (toCLong x)
  in  flip mapM_ opts $ \opt -> case opt of
        #{setopt CURLSHOPT_SHARE  , enum}
        #{setopt CURLSHOPT_UNSHARE, enum}


-------------------------------------------------------------------------------
setshopt'Long :: Ptr C.CURLSH -> CInt -> CLong -> IO ()
setshopt'Long a b c = withCURLSHE $ C.curl_share_setopt'Long a b c

setshopt'FPtr :: Ptr C.CURLSH -> CInt -> FunPtr () -> IO ()
setshopt'FPtr a b c = withCURLSHE $ C.curl_share_setopt'FPtr a b c


-------------------------------------------------------------------------------
type SHLocks  = MVar [(SHLockID, SHLock)]
type SHLock   = MVar ()
type SHLockID = CInt

newSHLocks :: IO SHLocks
newSHLocks = newMVar []

getSHLock :: SHLock -> IO ()
getSHLock shlock = takeMVar shlock

putSHLock :: SHLock -> IO ()
putSHLock shlock = tryPutMVar shlock () >> return ()

lookupSHLock :: SHLocks -> SHLockID -> IO SHLock
lookupSHLock mvar shlockid =
  modifyMVar mvar $ \shlocks ->
    case (lookup shlockid shlocks) of
      Just shlock -> return (shlocks, shlock)
      Nothing     -> newMVar () >>= \nl -> return ((shlockid, nl):shlocks, nl)


-------------------------------------------------------------------------------
lock_function :: SHLocks -> C.CURL_lock_function
lock_function shlocks _ccurl lockdata _lockaccess _usrptr =
  lookupSHLock shlocks lockdata >>= getSHLock

unlock_function :: SHLocks -> C.CURL_unlock_function
unlock_function shlocks _ccurl lockdata _usrptr =
  lookupSHLock shlocks lockdata >>= putSHLock

