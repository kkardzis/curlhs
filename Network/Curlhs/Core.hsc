-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Core
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curlhs.Core
  ( withLIBCURL
  , withGlobalInitCheck 
  , curlGlobalLocks  
  , curl_version
  , curl_version_info

  , withCODE
  , curl_easy_strerror
  , withCURLSHE
  , curl_share_strerror
  , traceIO
  ) where

import Foreign.Storable      (peek, sizeOf)
import Foreign.C.String      (peekCString)
import Foreign.C.Types       (CChar, CInt)
import Foreign.Ptr           (Ptr, nullPtr, plusPtr)

import Data.ByteString       (ByteString, packCString)
import Data.Maybe            (mapMaybe)
import Data.Bits             ((.&.), (.|.))
import Data.List             (foldl')

import Control.Applicative   ((<$>), (<*>))
import Control.Concurrent    (myThreadId)
import Control.Concurrent    (MVar, newMVar, takeMVar)
import Control.Concurrent    (modifyMVar, modifyMVar_, tryTakeMVar)
import Control.Exception     (throwIO, bracket_)
import Control.Monad         (when)

import System.IO.Unsafe      (unsafePerformIO)
import Debug.Trace           (putTraceMsg)

import Network.Curlhs.Base
import Network.Curlhs.Types



-------------------------------------------------------------------------------
curlGlobalInitFlag :: MVar ()
curlGlobalInitFlag = unsafePerformIO $ newMVar ()
{-# NOINLINE curlGlobalInitFlag #-}

curlGlobalLocks :: MVar ([MVar ()])
curlGlobalLocks = unsafePerformIO $ newMVar []
{-# NOINLINE curlGlobalLocks #-}


-------------------------------------------------------------------------------
withGlobalInitCheck :: IO a -> IO a
withGlobalInitCheck action = action


-------------------------------------------------------------------------------
withLIBCURL :: IO a -> IO a
withLIBCURL userIO = tryTakeMVar curlGlobalInitFlag >>= maybe userIO wrappedIO
  where wrappedIO _ = bracket_ globalInit globalCleanup userIO

globalInit :: IO ()
globalInit = modifyMVar_ curlGlobalLocks $ \_ ->
  curl_global_init [CURL_GLOBAL_ALL] >> newMVar () >>= \lock -> return [lock]

globalCleanup :: IO ()
globalCleanup = waitForLocks >> curl_global_cleanup

waitForLocks :: IO ()
waitForLocks = getLock >>= maybe (return ()) (\x -> takeMVar x >> waitForLocks)
  where getLock = modifyMVar curlGlobalLocks $ \locks -> case locks of
          [ ]   -> return ([], Nothing)
          [_]   -> return ([], Nothing)
          (x:_) -> return (locks, Just x)





-------------------------------------------------------------------------------
-- | Global libcurl initialisation
--   (<http://curl.haxx.se/libcurl/c/curl_global_init.html>).
-------------------------------------------------------------------------------
curl_global_init :: [CURLglobal] -> IO ()
curl_global_init xs = withCODE $ ccurl_global_init flags
  where
    flags = foldl' (.|.) 0 $ flip map xs $ \x -> case x of
      CURL_GLOBAL_ALL     -> cCURL_GLOBAL_ALL
      CURL_GLOBAL_SSL     -> cCURL_GLOBAL_SSL
      CURL_GLOBAL_WIN32   -> cCURL_GLOBAL_WIN32
      CURL_GLOBAL_NOTHING -> cCURL_GLOBAL_NOTHING
      CURL_GLOBAL_DEFAULT -> cCURL_GLOBAL_DEFAULT


-------------------------------------------------------------------------------
-- | Global libcurl cleanup
--   (<http://curl.haxx.se/libcurl/c/curl_global_cleanup.html>).
-------------------------------------------------------------------------------
curl_global_cleanup :: IO ()
curl_global_cleanup = ccurl_global_cleanup


-------------------------------------------------------------------------------
-- | Returns the libcurl version string
--   (<http://curl.haxx.se/libcurl/c/curl_version.html>).
-------------------------------------------------------------------------------
curl_version :: IO String
curl_version = ccurl_version >>= peekCString


-------------------------------------------------------------------------------
-- | Returns run-time libcurl version info
--   (<http://curl.haxx.se/libcurl/c/curl_version_info.html>).
-------------------------------------------------------------------------------
curl_version_info :: IO CURL_version_info_data
curl_version_info = ccurl_version_info cCURLVERSION_NOW >>= peek >>=
  \cval -> CURL_version_info_data
    <$> (peekCString      $ ccurl_version_info_data_version         cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_version_num     cval)
    <*> (peekCString      $ ccurl_version_info_data_host            cval)
    <*> (peekCFeatures    $ ccurl_version_info_data_features        cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_ssl_version     cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_ssl_version_num cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libz_version    cval)
    <*> (peekCStringList  $ ccurl_version_info_data_protocols       cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_ares            cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_ares_num        cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libidn          cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_iconv_ver_num   cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libssh_version  cval)

peekCStringList :: Ptr (Ptr CChar) -> IO [String]
peekCStringList ptr = peek ptr >>= \cstring ->
  if (cstring == nullPtr) then return [] else do
    let size = sizeOf (undefined :: Ptr CChar)
    strings <- peekCStringList (plusPtr ptr size)
    string  <- peekCString cstring
    return (string : strings)

peekCStringMaybe :: Ptr CChar -> IO (Maybe String)
peekCStringMaybe ptr = if (ptr /= nullPtr)
  then Just <$> peekCString ptr
  else return Nothing

peekCIntegral :: (Num h, Integral c) => c -> IO h
peekCIntegral = return . fromIntegral

peekCFeatures :: CInt -> IO [CURL_version]
peekCFeatures mask =
  return $ mapMaybe (\(v, b) -> if (mask .&. b == 0) then Nothing else Just v)
    [ (CURL_VERSION_IPV6        , cCURL_VERSION_IPV6        )
    , (CURL_VERSION_KERBEROS4   , cCURL_VERSION_KERBEROS4   )
    , (CURL_VERSION_SSL         , cCURL_VERSION_SSL         )
    , (CURL_VERSION_LIBZ        , cCURL_VERSION_LIBZ        )
    , (CURL_VERSION_NTLM        , cCURL_VERSION_NTLM        )
    , (CURL_VERSION_GSSNEGOTIATE, cCURL_VERSION_GSSNEGOTIATE)
    , (CURL_VERSION_DEBUG       , cCURL_VERSION_DEBUG       )
    , (CURL_VERSION_ASYNCHDNS   , cCURL_VERSION_ASYNCHDNS   )
    , (CURL_VERSION_SPNEGO      , cCURL_VERSION_SPNEGO      )
    , (CURL_VERSION_LARGEFILE   , cCURL_VERSION_LARGEFILE   )
    , (CURL_VERSION_IDN         , cCURL_VERSION_IDN         )
    , (CURL_VERSION_SSPI        , cCURL_VERSION_SSPI        )
    , (CURL_VERSION_CONV        , cCURL_VERSION_CONV        )
    , (CURL_VERSION_CURLDEBUG   , cCURL_VERSION_CURLDEBUG   )
    , (CURL_VERSION_TLSAUTH_SRP , cCURL_VERSION_TLSAUTH_SRP ) |7214:----|
    , (CURL_VERSION_NTLM_WB     , cCURL_VERSION_NTLM_WB     ) |7220:----|
    ]



-------------------------------------------------------------------------------
traceIO :: String -> IO ()
traceIO s = myThreadId >>= \t -> putTraceMsg ("[" ++ show t ++ "] " ++ s)


-------------------------------------------------------------------------------
withCODE :: IO CCURLcode -> IO ()
withCODE action =
  action >>= \code -> when (code /= cCURLE_OK) (throwIO (fromCCURLcode code))


-------------------------------------------------------------------------------
-- | Returns a string describing error code
--   (<http://curl.haxx.se/libcurl/c/curl_easy_strerror.html>).
-------------------------------------------------------------------------------
curl_easy_strerror :: CURLcode -> IO ByteString
curl_easy_strerror code =
  ccurl_easy_strerror (fromCURLcode code) >>= packCString


-------------------------------------------------------------------------------
#define hsc_curlcode(code) printf(#code " -> c" #code);

fromCURLcode :: CURLcode -> CCURLcode
fromCURLcode x = case x of
  #{curlcode CURLE_OK                      }
  #{curlcode CURLE_UNSUPPORTED_PROTOCOL    }
  #{curlcode CURLE_FAILED_INIT             }
  #{curlcode CURLE_URL_MALFORMAT           }
  #{curlcode CURLE_NOT_BUILT_IN            } |7215:----|
  #{curlcode CURLE_COULDNT_RESOLVE_PROXY   }
  #{curlcode CURLE_COULDNT_RESOLVE_HOST    }
  #{curlcode CURLE_COULDNT_CONNECT         }
  #{curlcode CURLE_FTP_WEIRD_SERVER_REPLY  }
  #{curlcode CURLE_REMOTE_ACCESS_DENIED    }
  #{curlcode CURLE_FTP_ACCEPT_FAILED       } |7240:----|
  #{curlcode CURLE_FTP_WEIRD_PASS_REPLY    }
  #{curlcode CURLE_FTP_ACCEPT_TIMEOUT      } |7240:----|
  #{curlcode CURLE_FTP_WEIRD_PASV_REPLY    }
  #{curlcode CURLE_FTP_WEIRD_227_FORMAT    }
  #{curlcode CURLE_FTP_CANT_GET_HOST       }
  #{curlcode CURLE_FTP_COULDNT_SET_TYPE    }
  #{curlcode CURLE_PARTIAL_FILE            }
  #{curlcode CURLE_FTP_COULDNT_RETR_FILE   }
  #{curlcode CURLE_QUOTE_ERROR             }
  #{curlcode CURLE_HTTP_RETURNED_ERROR     }
  #{curlcode CURLE_WRITE_ERROR             }
  #{curlcode CURLE_UPLOAD_FAILED           }
  #{curlcode CURLE_READ_ERROR              }
  #{curlcode CURLE_OUT_OF_MEMORY           }
  #{curlcode CURLE_OPERATION_TIMEDOUT      }
  #{curlcode CURLE_FTP_PORT_FAILED         }
  #{curlcode CURLE_FTP_COULDNT_USE_REST    }
  #{curlcode CURLE_RANGE_ERROR             }
  #{curlcode CURLE_HTTP_POST_ERROR         }
  #{curlcode CURLE_SSL_CONNECT_ERROR       }
  #{curlcode CURLE_BAD_DOWNLOAD_RESUME     }
  #{curlcode CURLE_FILE_COULDNT_READ_FILE  }
  #{curlcode CURLE_LDAP_CANNOT_BIND        }
  #{curlcode CURLE_LDAP_SEARCH_FAILED      }
  #{curlcode CURLE_FUNCTION_NOT_FOUND      }
  #{curlcode CURLE_ABORTED_BY_CALLBACK     }
  #{curlcode CURLE_BAD_FUNCTION_ARGUMENT   }
  #{curlcode CURLE_INTERFACE_FAILED        }
  #{curlcode CURLE_TOO_MANY_REDIRECTS      }
  #{curlcode CURLE_UNKNOWN_TELNET_OPTION   } |----:7214|
  #{curlcode CURLE_UNKNOWN_OPTION          } |7215:----|
  #{curlcode CURLE_TELNET_OPTION_SYNTAX    }
  #{curlcode CURLE_PEER_FAILED_VERIFICATION}
  #{curlcode CURLE_GOT_NOTHING             }
  #{curlcode CURLE_SSL_ENGINE_NOTFOUND     }
  #{curlcode CURLE_SSL_ENGINE_SETFAILED    }
  #{curlcode CURLE_SEND_ERROR              }
  #{curlcode CURLE_RECV_ERROR              }
  #{curlcode CURLE_SSL_CERTPROBLEM         }
  #{curlcode CURLE_SSL_CIPHER              }
  #{curlcode CURLE_SSL_CACERT              }
  #{curlcode CURLE_BAD_CONTENT_ENCODING    }
  #{curlcode CURLE_LDAP_INVALID_URL        }
  #{curlcode CURLE_FILESIZE_EXCEEDED       }
  #{curlcode CURLE_USE_SSL_FAILED          }
  #{curlcode CURLE_SEND_FAIL_REWIND        }
  #{curlcode CURLE_SSL_ENGINE_INITFAILED   }
  #{curlcode CURLE_LOGIN_DENIED            }
  #{curlcode CURLE_TFTP_NOTFOUND           }
  #{curlcode CURLE_TFTP_PERM               }
  #{curlcode CURLE_REMOTE_DISK_FULL        }
  #{curlcode CURLE_TFTP_ILLEGAL            }
  #{curlcode CURLE_TFTP_UNKNOWNID          }
  #{curlcode CURLE_REMOTE_FILE_EXISTS      }
  #{curlcode CURLE_TFTP_NOSUCHUSER         }
  #{curlcode CURLE_CONV_FAILED             }
  #{curlcode CURLE_CONV_REQD               }
  #{curlcode CURLE_SSL_CACERT_BADFILE      }
  #{curlcode CURLE_REMOTE_FILE_NOT_FOUND   }
  #{curlcode CURLE_SSH                     }
  #{curlcode CURLE_SSL_SHUTDOWN_FAILED     }
  #{curlcode CURLE_AGAIN                   }
  #{curlcode CURLE_SSL_CRL_BADFILE         }
  #{curlcode CURLE_SSL_ISSUER_ERROR        }
  #{curlcode CURLE_FTP_PRET_FAILED         }
  #{curlcode CURLE_RTSP_CSEQ_ERROR         }
  #{curlcode CURLE_RTSP_SESSION_ERROR      }
  #{curlcode CURLE_FTP_BAD_FILE_LIST       } |7210:----|
  #{curlcode CURLE_CHUNK_FAILED            } |7210:----|


-------------------------------------------------------------------------------
#define hsc_ccurlcode(code) printf("| x == c" #code " = " #code);

fromCCURLcode :: CCURLcode -> CURLcode
fromCCURLcode x
  #{ccurlcode CURLE_OK                      }
  #{ccurlcode CURLE_UNSUPPORTED_PROTOCOL    }
  #{ccurlcode CURLE_FAILED_INIT             }
  #{ccurlcode CURLE_URL_MALFORMAT           }
  #{ccurlcode CURLE_NOT_BUILT_IN            } |7215:----|
  #{ccurlcode CURLE_COULDNT_RESOLVE_PROXY   }
  #{ccurlcode CURLE_COULDNT_RESOLVE_HOST    }
  #{ccurlcode CURLE_COULDNT_CONNECT         }
  #{ccurlcode CURLE_FTP_WEIRD_SERVER_REPLY  }
  #{ccurlcode CURLE_REMOTE_ACCESS_DENIED    }
  #{ccurlcode CURLE_FTP_ACCEPT_FAILED       } |7240:----|
  #{ccurlcode CURLE_FTP_WEIRD_PASS_REPLY    }
  #{ccurlcode CURLE_FTP_ACCEPT_TIMEOUT      } |7240:----|
  #{ccurlcode CURLE_FTP_WEIRD_PASV_REPLY    }
  #{ccurlcode CURLE_FTP_WEIRD_227_FORMAT    }
  #{ccurlcode CURLE_FTP_CANT_GET_HOST       }
  #{ccurlcode CURLE_FTP_COULDNT_SET_TYPE    }
  #{ccurlcode CURLE_PARTIAL_FILE            }
  #{ccurlcode CURLE_FTP_COULDNT_RETR_FILE   }
  #{ccurlcode CURLE_QUOTE_ERROR             }
  #{ccurlcode CURLE_HTTP_RETURNED_ERROR     }
  #{ccurlcode CURLE_WRITE_ERROR             }
  #{ccurlcode CURLE_UPLOAD_FAILED           }
  #{ccurlcode CURLE_READ_ERROR              }
  #{ccurlcode CURLE_OUT_OF_MEMORY           }
  #{ccurlcode CURLE_OPERATION_TIMEDOUT      }
  #{ccurlcode CURLE_FTP_PORT_FAILED         }
  #{ccurlcode CURLE_FTP_COULDNT_USE_REST    }
  #{ccurlcode CURLE_RANGE_ERROR             }
  #{ccurlcode CURLE_HTTP_POST_ERROR         }
  #{ccurlcode CURLE_SSL_CONNECT_ERROR       }
  #{ccurlcode CURLE_BAD_DOWNLOAD_RESUME     }
  #{ccurlcode CURLE_FILE_COULDNT_READ_FILE  }
  #{ccurlcode CURLE_LDAP_CANNOT_BIND        }
  #{ccurlcode CURLE_LDAP_SEARCH_FAILED      }
  #{ccurlcode CURLE_FUNCTION_NOT_FOUND      }
  #{ccurlcode CURLE_ABORTED_BY_CALLBACK     }
  #{ccurlcode CURLE_BAD_FUNCTION_ARGUMENT   }
  #{ccurlcode CURLE_INTERFACE_FAILED        }
  #{ccurlcode CURLE_TOO_MANY_REDIRECTS      }
  #{ccurlcode CURLE_UNKNOWN_TELNET_OPTION   } |----:7214|
  #{ccurlcode CURLE_UNKNOWN_OPTION          } |7215:----|
  #{ccurlcode CURLE_TELNET_OPTION_SYNTAX    }
  #{ccurlcode CURLE_PEER_FAILED_VERIFICATION}
  #{ccurlcode CURLE_GOT_NOTHING             }
  #{ccurlcode CURLE_SSL_ENGINE_NOTFOUND     }
  #{ccurlcode CURLE_SSL_ENGINE_SETFAILED    }
  #{ccurlcode CURLE_SEND_ERROR              }
  #{ccurlcode CURLE_RECV_ERROR              }
  #{ccurlcode CURLE_SSL_CERTPROBLEM         }
  #{ccurlcode CURLE_SSL_CIPHER              }
  #{ccurlcode CURLE_SSL_CACERT              }
  #{ccurlcode CURLE_BAD_CONTENT_ENCODING    }
  #{ccurlcode CURLE_LDAP_INVALID_URL        }
  #{ccurlcode CURLE_FILESIZE_EXCEEDED       }
  #{ccurlcode CURLE_USE_SSL_FAILED          }
  #{ccurlcode CURLE_SEND_FAIL_REWIND        }
  #{ccurlcode CURLE_SSL_ENGINE_INITFAILED   }
  #{ccurlcode CURLE_LOGIN_DENIED            }
  #{ccurlcode CURLE_TFTP_NOTFOUND           }
  #{ccurlcode CURLE_TFTP_PERM               }
  #{ccurlcode CURLE_REMOTE_DISK_FULL        }
  #{ccurlcode CURLE_TFTP_ILLEGAL            }
  #{ccurlcode CURLE_TFTP_UNKNOWNID          }
  #{ccurlcode CURLE_REMOTE_FILE_EXISTS      }
  #{ccurlcode CURLE_TFTP_NOSUCHUSER         }
  #{ccurlcode CURLE_CONV_FAILED             }
  #{ccurlcode CURLE_CONV_REQD               }
  #{ccurlcode CURLE_SSL_CACERT_BADFILE      }
  #{ccurlcode CURLE_REMOTE_FILE_NOT_FOUND   }
  #{ccurlcode CURLE_SSH                     }
  #{ccurlcode CURLE_SSL_SHUTDOWN_FAILED     }
  #{ccurlcode CURLE_AGAIN                   }
  #{ccurlcode CURLE_SSL_CRL_BADFILE         }
  #{ccurlcode CURLE_SSL_ISSUER_ERROR        }
  #{ccurlcode CURLE_FTP_PRET_FAILED         }
  #{ccurlcode CURLE_RTSP_CSEQ_ERROR         }
  #{ccurlcode CURLE_RTSP_SESSION_ERROR      }
  #{ccurlcode CURLE_FTP_BAD_FILE_LIST       } |7210:----|
  #{ccurlcode CURLE_CHUNK_FAILED            } |7210:----|
  | otherwise = error "unknown CURLcode"




-------------------------------------------------------------------------------
withCURLSHE :: IO CCURLSHcode -> IO ()
withCURLSHE action =
  action >>= \code ->
    when (code /= cCURLSHE_OK) (throwIO (fromCCURLSHcode code))


-------------------------------------------------------------------------------
-- | Returns a string describing error code
--   (<http://curl.haxx.se/libcurl/c/curl_share_strerror.html>).
-------------------------------------------------------------------------------
curl_share_strerror :: CURLSHcode -> IO ByteString
curl_share_strerror code =
  ccurl_share_strerror (fromCURLSHcode code) >>= packCString


-------------------------------------------------------------------------------
#define hsc_curlshcode(code) printf(#code " -> c" #code);

fromCURLSHcode :: CURLSHcode -> CCURLSHcode
fromCURLSHcode x = case x of
  #{curlshcode CURLSHE_OK          }
  #{curlshcode CURLSHE_BAD_OPTION  }
  #{curlshcode CURLSHE_IN_USE      }
  #{curlshcode CURLSHE_INVALID     }
  #{curlshcode CURLSHE_NOMEM       }
  #{curlshcode CURLSHE_NOT_BUILT_IN} |7230:----|


-------------------------------------------------------------------------------
#define hsc_ccurlshcode(code) printf("| x == c" #code " = " #code);

fromCCURLSHcode :: CCURLSHcode -> CURLSHcode
fromCCURLSHcode x
  #{ccurlshcode CURLSHE_OK          }
  #{ccurlshcode CURLSHE_BAD_OPTION  }
  #{ccurlshcode CURLSHE_IN_USE      }
  #{ccurlshcode CURLSHE_INVALID     }
  #{ccurlshcode CURLSHE_NOMEM       }
  #{ccurlshcode CURLSHE_NOT_BUILT_IN} |7230:----|
  | otherwise = error "unknown CURLSHcode"

