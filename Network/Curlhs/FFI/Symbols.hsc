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

#include "curl/curl.h"

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

#define hsc_symbol(name, type) \
  printf("c" #name " :: C" #type "\n"); \
  printf("c" #name " =  C" #type " "); hsc_const(name);


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
#define hsc_curlcode(name) hsc_symbol(name, CURLcode)

#if LIBCURL_VERSION_NUM >= CURL_7_20_0
#{curlcode CURLE_OK                      }
#{curlcode CURLE_UNSUPPORTED_PROTOCOL    }
#{curlcode CURLE_FAILED_INIT             }
#{curlcode CURLE_URL_MALFORMAT           }
#{curlcode CURLE_COULDNT_RESOLVE_PROXY   }
#{curlcode CURLE_COULDNT_RESOLVE_HOST    }
#{curlcode CURLE_COULDNT_CONNECT         }
#{curlcode CURLE_FTP_WEIRD_SERVER_REPLY  }
#{curlcode CURLE_REMOTE_ACCESS_DENIED    }
#{curlcode CURLE_FTP_WEIRD_PASS_REPLY    }
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
#if LIBCURL_VERSION_NUM < CURL_7_21_5
#{curlcode CURLE_UNKNOWN_TELNET_OPTION   }
#endif
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
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_0
#{curlcode CURLE_FTP_BAD_FILE_LIST       }
#{curlcode CURLE_CHUNK_FAILED            }
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_21_5
#{curlcode CURLE_NOT_BUILT_IN            }
#{curlcode CURLE_UNKNOWN_OPTION          }
#endif

#if LIBCURL_VERSION_NUM >= CURL_7_24_0
#{curlcode CURLE_FTP_ACCEPT_FAILED       }
#{curlcode CURLE_FTP_ACCEPT_TIMEOUT      }
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

