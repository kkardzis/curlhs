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
libCURL_COPYRIGHT     = #{const_str LIBCURL_COPYRIGHT    }
libCURL_VERSION       = #{const_str LIBCURL_VERSION      }
libCURL_VERSION_MAJOR = #{const     LIBCURL_VERSION_MAJOR}
libCURL_VERSION_MINOR = #{const     LIBCURL_VERSION_MINOR}
libCURL_VERSION_PATCH = #{const     LIBCURL_VERSION_PATCH}
libCURL_VERSION_NUM   = #{const     LIBCURL_VERSION_NUM  }
libCURL_TIMESTAMP     = #{const_str LIBCURL_TIMESTAMP    }


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
cURLVERSION_FIRST  = #{const CURLVERSION_FIRST } :: CCURLversion
cURLVERSION_SECOND = #{const CURLVERSION_SECOND} :: CCURLversion
cURLVERSION_THIRD  = #{const CURLVERSION_THIRD } :: CCURLversion
cURLVERSION_FOURTH = #{const CURLVERSION_FOURTH} :: CCURLversion
cURLVERSION_NOW    = #{const CURLVERSION_NOW   } :: CCURLversion

cURL_VERSION_IPV6         = #{const CURL_VERSION_IPV6        } :: CInt
cURL_VERSION_KERBEROS4    = #{const CURL_VERSION_KERBEROS4   } :: CInt
cURL_VERSION_SSL          = #{const CURL_VERSION_SSL         } :: CInt
cURL_VERSION_LIBZ         = #{const CURL_VERSION_LIBZ        } :: CInt
cURL_VERSION_NTLM         = #{const CURL_VERSION_NTLM        } :: CInt
cURL_VERSION_GSSNEGOTIATE = #{const CURL_VERSION_GSSNEGOTIATE} :: CInt
cURL_VERSION_DEBUG        = #{const CURL_VERSION_DEBUG       } :: CInt
cURL_VERSION_ASYNCHDNS    = #{const CURL_VERSION_ASYNCHDNS   } :: CInt
cURL_VERSION_SPNEGO       = #{const CURL_VERSION_SPNEGO      } :: CInt
cURL_VERSION_LARGEFILE    = #{const CURL_VERSION_LARGEFILE   } :: CInt
cURL_VERSION_IDN          = #{const CURL_VERSION_IDN         } :: CInt
cURL_VERSION_SSPI         = #{const CURL_VERSION_SSPI        } :: CInt
cURL_VERSION_CONV         = #{const CURL_VERSION_CONV        } :: CInt
cURL_VERSION_CURLDEBUG    = #{const CURL_VERSION_CURLDEBUG   } :: CInt

