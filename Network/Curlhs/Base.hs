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

module Network.Curlhs.Base
  ( module Network.Curlhs.FFI.Functions
  , module Network.Curlhs.FFI.Callbacks
  , module Network.Curlhs.FFI.Symbols

  -- module Network.Curlhs.FFI.Types
  -- from "curlbuild.h"
  , CCURL_socket_t
  , CCURL_off_t

  -- simple types from "curl.h"
  , CCURL
  , CCURLSH
  , CCURLfiletype
  , CCURLsocktype
  , CCURLioerr
  , CCURLiocmd
  , CCURL_infotype
  , CCURLcode
  , CCURL_proxytype
  , CCURL_khtype
  , CCURL_khstat
  , CCURL_khmatch
  , CCURL_usessl
  , CCURL_ftpccc
  , CCURL_ftpauth
  , CCURL_ftpcreatedir
  , CCURL_ftpmethod
  , CCURLoption'Int32
  , CCURLoption'Int64
  , CCURLoption'String
  , CCURLoption'SList
  , CCURLoption'HTTPP
  , CCURLoption'File
  , CCURLoption'Share
  , CCURLoption'Ptr_a
  , CCURLoption'FunPtr
  , CCURLoption'FWRITE
  , CCURLoption'FREAD
  , CCURL_http_version
  , CCURL_rtspreq
  , CCURL_netrc_option
  , CCURL_sslversion
  , CCURL_tlsauth
  , CCURL_timecond
  , CCURLformoption
  , CCURLformcode
  , CCURLinfo'CString
  , CCURLinfo'CDouble
  , CCURLinfo'CLong
  , CCURLinfo'SList
  , CCURLinfo'CertI
  , CCURL_closepolicy
  , CCURL_lock_data
  , CCURL_lock_access
  , CCURLversion
  , CCURLSHcode
  , CCURLSHoption

  -- struct types from "curl.h"
  , CCURL_httppost (..)
  , CCURL_fileinfo (..)
  , CCURL_sockaddr (..)
  , CCURL_khkey (..)
  , CCURL_forms (..)
  , CCURL_slist (..)
  , CCURL_certinfo (..)
  , CCURL_version_info_data (..)

  ) where

import Network.Curlhs.FFI.Functions
import Network.Curlhs.FFI.Callbacks
import Network.Curlhs.FFI.Symbols
import Network.Curlhs.FFI.Types

