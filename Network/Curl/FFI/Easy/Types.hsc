-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curl.FFI.Easy.Types
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE EmptyDataDecls #-}

module Network.Curl.FFI.Easy.Types where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#{include "curl/curl.h"}
#{let alignof type = "(%ld)", (long) offsetof (struct {char x; type y;}, y)}


-------------------------------------------------------------------------------
data CCurl
type CCURL = Ptr CCurl

type CCURLcode    = CInt
type CCURLinfo    = CInt
type CCURLversion = CInt
type CCURLoption  = CInt
type CCURL_off_t  = CLLong


-------------------------------------------------------------------------------
data CCURL_slist = CCURL_slist
  { ccurl_slist_data :: CString
  , ccurl_slist_next :: Ptr CCURL_slist
  } deriving (Show)

instance Storable CCURL_slist where
  sizeOf _    = #{size    struct curl_slist}
  alignment _ = #{alignof struct curl_slist}
  peek ptr    = CCURL_slist
    <$> #{peek struct curl_slist, data} ptr
    <*> #{peek struct curl_slist, next} ptr
  poke ptr v  = do
    #{poke struct curl_slist, data} ptr $ ccurl_slist_data v
    #{poke struct curl_slist, next} ptr $ ccurl_slist_next v


-------------------------------------------------------------------------------
data CCURL_version_info_data = CCURL_version_info_data
  { ccurl_version_info_data_age             :: CCURLversion
  , ccurl_version_info_data_version         :: CString
  , ccurl_version_info_data_version_num     :: CUInt
  , ccurl_version_info_data_host            :: CString
  , ccurl_version_info_data_features        :: CInt
  , ccurl_version_info_data_ssl_version     :: CString
  , ccurl_version_info_data_ssl_version_num :: CLong
  , ccurl_version_info_data_libz_version    :: CString
  , ccurl_version_info_data_protocols       :: Ptr CString
  , ccurl_version_info_data_ares            :: CString
  , ccurl_version_info_data_ares_num        :: CInt
  , ccurl_version_info_data_libidn          :: CString
  , ccurl_version_info_data_iconv_ver_num   :: CInt
  , ccurl_version_info_data_libssh_version  :: CString
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

