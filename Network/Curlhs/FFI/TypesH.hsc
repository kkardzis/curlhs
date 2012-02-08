-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.FFI.TypesH
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE EmptyDataDecls #-}

module Network.Curlhs.FFI.TypesH where

import Foreign.C.Types (CChar, CInt, CUInt, CLong, CLLong, CSize, CTime)
import Foreign.Storable (Storable (..))
import Foreign.Ptr (Ptr, castPtr)

import Control.Applicative ((<$>), (<*>))


#{include "curl/curl.h"}
#{let alignof type = "(%ld)", (long) offsetof (struct {char x; type y;}, y)}


-------------------------------------------------------------------------------
-- from "curlbuild.h"
-------------------------------------------------------------------------------
type CCURL_socket_t = CInt    -- or SOCKET?? -- TODO
type CCURL_off_t    = CLLong  -- ??


-------------------------------------------------------------------------------
-- simple types from "curl.h"
-------------------------------------------------------------------------------
data CCURL
data CCURLSH

newtype CCURLfiletype      = CCURLfiletype      CInt deriving (Eq, Show)
newtype CCURLsocktype      = CCURLsocktype      CInt deriving (Eq, Show)
newtype CCURLioerr         = CCURLioerr         CInt deriving (Eq, Show)
newtype CCURLiocmd         = CCURLiocmd         CInt deriving (Eq, Show)
newtype CCURL_infotype     = CCURL_infotype     CInt deriving (Eq, Show)
newtype CCURLcode          = CCURLcode          CInt deriving (Eq, Show)
newtype CCURL_proxytype    = CCURL_proxytype    CInt deriving (Eq, Show)
newtype CCURL_khtype       = CCURL_khtype       CInt deriving (Eq, Show)
newtype CCURL_khstat       = CCURL_khstat       CInt deriving (Eq, Show)
newtype CCURL_khmatch      = CCURL_khmatch      CInt deriving (Eq, Show)
newtype CCURL_usessl       = CCURL_usessl       CInt deriving (Eq, Show)
newtype CCURL_ftpccc       = CCURL_ftpccc       CInt deriving (Eq, Show)
newtype CCURL_ftpauth      = CCURL_ftpauth      CInt deriving (Eq, Show)
newtype CCURL_ftpcreatedir = CCURL_ftpcreatedir CInt deriving (Eq, Show)
newtype CCURL_ftpmethod    = CCURL_ftpmethod    CInt deriving (Eq, Show)
newtype CCURLoption'Int32  = CCURLoption'Int32  CInt deriving (Eq, Show)
newtype CCURLoption'Int64  = CCURLoption'Int64  CInt deriving (Eq, Show)
newtype CCURLoption'String = CCURLoption'String CInt deriving (Eq, Show)
newtype CCURLoption'SList  = CCURLoption'SList  CInt deriving (Eq, Show)
newtype CCURLoption'HTTPP  = CCURLoption'HTTPP  CInt deriving (Eq, Show)
newtype CCURLoption'File   = CCURLoption'File   CInt deriving (Eq, Show)
newtype CCURLoption'Share  = CCURLoption'Share  CInt deriving (Eq, Show)
newtype CCURLoption'Ptr_a  = CCURLoption'Ptr_a  CInt deriving (Eq, Show)
newtype CCURLoption'FunPtr = CCURLoption'FunPtr CInt deriving (Eq, Show)
newtype CCURL_http_version = CCURL_http_version CInt deriving (Eq, Show)
newtype CCURL_rtspreq      = CCURL_rtspreq      CInt deriving (Eq, Show)
newtype CCURL_netrc_option = CCURL_netrc_option CInt deriving (Eq, Show)
newtype CCURL_sslversion   = CCURL_sslversion   CInt deriving (Eq, Show)
newtype CCURL_tlsauth      = CCURL_tlsauth      CInt deriving (Eq, Show)
newtype CCURL_timecond     = CCURL_timecond     CInt deriving (Eq, Show)
newtype CCURLformoption    = CCURLformoption    CInt deriving (Eq, Show)
newtype CCURLformcode      = CCURLformcode      CInt deriving (Eq, Show)
newtype CCURLinfo'S        = CCURLinfo'S        CInt deriving (Eq, Show)
newtype CCURLinfo'I        = CCURLinfo'I        CInt deriving (Eq, Show)
newtype CCURLinfo'D        = CCURLinfo'D        CInt deriving (Eq, Show)
newtype CCURLinfo'L        = CCURLinfo'L        CInt deriving (Eq, Show)
newtype CCURL_closepolicy  = CCURL_closepolicy  CInt deriving (Eq, Show)
newtype CCURL_lock_data    = CCURL_lock_data    CInt deriving (Eq, Show)
newtype CCURL_lock_access  = CCURL_lock_access  CInt deriving (Eq, Show)
newtype CCURLversion       = CCURLversion       CInt deriving (Eq, Show)
newtype CCURLSHcode        = CCURLSHcode        CInt deriving (Eq, Show)
newtype CCURLSHoption      = CCURLSHoption      CInt deriving (Eq, Show)

instance Storable CCURLversion where
  sizeOf _    = #{size    CURLversion}
  alignment _ = #{alignof CURLversion}
  poke _ _    = undefined
  peek ptr    = CCURLversion <$> peek (castPtr ptr)


-------------------------------------------------------------------------------
-- struct types from "curl.h"
-------------------------------------------------------------------------------
data CCURL_httppost = CCURL_httppost
  { ccurl_httppost_next           :: Ptr CCURL_httppost
  , ccurl_httppost_name           :: Ptr CChar
  , ccurl_httppost_namelength     :: CLong
  , ccurl_httppost_contents       :: Ptr CChar
  , ccurl_httppost_contentslength :: CLong
  , ccurl_httppost_buffer         :: Ptr CChar
  , ccurl_httppost_bufferlength   :: CLong
  , ccurl_httppost_contenttype    :: Ptr CChar
  , ccurl_httppost_contentheader  :: Ptr CCURL_slist
  , ccurl_httppost_more           :: Ptr CCURL_httppost
  , ccurl_httppost_flags          :: CLong
  , ccurl_httppost_showfilename   :: Ptr CChar
  , ccurl_httppost_userp          :: Ptr ()  -- Ptr a -- TODO!!!
  } deriving (Show)

instance Storable CCURL_httppost where
  sizeOf _    = #{size    struct curl_httppost}
  alignment _ = #{alignof struct curl_httppost}
  poke _ _    = undefined
  peek _      = undefined


-------------------------------------------------------------------------------
data CCURL_fileinfo = CCURL_fileinfo
  { ccurl_fileinfo_filename       :: Ptr CChar
  , ccurl_fileinfo_filetype       :: CCURLfiletype
  , ccurl_fileinfo_time           :: CTime
  , ccurl_fileinfo_perm           :: CUInt
  , ccurl_fileinfo_uid            :: CInt
  , ccurl_fileinfo_gid            :: CInt
  , ccurl_fileinfo_size           :: CCURL_off_t
  , ccurl_fileinfo_hardlinks      :: CLong
  , ccurl_fileinfo_strings_time   :: Ptr CChar
  , ccurl_fileinfo_strings_perm   :: Ptr CChar
  , ccurl_fileinfo_strings_user   :: Ptr CChar
  , ccurl_fileinfo_strings_group  :: Ptr CChar
  , ccurl_fileinfo_strings_target :: Ptr CChar
  , ccurl_fileinfo_flags          :: CUInt
  , ccurl_fileinfo_b_data         :: Ptr CChar
  , ccurl_fileinfo_b_size         :: CSize
  , ccurl_fileinfo_b_used         :: CSize
  } deriving (Show)

instance Storable CCURL_fileinfo where
  sizeOf _    = #{size    struct curl_fileinfo}
  alignment _ = #{alignof struct curl_fileinfo}
  poke _ _    = undefined
  peek _      = undefined


-------------------------------------------------------------------------------
data CCURL_sockaddr = CCURL_sockaddr
  { ccurl_sockaddr_family   :: CInt
  , ccurl_sockaddr_socktype :: CInt
  , ccurl_sockaddr_protocol :: CInt
  , ccurl_sockaddr_addrlen  :: CUInt
--  , ccurl_sockaddr_addr     :: CSockaddr -- ?? TODO
  } deriving (Show)

instance Storable CCURL_sockaddr where
  sizeOf _    = #{size    struct curl_sockaddr}
  alignment _ = #{alignof struct curl_sockaddr}
  poke _ _    = undefined
  peek _      = undefined


-------------------------------------------------------------------------------
data CCURL_khkey = CCURL_khkey
  { ccurl_khkey_key     :: Ptr CChar
  , ccurl_khkey_len     :: CSize
  , ccurl_khkey_keytype :: CCURL_khtype
  } deriving (Show)

instance Storable CCURL_khkey where
  sizeOf _    = #{size    struct curl_khkey}
  alignment _ = #{alignof struct curl_khkey}
  poke _ _    = undefined
  peek _      = undefined


-------------------------------------------------------------------------------
data CCURL_forms = CCURL_forms
  { ccurl_forms_option :: CCURLformoption
  , ccurl_forms_value  :: Ptr CChar
  } deriving (Show)

instance Storable CCURL_forms where
  sizeOf _    = #{size    struct curl_forms}
  alignment _ = #{alignof struct curl_forms}
  poke _ _    = undefined
  peek _      = undefined


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
data CCURL_certinfo = CCURL_certinfo
  { ccurl_certinfo_num_of_certs :: CInt
  , ccurl_certinfo_certinfo     :: Ptr (Ptr CCURL_slist)
  } deriving (Show)

instance Storable CCURL_certinfo where
  sizeOf _    = #{size    struct curl_certinfo}
  alignment _ = #{alignof struct curl_certinfo}
  poke _ _    = undefined
  peek _      = undefined


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

