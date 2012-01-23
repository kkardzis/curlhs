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
-- from "curlbuild.h"
-------------------------------------------------------------------------------
type CCURL_socket_t = CInt  -- or SOCKET?? -- TODO

type CCURL_off_t  = CLLong  -- ??


-------------------------------------------------------------------------------
-- from "curl.h"
-------------------------------------------------------------------------------
data CCURL

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
  peek ptr    = undefined

--type CCURL_progress_callback
--  = Ptr a -> CDouble -> CDouble -> CDouble -> CDouble -> IO CInt

--type CCURL_write_callback
--  = Ptr CChar -> CSize -> CSize -> Ptr a -> IO CSize

type CCURLfiletype = CInt

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
  peek ptr    = undefined

--type CCURL_chunk_bgn_callback
--  = Ptr a -> Ptr a -> CInt -> IO CLong

--type CCURL_chunk_end_callback
--  = Ptr a -> IO CLong

--type CCURL_fnmatch_callback
--  = Ptr a -> Ptr CChar -> Ptr CChar -> IO CInt

--type CCURL_seek_callback
--  = Ptr a -> CCURL_off_t -> CInt -> IO CInt

--type CCURL_read_callback
--  = Ptr CChar -> CSize -> CSize -> Ptr a -> IO CSize

type CCURLsocktype = CInt

--type CCURL_sockopt_callback
--  = Ptr a -> CCURL_socket_t -> CCURLsocktype -> IO CInt

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
  peek ptr    = undefined

--type CCURL_opensocket_callback
--  = Ptr a -> CCURLsocktype -> Ptr CCURL_sockaddr -> IO CCURL_socket_t

--type CCURL_closesocket_callback
--  = Ptr a -> CCURL_socket_t -> IO CInt

type CCURLioerr = CInt

type CCURLiocmd = CInt

--type CCURL_ioctl_callback
--  = Ptr CCURL -> CInt -> Ptr a -> IO CCURLioerr

--type CCURL_malloc_callback
--  = CSize -> IO (Ptr a)

--type CCURL_free_callback
--  = Ptr a -> IO ()

--type CCURL_realloc_callback
--  = Ptr a -> CSize -> IO (Ptr a)

--type CCURL_strdup_callback
--  = Ptr CChar -> IO (Ptr CChar)

--type CCURL_calloc_callback
--  = CSize -> CSize -> IO (Ptr a)

type CCURL_infotype = CInt

--type CCURL_debug_callback
--  = Ptr CCURL -> CCURL_infotype -> Ptr CChar -> CSize -> Ptr a -> IO CInt

type CCURLcode = CInt

--type CCURL_conv_callback
--  = Ptr CChar -> CSize -> IO CCURLcode

--type CCURL_ssl_ctx_callback
--  = Ptr CCURL -> Ptr a -> Ptr a -> IO CCURLcode

type CCURL_proxytype = CInt

type CCURL_khtype = CInt

data CCURL_khkey = CCURL_khkey
  { ccurl_khkey_key     :: Ptr CChar
  , ccurl_khkey_len     :: CSize
  , ccurl_khkey_keytype :: CCURL_khtype
  } deriving (Show)

instance Storable CCURL_khkey where
  sizeOf _    = #{size    struct curl_khkey}
  alignment _ = #{alignof struct curl_khkey}
  poke _ _    = undefined
  peek ptr    = undefined

type CCURL_khstat = CInt

type CCURL_khmatch = CInt

--type CCURL_sshkey_callback
--  = Ptr CCURL -> Ptr CCURL_khkey -> Ptr CCURL_khkey
--    -> CCURL_khmatch -> Ptr a -> IO CInt

type CCURL_usessl = CInt

type CCURL_ftpccc = CInt

type CCURL_ftpauth = CInt

type CCURL_ftpcreatedir = CInt

type CCURL_ftpmethod = CInt

type CCURLoption = CInt

--type CCURL_http_version = CInt -- ??
--type CCURL_rtspreq = CInt -- ??
--type CCURL_netrc_option = CInt -- ??
--type CCURL_sslversion = CInt -- ??
--type CCURL_tlsauth = CInt -- ??

type CCURL_timecond = CInt

type CCURLformoption = CInt

data CCURL_forms = CCURL_forms
  { ccurl_forms_option :: CCURLformoption
  , ccurl_forms_value  :: Ptr CChar
  } deriving (Show)

instance Storable CCURL_forms where
  sizeOf _    = #{size    struct curl_forms}
  alignment _ = #{alignof struct curl_forms}
  poke _ _    = undefined
  peek ptr    = undefined

type CCURLformcode = CInt

--type CCURL_formget_callback
--  = Ptr a -> Ptr CChar -> CSize -> IO CSize

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

data CCURL_certinfo = CCURL_certinfo
  { ccurl_certinfo_num_of_certs :: CInt
  , ccurl_certinfo_certinfo     :: Ptr (Ptr CCURL_slist)
  } deriving (Show)

instance Storable CCURL_certinfo where
  sizeOf _    = #{size    struct curl_certinfo}
  alignment _ = #{alignof struct curl_certinfo}
  poke _ _    = undefined
  peek ptr    = undefined

type CCURLinfo = CInt

type CCURL_closepolicy = CInt

type CCURL_lock_data = CInt

type CCURL_lock_access = CInt

--type CCURL_lock_function
--  = Ptr CCURL -> CCURL_lock_data -> CCURL_lock_access -> Ptr a -> IO ()

--type CCURL_unlock_function
--  = Ptr CCURL -> CCURL_lock_data -> Ptr a -> IO ()

data CCURLSH

type CCURLSHcode = CInt

type CCURLSHoption = CInt

type CCURLversion = CInt

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

