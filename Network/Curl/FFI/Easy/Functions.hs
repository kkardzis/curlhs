-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curl.FFI.Easy.Functions
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Curl.FFI.Easy.Functions where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Network.Curl.FFI.Easy.Types


-------------------------------------------------------------------------------
foreign import ccall curl_version :: IO CString

foreign import ccall curl_version_info
  :: CCURLversion -> IO (Ptr CCURL_version_info_data)


-------------------------------------------------------------------------------
foreign import ccall curl_global_init :: CLong -> IO CCURLcode

foreign import ccall curl_global_cleanup :: IO ()

--foreign import ccall curl_global_init_mem
--  :: CLong -> CCURL_malloc_callback -> CCURL_free_callback
--  -> CCURL_realloc_callback -> CCURL_strdup_callback
--  -> CCURL_calloc_callback -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall curl_easy_init :: IO CCURL

foreign import ccall curl_easy_cleanup :: CCURL -> IO ()

foreign import ccall curl_easy_duphandle :: CCURL -> IO CCURL

foreign import ccall curl_easy_perform :: CCURL -> IO CCURLcode

foreign import ccall curl_easy_pause :: CCURL -> CInt -> IO CCURLcode

foreign import ccall curl_easy_reset :: CCURL -> IO ()

foreign import ccall curl_easy_getinfo
  :: CCURL -> CCURLinfo -> Ptr a -> IO CCURLcode

foreign import ccall "curl_easy_setopt" curl_easy_setopt_long
  :: CCURL -> CCURLoption -> CLong -> IO CCURLcode

foreign import ccall "curl_easy_setopt" curl_easy_setopt_ptr
  :: CCURL -> CCURLoption -> Ptr a -> IO CCURLcode

foreign import ccall "curl_easy_setopt" curl_easy_setopt_fptr
  :: CCURL -> CCURLoption -> FunPtr a -> IO CCURLcode

foreign import ccall "curl_easy_setopt" curl_easy_setopt_off_t
  :: CCURL -> CCURLoption -> CCURL_off_t -> IO CCURLcode

foreign import ccall curl_easy_recv
  :: CCURL -> Ptr a -> CSize -> Ptr CSize -> IO CCURLcode

foreign import ccall curl_easy_send
  :: CCURL -> Ptr a -> CSize -> Ptr CSize -> IO CCURLcode

foreign import ccall curl_easy_strerror :: CCURLcode -> IO CString


-------------------------------------------------------------------------------
foreign import ccall curl_getdate :: CString -> Ptr CTime -> IO CTime

foreign import ccall curl_free :: CString -> IO ()

--foreign import ccall curl_easy_escape
--foreign import ccall curl_easy_unescape

--foreign import ccall curl_escape
--foreign import ccall curl_unescape

--foreign import ccall curl_formadd
--foreign import ccall curl_formfree

--foreign import ccall curl_slist_append
--foreign import ccall curl_slist_free_all

