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

import Foreign.C.Types
import Foreign.Ptr

import Network.Curl.FFI.Easy.Types


-------------------------------------------------------------------------------
-- from "curl.h"
-------------------------------------------------------------------------------
foreign import ccall "curl_strequal"   -- deprecated
  ccurl_strequal
    :: Ptr CChar
    -> Ptr CChar
    -> IO CInt

foreign import ccall "curl_strnequal"  -- deprecated
  ccurl_strnequal
    :: Ptr CChar
    -> Ptr CChar
    -> CSize
    -> IO CInt

{-
foreign import ccall "curl_formadd"
  ccurl_formadd
    :: Ptr (Ptr CCURL_httppost)
    -> Ptr (Ptr CCURL_httppost)
    -> CCURLformoption  -- check!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    -> Ptr a            -- check!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    -> IO CCURLformcode
-}

{-
foreign import ccall "curl_formget"
  ccurl_formget
    :: Ptr CCURL_httppost
    -> Ptr a
    -> FunPtr CCURL_formget_callback
    -> IO CInt
-}

{-
foreign import ccall "curl_formfree"
  ccurl_formfree
    :: Ptr CCURL_httppost
    -> IO ()
-}

foreign import ccall "curl_getenv"  -- deprecated
  ccurl_getenv
    :: Ptr CChar
    -> IO (Ptr CChar)

foreign import ccall "curl_version"
  ccurl_version
    :: IO (Ptr CChar)

foreign import ccall "curl_easy_escape"
  ccurl_easy_escape
    :: Ptr CCURL
    -> Ptr CChar
    -> CInt
    -> IO (Ptr CChar)

foreign import ccall "curl_escape"
  ccurl_escape
    :: Ptr CChar
    -> CInt
    -> IO (Ptr CChar)

foreign import ccall "curl_easy_unescape"
  ccurl_easy_unescape
    :: Ptr CCURL
    -> Ptr CChar
    -> CInt
    -> Ptr CInt
    -> IO (Ptr CChar)

foreign import ccall "curl_unescape"
  ccurl_unescape
    :: Ptr CChar
    -> CInt
    -> IO (Ptr CChar)

foreign import ccall "curl_free"
  ccurl_free
    :: Ptr a
    -> IO ()

foreign import ccall "curl_global_init"
  ccurl_global_init
    :: CLong
    -> IO CCURLcode

{-
foreign import ccall "curl_global_init_mem"
  ccurl_global_init_mem
    :: CLong
    -> FunPtr CCURL_malloc_callback
    -> FunPtr CCURL_free_callback
    -> FunPtr CCURL_realloc_callback
    -> FunPtr CCURL_strdup_callback
    -> FunPtr CCURL_calloc_callback
    -> IO CCURLcode
-}

foreign import ccall "curl_global_cleanup"
  ccurl_global_cleanup
    :: IO ()

foreign import ccall "curl_slist_append"
  ccurl_slist_append
    :: Ptr CCURL_slist
    -> Ptr CChar
    -> IO (Ptr CCURL_slist)

foreign import ccall "curl_slist_free_all"
  ccurl_slist_free_all
    :: Ptr CCURL_slist
    -> IO ()

foreign import ccall "curl_getdate"
  ccurl_getdate
    :: Ptr CChar
    -> Ptr CTime
    -> IO CTime

--foreign import ccall "curl_share_init"
--foreign import ccall "curl_share_setopt"
--foreign import ccall "curl_share_cleanup"

foreign import ccall "curl_version_info"
  ccurl_version_info
    :: CCURLversion
    -> IO (Ptr CCURL_version_info_data)

foreign import ccall "curl_easy_strerror"
  ccurl_easy_strerror
    :: CCURLcode
    -> IO (Ptr CChar)

--foreign import ccall "curl_share_strerror"

foreign import ccall "curl_easy_pause"
  ccurl_easy_pause
    :: Ptr CCURL
    -> CInt
    -> IO CCURLcode


-------------------------------------------------------------------------------
-- from "easy.h"
-------------------------------------------------------------------------------
foreign import ccall "curl_easy_init"
  ccurl_easy_init
    :: IO (Ptr CCURL)

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt_long
    :: Ptr CCURL
    -> CCURLoption
    -> CLong
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt_ptr
    :: Ptr CCURL
    -> CCURLoption
    -> Ptr a
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt_fptr
    :: Ptr CCURL
    -> CCURLoption
    -> FunPtr a
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt_off_t
    :: Ptr CCURL
    -> CCURLoption
    -> CCURL_off_t
    -> IO CCURLcode

foreign import ccall "curl_easy_perform"
  ccurl_easy_perform
    :: Ptr CCURL
    -> IO CCURLcode

foreign import ccall "curl_easy_cleanup"
  ccurl_easy_cleanup
    :: Ptr CCURL
    -> IO ()

foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo
    :: Ptr CCURL
    -> CCURLinfo
    -> Ptr a
    -> IO CCURLcode

foreign import ccall "curl_easy_duphandle"
  ccurl_easy_duphandle
    :: Ptr CCURL
    -> IO (Ptr CCURL)

foreign import ccall "curl_easy_reset"
  ccurl_easy_reset
    :: Ptr CCURL
    -> IO ()

foreign import ccall "curl_easy_recv"
  ccurl_easy_recv
    :: Ptr CCURL
    -> Ptr a
    -> CSize
    -> Ptr CSize
    -> IO CCURLcode

foreign import ccall "curl_easy_send"
  ccurl_easy_send
    :: Ptr CCURL
    -> Ptr a
    -> CSize
    -> Ptr CSize
    -> IO CCURLcode

