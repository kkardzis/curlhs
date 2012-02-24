-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.FFI.Callbacks
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Curlhs.FFI.Callbacks where

import Foreign.C.Types (CChar, CSize)
import Foreign.Ptr     (Ptr, FunPtr)

--import Network.Curlhs.FFI.Types


-------------------------------------------------------------------------------
-- from "curl.h"
-------------------------------------------------------------------------------
--type CCURL_progress_callback
--  = Ptr a -> CDouble -> CDouble -> CDouble -> CDouble -> IO CInt

type CCURL_write_callback
  = Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall "wrapper"
  wrap_ccurl_write_callback
    :: CCURL_write_callback
    -> IO (FunPtr CCURL_write_callback)


--type CCURL_chunk_bgn_callback
--  = Ptr a -> Ptr a -> CInt -> IO CLong

--type CCURL_chunk_end_callback
--  = Ptr a -> IO CLong

--type CCURL_fnmatch_callback
--  = Ptr a -> Ptr CChar -> Ptr CChar -> IO CInt

--type CCURL_seek_callback
--  = Ptr a -> CCURL_off_t -> CInt -> IO CInt

type CCURL_read_callback
  = Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall "wrapper"
  wrap_ccurl_read_callback
    :: CCURL_read_callback
    -> IO (FunPtr CCURL_read_callback)


--type CCURL_sockopt_callback
--  = Ptr a -> CCURL_socket_t -> CCURLsocktype -> IO CInt

--type CCURL_opensocket_callback
--  = Ptr a -> CCURLsocktype -> Ptr CCURL_sockaddr -> IO CCURL_socket_t

--type CCURL_closesocket_callback
--  = Ptr a -> CCURL_socket_t -> IO CInt

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

--type CCURL_debug_callback
--  = Ptr CCURL -> CCURL_infotype -> Ptr CChar -> CSize -> Ptr a -> IO CInt

--type CCURL_conv_callback
--  = Ptr CChar -> CSize -> IO CCURLcode

--type CCURL_ssl_ctx_callback
--  = Ptr CCURL -> Ptr a -> Ptr a -> IO CCURLcode

--type CCURL_sshkey_callback
--  = Ptr CCURL -> Ptr CCURL_khkey -> Ptr CCURL_khkey
--    -> CCURL_khmatch -> Ptr a -> IO CInt

--type CCURL_formget_callback
--  = Ptr a -> Ptr CChar -> CSize -> IO CSize

--type CCURL_lock_function
--  = Ptr CCURL -> CCURL_lock_data -> CCURL_lock_access -> Ptr a -> IO ()

--type CCURL_unlock_function
--  = Ptr CCURL -> CCURL_lock_data -> Ptr a -> IO ()

