-------------------------------------------------------------------------------
-- |
-- Module      :  Network.CURL000.LibCC
-- Copyright   :  Copyright © 2012-2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}

module Network.CURL000.LibCC
  ( curlADRTAB
  , curlSYMTAB
  , curlTABLEN

  , curl_easy_cleanup
  , curl_easy_duphandle
  , curl_easy_escape
  , curl_easy_getinfo
  , curl_easy_init
  , curl_easy_pause
  , curl_easy_perform
  , curl_easy_recv
  , curl_easy_reset
  , curl_easy_send
  , curl_easy_setopt'Long
  , curl_easy_setopt'COff
  , curl_easy_setopt'DPtr
  , curl_easy_setopt'FPtr
  , curl_easy_strerror
  , curl_easy_unescape
 -- curl_escape
  , curl_formadd
  , curl_formfree
  , curl_formget
  , curl_free
  , curl_getdate
 -- curl_getenv
  , curl_global_cleanup
  , curl_global_init
  , curl_global_init_mem
 -- curl_maprintf
 -- curl_mfprintf
 -- curl_mprintf
 -- curl_msnprintf
 -- curl_msprintf
  , curl_multi_add_handle
  , curl_multi_assign
  , curl_multi_cleanup
  , curl_multi_fdset
  , curl_multi_info_read
  , curl_multi_init
  , curl_multi_perform
  , curl_multi_remove_handle
  , curl_multi_setopt
  , curl_multi_socket
  , curl_multi_socket_action
  , curl_multi_socket_all
  , curl_multi_strerror
  , curl_multi_timeout
  , curl_multi_wait
 -- curl_mvaprintf
 -- curl_mvfprintf
 -- curl_mvprintf
 -- curl_mvsnprintf
 -- curl_mvsprintf
  , curl_share_cleanup
  , curl_share_init
  , curl_share_setopt'Long
  , curl_share_setopt'FPtr
  , curl_share_strerror
  , curl_slist_append
  , curl_slist_free_all
 -- curl_strequal
 -- curl_strnequal
 -- curl_unescape
  , curl_version
  , curl_version_info

  , CURL_write_callback, wrapCURL_write_callback
  , CURL_read_callback , wrapCURL_read_callback

  , CURL_lock_function  , wrapCURL_lock_function
  , CURL_unlock_function, wrapCURL_unlock_function

  , CURL, CURLM, CURLSH

  , CURLslist (..)
  , CURLcerts (..)

  , CURL_off_t

  ) where

import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (when)

import System.IO.Unsafe (unsafePerformIO)
import System.RTLD

#include "LibC0.c"


-------------------------------------------------------------------------------
foreign import ccall "&curlADRTAB" curlADRTAB :: Ptr (FunPtr ())
foreign import ccall "&curlSYMTAB" curlSYMTAB :: Ptr SYMTABENTRY

curlTABLEN :: Int
curlTABLEN = #{const TABLEN}

peekFP :: Int -> IO (FunPtr a)
peekFP fid = do
  fp <- peekElemOff curlADRTAB fid
  when (fp==nullFunPtr) (nullFP fid)
  return (castFunPtr fp)

nullFP :: Int -> IO ()
nullFP fid =
  let name = unsafePerformIO $
        peekElemOff curlSYMTAB fid >>= \(RTSYM (_,_,cs)) -> peekCString cs
  in  error $ concat ["<curlhs> failed to call '", name, "' (NULL)"]



-------------------------------------------------------------------------------
data CURL
data CURLM
data CURLSH

data CURLhttppost
data CURLvinfo
data CURLwaitfd
data CURLmsg

type CURLcode   = CInt
type CURLMcode  = CInt
type CURLSHcode = CInt

type CURLoption   = CInt
type CURLMoption  = CInt
type CURLSHopt = CInt

type CURL_lock_data   = CInt
type CURL_lock_access = CInt

type CURLinfo    = CInt
type CURLversion = CInt

type CURLformcode   = CInt
type CURLformoption = CInt

type CURLsocket = CInt

type CURL_off_t = CLLong  -- ??

type CURL_formget_callback = IO ()
type CURL_malloc_callback = IO ()
type CURL_free_callback = IO ()
type CURL_realloc_callback = IO ()
type CURL_calloc_callback = IO ()
type CURL_strdup_callback = IO ()


data CURLslist = CURLslist (Ptr CChar) (Ptr CURLslist)

instance Storable CURLslist where
  sizeOf _    = #{size    struct curl_slist}
  alignment _ = #{alignof struct curl_slist}
  poke _ _    = undefined
  peek ptr    = CURLslist
    <$> #{peek struct curl_slist, data} ptr
    <*> #{peek struct curl_slist, next} ptr


data CURLcerts = CURLcerts CInt (Ptr (Ptr CURLslist))

instance Storable CURLcerts where
  sizeOf _    = #{size    struct curl_certinfo}
  alignment _ = #{alignof struct curl_certinfo}
  poke _ _    = undefined
  peek ptr    = CURLcerts
    <$> #{peek struct curl_certinfo, num_of_certs} ptr
    <*> #{peek struct curl_certinfo, certinfo    } ptr


-------------------------------------------------------------------------------
#{SAFECALL curl_easy_cleanup \
, Ptr CURL {- ^ handle -}    \
, IO ()                      }

#{SAFECALL curl_easy_duphandle \
, Ptr CURL      {- ^ handle -} \
, IO (Ptr CURL) {- ^ handle -} }

#{SAFECALL curl_easy_escape                 \
, Ptr CURL       {- ^ handle             -} \
, Ptr CChar      {- ^ const string (url) -} \
, CInt           {- ^ length             -} \
, IO (Ptr CChar) {- ^ URL encoded string -} }

-- #{SAFECALL curl_easy_getinfo \
-- , Ptr CURL    {- ^ handle -} \
-- , CURLinfo    {- ^ info   -} \
-- , Ptr ()      {- ^ param  -} \
-- , IO CURLcode {- ^ status -} }

#{SAFECALL curl_easy_init      \
, IO (Ptr CURL) {- ^ handle -} }

#{SAFECALL curl_easy_pause    \
, Ptr CURL    {- ^ handle  -} \
, CInt        {- ^ bitmask -} \
, IO CURLcode {- ^ status  -} }

#{SAFECALL curl_easy_perform \
, Ptr CURL    {- ^ handle -} \
, IO CURLcode {- ^ status -} }

#{SAFECALL curl_easy_recv    \
, Ptr CURL    {- ^ handle -} \
, Ptr ()      {- ^ buffer -} \
, CSize       {- ^ buflen -} \
, Ptr CSize   {- ^ n      -} \
, IO CURLcode {- ^ status -} }

#{SAFECALL curl_easy_reset \
, Ptr CURL  {- ^ handle -} \
, IO ()                    }

#{SAFECALL curl_easy_send          \
, Ptr CURL    {- ^ handle       -} \
, Ptr ()      {- ^ const buffer -} \
, CSize       {- ^ buflen       -} \
, Ptr CSize   {- ^ n            -} \
, IO CURLcode {- ^ status       -} }

-- #{SAFECALL curl_easy_setopt  \
-- , Ptr CURL    {- ^ handle -} \
-- , CURLoption  {- ^ option -} \
-- , Ptr ()      {- ^ param  -} \
-- , IO CURLcode {- ^ status -} }

#{SAFECALL curl_easy_strerror         \
, CURLcode       {- ^ errornum     -} \
, IO (Ptr CChar) {- ^ const string -} }

#{SAFECALL curl_easy_unescape           \
, Ptr CURL       {- ^ handle         -} \
, Ptr CChar      {- ^ const string   -} \
, CInt           {- ^ inlength       -} \
, Ptr CInt       {- ^ outlength      -} \
, IO (Ptr CChar) {- ^ decoded string -} }


-------------------------------------------------------------------------------
#{SAFECALL curl_formadd                   \
, Ptr (Ptr CURLhttppost) {- ^ httppost -} \
, Ptr (Ptr CURLhttppost) {- ^ lastpost -} \
, CURLformoption         {- ^ option   -} \
, Ptr ()                 {- ^ value    -} \
, IO CURLformcode        {- ^ status   -} }

#{SAFECALL curl_formfree        \
, Ptr CURLhttppost {- ^ form -} \
, IO ()                         }

#{SAFECALL curl_formget                       \
, Ptr CURLhttppost             {- ^ form   -} \
, Ptr ()                       {- ^ userp  -} \
, FunPtr CURL_formget_callback {- ^ append -} \
, IO CInt                      {- ^ status -} }


-------------------------------------------------------------------------------
#{SAFECALL curl_free \
, Ptr () {- ^ ptr -} \
, IO ()              }


-------------------------------------------------------------------------------
#{SAFECALL curl_getdate                \
, Ptr CChar {- ^ const datestring   -} \
, Ptr CTime {- ^ const now (unused) -} \
, IO CTime  {- ^ time               -} }


-------------------------------------------------------------------------------
#{SAFECALL curl_global_cleanup \
, IO ()                        }

#{SAFECALL curl_global_init  \
, CLong       {- ^ flags  -} \
, IO CURLcode {- ^ status -} }

#{SAFECALL curl_global_init_mem               \
, CLong                        {- ^ flags  -} \
, FunPtr CURL_malloc_callback  {- ^ m      -} \
, FunPtr CURL_free_callback    {- ^ f      -} \
, FunPtr CURL_realloc_callback {- ^ r      -} \
, FunPtr CURL_strdup_callback  {- ^ s      -} \
, FunPtr CURL_calloc_callback  {- ^ c      -} \
, IO CURLcode                  {- ^ status -} }


-------------------------------------------------------------------------------
#{SAFECALL curl_multi_add_handle    \
, Ptr CURLM    {- ^ multi handle -} \
, Ptr CURL     {- ^ easy handle  -} \
, IO CURLMcode {- ^ multi status -} }

#{SAFECALL curl_multi_assign        \
, Ptr CURLM    {- ^ multi handle -} \
, CURLsocket   {- ^ sockfd       -} \
, Ptr ()       {- ^ sockptr      -} \
, IO CURLMcode {- ^ multi status -} }

#{SAFECALL curl_multi_cleanup       \
, Ptr CURLM    {- ^ multi handle -} \
, IO CURLMcode {- ^ multi status -} }

#{SAFECALL curl_multi_fdset         \
, Ptr CURLM    {- ^ multi handle -} \
, Ptr ()       {- ^ read_fd_set  -} \
, Ptr ()       {- ^ write_fd_set -} \
, Ptr ()       {- ^ exc_fd_set   -} \
, Ptr CInt     {- ^ max_fd       -} \
, IO CURLMcode {- ^ multi status -} }

#{SAFECALL curl_multi_info_read          \
, Ptr CURLM        {- ^ multi handle  -} \
, Ptr CInt         {- ^ msgs_in_queue -} \
, IO (Ptr CURLmsg) {- ^ message       -} }

#{SAFECALL curl_multi_init            \
, IO (Ptr CURLM) {- ^ multi handle -} }

#{SAFECALL curl_multi_perform          \
, Ptr CURLM    {- ^ multi handle    -} \
, Ptr CInt     {- ^ running handles -} \
, IO CURLMcode {- ^ multi status    -} }

#{SAFECALL curl_multi_remove_handle \
, Ptr CURLM    {- ^ multi handle -} \
, Ptr CURL     {- ^ easy handle  -} \
, IO CURLMcode {- ^ multi status -} }

#{SAFECALL curl_multi_setopt        \
, Ptr CURLM    {- ^ multi handle -} \
, CURLMoption  {- ^ option       -} \
, Ptr ()       {- ^ value        -} \
, IO CURLMcode {- ^ multi status -} }

#{SAFECALL curl_multi_socket           \
, Ptr CURLM    {- ^ multi handle    -} \
, CURLsocket   {- ^ sockfd          -} \
, Ptr CInt     {- ^ running handles -} \
, IO CURLMcode {- ^ multi status    -} }

#{SAFECALL curl_multi_socket_action    \
, Ptr CURLM    {- ^ multi handle    -} \
, CURLsocket   {- ^ sockfd          -} \
, CInt         {- ^ ev_bitmask      -} \
, Ptr CInt     {- ^ running handles -} \
, IO CURLMcode {- ^ multi status    -} }

#{SAFECALL curl_multi_socket_all       \
, Ptr CURLM    {- ^ multi handle    -} \
, Ptr CInt     {- ^ running handles -} \
, IO CURLMcode {- ^ multi status    -} }

#{SAFECALL curl_multi_strerror        \
, CURLMcode      {- ^ errornum     -} \
, IO (Ptr CChar) {- ^ const string -} }

#{SAFECALL curl_multi_timeout       \
, Ptr CURLM    {- ^ multi handle -} \
, Ptr CLong    {- ^ timeout      -} \
, IO CURLMcode {- ^ multi status -} }

#{SAFECALL curl_multi_wait            \
, Ptr CURLM      {- ^ multi handle -} \
, Ptr CURLwaitfd {- ^ extra_fds[]  -} \
, CUInt          {- ^ extra_nfds   -} \
, CInt           {- ^ timeout_ms   -} \
, Ptr CInt       {- ^ numfds       -} \
, IO CURLMcode   {- ^ multi status -} }


-------------------------------------------------------------------------------
#{SAFECALL curl_share_cleanup        \
, Ptr CURLSH    {- ^ share handle -} \
, IO CURLSHcode {- ^ share status -} }

#{SAFECALL curl_share_init             \
, IO (Ptr CURLSH) {- ^ share handle -} }

-- #{SAFECALL curl_share_setopt         \
-- , Ptr CURLSH    {- ^ share handle -} \
-- , CURLSHopt  {- ^ option       -} \
-- , Ptr ()        {- ^ value        -} \
-- , IO CURLSHcode {- ^ share status -} }

#{SAFECALL curl_share_strerror        \
, CURLSHcode     {- ^ errornum     -} \
, IO (Ptr CChar) {- ^ const string -} }


-------------------------------------------------------------------------------
#{SAFECALL curl_slist_append              \
, Ptr CURLslist      {- ^ list         -} \
, Ptr CChar          {- ^ const string -} \
, IO (Ptr CURLslist) {- ^ list         -} }

#{SAFECALL curl_slist_free_all \
, Ptr CURLslist {- ^ list -}   \
, IO ()                        }


-------------------------------------------------------------------------------
#{SAFECALL curl_version         \
, IO (Ptr CChar) {- ^ string -} }

#{SAFECALL curl_version_info   \
, CURLversion {- ^ type/age -} \
, IO (Ptr CURLvinfo)           }


-------------------------------------------------------------------------------
-- Callbacks
-------------------------------------------------------------------------------
#{WRAP CURL_write_callback, Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize}
#{WRAP CURL_read_callback , Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize}


#{WRAP CURL_lock_function                                          \
, Ptr CURL -> CURL_lock_data -> CURL_lock_access -> Ptr () -> IO ()}

#{WRAP CURL_unlock_function                    \
, Ptr CURL -> CURL_lock_data -> Ptr () -> IO ()}


-------------------------------------------------------------------------------
-- Wrappers for variadic functions
-------------------------------------------------------------------------------
{-# NOINLINE curl_easy_getinfo #-}
curl_easy_getinfo :: Ptr CURL -> CURLinfo -> Ptr () -> IO CURLcode
curl_easy_getinfo a b c =
  peekFP #{FPID curl_easy_getinfo} >>= \fp -> curlOptDPtr fp (castPtr a) b c


{-# NOINLINE curl_easy_setopt'Long #-}
curl_easy_setopt'Long :: Ptr CURL -> CURLoption -> CLong -> IO CURLcode
curl_easy_setopt'Long a b c =
  peekFP #{FPID curl_easy_setopt} >>= \fp -> curlOptLong fp (castPtr a) b c

{-# NOINLINE curl_easy_setopt'COff #-}
curl_easy_setopt'COff :: Ptr CURL -> CURLoption -> CURL_off_t -> IO CURLcode
curl_easy_setopt'COff a b c =
  peekFP #{FPID curl_easy_setopt} >>= \fp -> curlOptCOff fp (castPtr a) b c

{-# NOINLINE curl_easy_setopt'DPtr #-}
curl_easy_setopt'DPtr :: Ptr CURL -> CURLoption -> Ptr () -> IO CURLcode
curl_easy_setopt'DPtr a b c =
  peekFP #{FPID curl_easy_setopt} >>= \fp -> curlOptDPtr fp (castPtr a) b c

{-# NOINLINE curl_easy_setopt'FPtr #-}
curl_easy_setopt'FPtr :: Ptr CURL -> CURLoption -> FunPtr () -> IO CURLcode
curl_easy_setopt'FPtr a b c =
  peekFP #{FPID curl_easy_setopt} >>= \fp -> curlOptFPtr fp (castPtr a) b c


{-# NOINLINE curl_share_setopt'Long #-}
curl_share_setopt'Long :: Ptr CURLSH -> CURLSHopt -> CLong -> IO CURLSHcode
curl_share_setopt'Long a b c =
  peekFP #{FPID curl_share_setopt} >>= \fp -> curlOptLong fp (castPtr a) b c

{-# NOINLINE curl_share_setopt'FPtr #-}
curl_share_setopt'FPtr :: Ptr CURLSH -> CURLSHopt -> FunPtr () -> IO CURLSHcode
curl_share_setopt'FPtr a b c =
  peekFP #{FPID curl_share_setopt} >>= \fp -> curlOptFPtr fp (castPtr a) b c


foreign import ccall safe curlOptLong
  :: FunPtr () -> Ptr () -> CInt -> CLong -> IO CInt

foreign import ccall safe curlOptCOff
  :: FunPtr () -> Ptr () -> CInt -> CURL_off_t -> IO CInt

foreign import ccall safe curlOptDPtr
  :: FunPtr () -> Ptr () -> CInt -> Ptr () -> IO CInt

foreign import ccall safe curlOptFPtr
  :: FunPtr () -> Ptr () -> CInt -> FunPtr () -> IO CInt

