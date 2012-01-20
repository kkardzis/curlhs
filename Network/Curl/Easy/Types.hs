-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curl.Easy.Types
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Network.Curl.Easy.Types
  ( PeekCCURL (..), FromC (..), FromH (..), CURL
  , CURLversion (..), CURL_version_info_data (..)
  , CURL_slist
  ) where

import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Data.Tuple (swap)
import Data.Bits

import Network.Curl.FFI.Easy.Types
import Network.Curl.FFI.Easy.Symbols


-------------------------------------------------------------------------------
class PeekCCURL c h where
  peekCCURL :: Ptr c -> IO h

class FromC c h where
  fromC :: c -> h

class FromH h c where
  fromH :: h -> c


-------------------------------------------------------------------------------
type CURL_slist = ()
type CURL = Ptr CCURL


-------------------------------------------------------------------------------
data CURLversion
  = CURLVERSION_FIRST
  | CURLVERSION_SECOND
  | CURLVERSION_THIRD
  | CURLVERSION_FOURTH
  | CURLVERSION_NOW
  deriving (Eq, Show)

instance FromC CCURLversion CURLversion where
  fromC x = findWithDef (cError "CCURLversion") x $ map swap knownCURLversion

instance FromH CURLversion CCURLversion where
  fromH x = findWithDef (hError "CURLversion") x knownCURLversion

knownCURLversion :: [(CURLversion, CCURLversion)]
knownCURLversion =
  [ (CURLVERSION_FIRST , cURLVERSION_FIRST )
  , (CURLVERSION_SECOND, cURLVERSION_SECOND)
  , (CURLVERSION_THIRD , cURLVERSION_THIRD )
  , (CURLVERSION_FOURTH, cURLVERSION_FOURTH)
  , (CURLVERSION_NOW   , cURLVERSION_NOW   )
  ]


-------------------------------------------------------------------------------
data CURL_version_info_data = CURL_version_info_data
  { curl_version_info_data_age             :: CURLversion
  , curl_version_info_data_version         :: String
  , curl_version_info_data_version_num     :: (Int, Int, Int)
  , curl_version_info_data_host            :: String
  , curl_version_info_data_features        :: Int
  , curl_version_info_data_ssl_version     :: Maybe String
  , curl_version_info_data_ssl_version_num :: Int
  , curl_version_info_data_libz_version    :: Maybe String
  , curl_version_info_data_protocols       :: [String]
  , curl_version_info_data_ares            :: Maybe String
  , curl_version_info_data_ares_num        :: Int
  , curl_version_info_data_libidn          :: Maybe String
  , curl_version_info_data_iconv_ver_num   :: Int
  , curl_version_info_data_libssh_version  :: Maybe String
  } deriving (Show)

instance PeekCCURL CCURL_version_info_data CURL_version_info_data where
  peekCCURL ptr = peek ptr >>= \cval -> CURL_version_info_data
    <$> (return $ fromC   $ ccurl_version_info_data_age             cval)
    <*> (peekCString      $ ccurl_version_info_data_version         cval)
    <*> (peekCVersionNum  $ ccurl_version_info_data_version_num     cval)
    <*> (peekCString      $ ccurl_version_info_data_host            cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_features        cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_ssl_version     cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_ssl_version_num cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libz_version    cval)
    <*> (peekCStringList  $ ccurl_version_info_data_protocols       cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_ares            cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_ares_num        cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libidn          cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_iconv_ver_num   cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libssh_version  cval)

peekCVersionNum :: CUInt -> IO (Int, Int, Int)
peekCVersionNum num = return (major, minor, patch)
  where
    major = fromIntegral $ shiftR (num .&. 0xff0000) 16
    minor = fromIntegral $ shiftR (num .&. 0x00ff00) 8
    patch = fromIntegral $ shiftR (num .&. 0x0000ff) 0

peekCStringList :: Ptr CString -> IO [String]
peekCStringList ptr = do
  cstring <- peek ptr
  if (cstring == nullPtr)
    then return []
    else do
      strings <- peekCStringList (plusPtr ptr (sizeOf ptr))
      string  <- peekCString cstring
      return (string : strings)

peekCStringMaybe :: CString -> IO (Maybe String)
peekCStringMaybe ptr =
  if (ptr == nullPtr)
    then return Nothing
    else peekCString ptr >>= return . Just

peekCIntegral :: (Num h, Integral c) => c -> IO h
peekCIntegral = return . fromIntegral



-------------------------------------------------------------------------------
findWithDef :: (Eq a) => b -> a -> [(a, b)] -> b
findWithDef def key table = maybe def id $ lookup key table

cError s = error $ "FromC "++s++": bad argument"
hError s = error $ "FromH "++s++": incomplete lookup table"

