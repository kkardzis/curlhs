-------------------------------------------------------------------------------
-- |
-- Module      :  Network.CURL000.LibLD
-- Copyright   :  Copyright © 2012-2013 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.CURL000.LibLD
  ( CURLAPI(..), RTLD(..)
  ) where

import Network.CURL000.LibCC (curlADRTAB, curlSYMTAB, curlTABLEN)
import Network.CURL000.LibHS (curl_global_init, curl_global_cleanup)
import Network.CURL000.LibHS (curl_version)

import Data.List (stripPrefix)

import Control.Concurrent (MVar, newMVar)

import System.IO.Unsafe (unsafePerformIO)

import System.Info (os)
import System.RTLD


-------------------------------------------------------------------------------
data CURLAPI 
  = CURL720 | CURL721 | CURL722 | CURL723 | CURL724
  | CURL725 | CURL726 | CURL727 | CURL728 | CURL729
  | CURL730
  deriving (Eq, Ord, Enum, Bounded)

instance RTLD CURLAPI where
  loadlib x = rtload curlRTSO x
  freelib _ = rtfree curlRTSO


-------------------------------------------------------------------------------
curlGlobalState :: MVar (Maybe (CURLAPI, LIBH, Int))
curlGlobalState = unsafePerformIO (newMVar Nothing)
{-# NOINLINE curlGlobalState #-}

curlRTSO :: RTSO CURLAPI
curlRTSO = RTSO
  { rtPKGMVAR = curlGlobalState
  , rtPKGNAME = "<curlhs>"
  , rtLIBNAME = libname
  , rtSONAMES = sonames
  , rtONLOAD  = const curl_global_init
  , rtONFREE  = const curl_global_cleanup
  , rtGETAPI  = const (fmap readapi curl_version)
  , rtSYMTAB  = curlSYMTAB
  , rtADRTAB  = curlADRTAB
  , rtTABLEN  = curlTABLEN
  }


-------------------------------------------------------------------------------
libname :: CURLAPI -> String
libname CURL720 = "libcurl 7.20.0"
libname CURL721 = "libcurl 7.21.0"
libname CURL722 = "libcurl 7.22.0"
libname CURL723 = "libcurl 7.23.0"
libname CURL724 = "libcurl 7.24.0"
libname CURL725 = "libcurl 7.25.0"
libname CURL726 = "libcurl 7.26.0"
libname CURL727 = "libcurl 7.27.0"
libname CURL728 = "libcurl 7.28.0"
libname CURL729 = "libcurl 7.29.0"
libname CURL730 = "libcurl 7.30.0"

readapi :: String -> Maybe CURLAPI
readapi xs = do
  num <- fmap (takeWhile (/='.')) (stripPrefix "libcurl/7." xs)
  case num of
    "20" -> Just CURL720
    "21" -> Just CURL721
    "22" -> Just CURL722
    "23" -> Just CURL723
    "24" -> Just CURL724
    "25" -> Just CURL725
    "26" -> Just CURL726
    "27" -> Just CURL727
    "28" -> Just CURL728
    "29" -> Just CURL729
    "30" -> Just CURL730
    _    -> Nothing


-------------------------------------------------------------------------------
sonames :: CURLAPI -> [String]
sonames
  | os == "mingw32" = winsonames
  | os == "darwin"  = osxsonames
  | os == "linux"   = gnusonames
  | os == "freebsd" = bsdsonames
  | otherwise = const []

winsonames :: CURLAPI -> [String]
winsonames _ = ["libcurl.dll"]

osxsonames :: CURLAPI -> [String]
osxsonames _ = ["libcurl.dylib"]

gnusonames :: CURLAPI -> [String]
gnusonames _ = ["libcurl.so.4", "libcurl.so"]

bsdsonames :: CURLAPI -> [String]
bsdsonames _ = ["libcurl.so"]

