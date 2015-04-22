-------------------------------------------------------------------------------
-- |
-- Module      :  Network.CURL000.LibLD
-- Copyright   :  Copyright (c) 2012-2015 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
--
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.CURL000.LibLD
  ( RTLD(..), LIBCURL(..)
  ) where

import Network.CURL000.LibCC (curlADRTAB, curlSYMTAB, curlTABLEN)
import Network.CURL000.LibHS (curl_global_init, curl_global_cleanup)
import Network.CURL000.LibHS (curl_version)

import Text.ParserCombinators.ReadP (ReadP, readP_to_S, string)
import Text.Read.Lex (readDecP)

import Control.Concurrent (MVar, newMVar)
import System.IO.Unsafe (unsafePerformIO)

import System.Info (os)
import System.RTLD


-------------------------------------------------------------------------------
data LIBCURL = CURL720 | CURL730
  deriving (Eq, Ord, Enum, Bounded)

instance RTLD LIBCURL where
  loadlib x = rtload curlRTSO x
  freelib _ = rtfree curlRTSO


-------------------------------------------------------------------------------
curlGlobalState :: MVar (Maybe (LIBCURL, LIBH, Int))
curlGlobalState = unsafePerformIO (newMVar Nothing)
{-# NOINLINE curlGlobalState #-}

curlRTSO :: RTSO LIBCURL
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
libname :: LIBCURL -> String
libname CURL720 = "libcurl/7.20"
libname CURL730 = "libcurl/7.30"

readapi :: String -> Maybe LIBCURL
readapi xs = parse >>= check
  where
  readP = readP_to_S (string "libcurl/7." >> (readDecP :: ReadP Int))
  parse = case (readP xs) of [(v,_)] -> Just v; _ -> Nothing
  check v
    | v>=20 && v<30 = Just CURL720
    | v>=30         = Just CURL730
    | otherwise     = Nothing


-------------------------------------------------------------------------------
sonames :: LIBCURL -> [String]
sonames
  | os == "mingw32" = winsonames
  | os == "darwin"  = osxsonames
  | os == "linux"   = gnusonames
  | os == "freebsd" = bsdsonames
  | otherwise = const []

winsonames :: LIBCURL -> [String]
winsonames _ = ["libcurl.dll"]

osxsonames :: LIBCURL -> [String]
osxsonames _ = ["libcurl.dylib"]

gnusonames :: LIBCURL -> [String]
gnusonames _ = ["libcurl.so.4", "libcurl.so"]

bsdsonames :: LIBCURL -> [String]
bsdsonames _ = ["libcurl.so"]

