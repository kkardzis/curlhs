-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curl.Easy.CURLinfo
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curl.Easy.CURLinfo
  ( CURLinfo_S (..), CURLinfo_I (..), CURLinfo_D (..), CURLinfo_L (..)
  ) where

import Foreign.C.Types (CInt)
import Data.Tuple (swap)

import Network.Curl.Easy.CURLenum


-------------------------------------------------------------------------------
data CURLinfo_S
  = CURLINFO_EFFECTIVE_URL    -- CURLINFO_STRING + 1,
  | CURLINFO_CONTENT_TYPE     -- CURLINFO_STRING + 18,
  | CURLINFO_PRIVATE          -- CURLINFO_STRING + 21,
  | CURLINFO_FTP_ENTRY_PATH   -- CURLINFO_STRING + 30,
  | CURLINFO_REDIRECT_URL     -- CURLINFO_STRING + 31,
  | CURLINFO_PRIMARY_IP       -- CURLINFO_STRING + 32,
  | CURLINFO_RTSP_SESSION_ID  -- CURLINFO_STRING + 36,
  | CURLINFO_LOCAL_IP         -- CURLINFO_STRING + 41,
  deriving (Eq, Show)

data CURLinfo_I
  = CURLINFO_RESPONSE_CODE    -- CURLINFO_LONG   + 2,
  | CURLINFO_HEADER_SIZE      -- CURLINFO_LONG   + 11,
  | CURLINFO_REQUEST_SIZE     -- CURLINFO_LONG   + 12,
  | CURLINFO_SSL_VERIFYRESULT -- CURLINFO_LONG   + 13,
  | CURLINFO_FILETIME         -- CURLINFO_LONG   + 14,
  | CURLINFO_REDIRECT_COUNT   -- CURLINFO_LONG   + 20,
  | CURLINFO_HTTP_CONNECTCODE -- CURLINFO_LONG   + 22,
  | CURLINFO_HTTPAUTH_AVAIL   -- CURLINFO_LONG   + 23,
  | CURLINFO_PROXYAUTH_AVAIL  -- CURLINFO_LONG   + 24,
  | CURLINFO_OS_ERRNO         -- CURLINFO_LONG   + 25,
  | CURLINFO_NUM_CONNECTS     -- CURLINFO_LONG   + 26,
  | CURLINFO_LASTSOCKET       -- CURLINFO_LONG   + 29,
  | CURLINFO_CONDITION_UNMET  -- CURLINFO_LONG   + 35,
  | CURLINFO_RTSP_CLIENT_CSEQ -- CURLINFO_LONG   + 37,
  | CURLINFO_RTSP_SERVER_CSEQ -- CURLINFO_LONG   + 38,
  | CURLINFO_RTSP_CSEQ_RECV   -- CURLINFO_LONG   + 39,
  | CURLINFO_PRIMARY_PORT     -- CURLINFO_LONG   + 40,
  | CURLINFO_LOCAL_PORT       -- CURLINFO_LONG   + 42,
  deriving (Eq, Show)

data CURLinfo_D
  = CURLINFO_TOTAL_TIME              -- CURLINFO_DOUBLE + 3,
  | CURLINFO_NAMELOOKUP_TIME         -- CURLINFO_DOUBLE + 4,
  | CURLINFO_CONNECT_TIME            -- CURLINFO_DOUBLE + 5,
  | CURLINFO_PRETRANSFER_TIME        -- CURLINFO_DOUBLE + 6,
  | CURLINFO_SIZE_UPLOAD             -- CURLINFO_DOUBLE + 7,
  | CURLINFO_SIZE_DOWNLOAD           -- CURLINFO_DOUBLE + 8,
  | CURLINFO_SPEED_DOWNLOAD          -- CURLINFO_DOUBLE + 9,
  | CURLINFO_SPEED_UPLOAD            -- CURLINFO_DOUBLE + 10,
  | CURLINFO_CONTENT_LENGTH_DOWNLOAD -- CURLINFO_DOUBLE + 15,
  | CURLINFO_CONTENT_LENGTH_UPLOAD   -- CURLINFO_DOUBLE + 16,
  | CURLINFO_STARTTRANSFER_TIME      -- CURLINFO_DOUBLE + 17,
  | CURLINFO_REDIRECT_TIME           -- CURLINFO_DOUBLE + 19,
  | CURLINFO_APPCONNECT_TIME         -- CURLINFO_DOUBLE + 33,
  deriving (Eq, Show)

data CURLinfo_L
  = CURLINFO_SSL_ENGINES      -- CURLINFO_SLIST  + 27,
  | CURLINFO_COOKIELIST       -- CURLINFO_SLIST  + 28,
  | CURLINFO_CERTINFO         -- CURLINFO_SLIST  + 34,
  deriving (Eq, Show)


-------------------------------------------------------------------------------
lookup_err = error "CURLenum.CURLinfo: bad argument or incomplete lookup table"

lookup_key :: (Eq a) => [(CInt, a)] -> CInt -> a
lookup_key table key = maybe lookup_err id $ lookup key table

lookup_val :: (Eq a) => [(CInt, a)] -> a -> CInt
lookup_val table val = maybe lookup_err id $ lookup val $ map swap table

instance CURLenum CURLinfo_S where
  fromCURLenum = lookup_key knownCURLinfo_S
  toCURLenum   = lookup_val knownCURLinfo_S

instance CURLenum CURLinfo_I where
  fromCURLenum = lookup_key knownCURLinfo_I
  toCURLenum   = lookup_val knownCURLinfo_I

instance CURLenum CURLinfo_D where
  fromCURLenum = lookup_key knownCURLinfo_D
  toCURLenum   = lookup_val knownCURLinfo_D

instance CURLenum CURLinfo_L where
  fromCURLenum = lookup_key knownCURLinfo_L
  toCURLenum   = lookup_val knownCURLinfo_L


-------------------------------------------------------------------------------
knownCURLinfo_S :: [(CInt, CURLinfo_S)]
knownCURLinfo_S = map (\(v, i) -> (i+0x100000, v))
  [ (CURLINFO_EFFECTIVE_URL    ,  1)
  , (CURLINFO_CONTENT_TYPE     , 18)
  , (CURLINFO_PRIVATE          , 21)
  , (CURLINFO_FTP_ENTRY_PATH   , 30)
  , (CURLINFO_REDIRECT_URL     , 31)
  , (CURLINFO_PRIMARY_IP       , 32)
  , (CURLINFO_RTSP_SESSION_ID  , 36)
  , (CURLINFO_LOCAL_IP         , 41)
  ]

knownCURLinfo_I :: [(CInt, CURLinfo_I)]
knownCURLinfo_I = map (\(v, i) -> (i+0x200000, v))
  [ (CURLINFO_RESPONSE_CODE    ,  2)
  , (CURLINFO_HEADER_SIZE      , 11)
  , (CURLINFO_REQUEST_SIZE     , 12)
  , (CURLINFO_SSL_VERIFYRESULT , 13)
  , (CURLINFO_FILETIME         , 14)
  , (CURLINFO_REDIRECT_COUNT   , 20)
  , (CURLINFO_HTTP_CONNECTCODE , 22)
  , (CURLINFO_HTTPAUTH_AVAIL   , 23)
  , (CURLINFO_PROXYAUTH_AVAIL  , 24)
  , (CURLINFO_OS_ERRNO         , 25)
  , (CURLINFO_NUM_CONNECTS     , 26)
  , (CURLINFO_LASTSOCKET       , 29)
  , (CURLINFO_CONDITION_UNMET  , 35)
  , (CURLINFO_RTSP_CLIENT_CSEQ , 37)
  , (CURLINFO_RTSP_SERVER_CSEQ , 38)
  , (CURLINFO_RTSP_CSEQ_RECV   , 39)
  , (CURLINFO_PRIMARY_PORT     , 40)
  , (CURLINFO_LOCAL_PORT       , 42)
  ]

knownCURLinfo_D :: [(CInt, CURLinfo_D)]
knownCURLinfo_D = map (\(v, i) -> (i+0x300000, v))
  [ (CURLINFO_TOTAL_TIME              ,  3)
  , (CURLINFO_NAMELOOKUP_TIME         ,  4)
  , (CURLINFO_CONNECT_TIME            ,  5)
  , (CURLINFO_PRETRANSFER_TIME        ,  6)
  , (CURLINFO_SIZE_UPLOAD             ,  7)
  , (CURLINFO_SIZE_DOWNLOAD           ,  8)
  , (CURLINFO_SPEED_DOWNLOAD          ,  9)
  , (CURLINFO_SPEED_UPLOAD            , 10)
  , (CURLINFO_CONTENT_LENGTH_DOWNLOAD , 15)
  , (CURLINFO_CONTENT_LENGTH_UPLOAD   , 16)
  , (CURLINFO_STARTTRANSFER_TIME      , 17)
  , (CURLINFO_REDIRECT_TIME           , 19)
  , (CURLINFO_APPCONNECT_TIME         , 33)
  ]

knownCURLinfo_L :: [(CInt, CURLinfo_L)]
knownCURLinfo_L = map (\(v, i) -> (i+0x400000, v))
  [ (CURLINFO_SSL_ENGINES , 27)
  , (CURLINFO_COOKIELIST  , 28)
  , (CURLINFO_CERTINFO    , 34)
  ]

