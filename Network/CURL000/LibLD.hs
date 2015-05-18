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

import System.RTLD


-------------------------------------------------------------------------------
data LIBCURL = CURL720 | CURL730
  deriving (Eq, Ord, Enum, Bounded)

instance RTLD LIBCURL where
  loadlib _ = return ()
  freelib _ = return ()

