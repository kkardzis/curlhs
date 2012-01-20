-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curl.Easy.CURLenum
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curl.Easy.CURLenum
  ( CURLenum (..)
  ) where

import Foreign.C.Types


-------------------------------------------------------------------------------
class CURLenum a where
  fromCURLenum :: CInt -> a
  toCURLenum   :: a -> CInt


