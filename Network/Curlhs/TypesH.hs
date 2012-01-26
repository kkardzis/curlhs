-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.TypesH
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Curlhs.TypesH where

import Foreign.Ptr


-------------------------------------------------------------------------------
class PeekCCURL c h where
  peekCCURL :: Ptr c -> IO h

class FromC c h where
  fromC :: c -> h

class FromH h c where
  fromH :: h -> c


-------------------------------------------------------------------------------
findWithDef :: (Eq a) => b -> a -> [(a, b)] -> b
findWithDef def key table = maybe def id $ lookup key table

cError s = error $ "FromC "++s++": bad argument"
hError s = error $ "FromH "++s++": incomplete lookup table"

