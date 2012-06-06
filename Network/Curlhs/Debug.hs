-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Debug
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curlhs.Debug
  ( traceIO
  ) where

import Control.Concurrent (myThreadId)
import Debug.Trace        (putTraceMsg)

traceIO :: String -> IO ()
traceIO s = myThreadId >>= \t -> putTraceMsg ("[" ++ show t ++ "] " ++ s)

