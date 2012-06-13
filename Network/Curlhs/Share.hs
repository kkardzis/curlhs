-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Share
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curlhs.Share (

  -- * Init, cleanup
    CURLSH
  , withCURLSH
  , curl_share_init
  , curl_share_cleanup

  -- * Set options
  , curl_share_setopt
  , CURLSHoption (..)

  -- ** Constants
  , CURLSHlockdata (..)

  -- * Exceptions
  -- | More about error codes in libcurl on
  --   <http://curl.haxx.se/libcurl/c/libcurl-errors.html>
  , curl_share_strerror
  , CURLSHcode (..)

  ) where

import Foreign.Ptr        (Ptr, freeHaskellFunPtr, nullPtr)

import Control.Exception  (throwIO, bracket, bracketOnError)
import Control.Concurrent (MVar, newMVar, takeMVar, tryPutMVar, modifyMVar)

import Network.Curlhs.Base
import Network.Curlhs.Core
import Network.Curlhs.Types


-------------------------------------------------------------------------------
withCURLSH :: (CURLSH -> IO a) -> IO a
withCURLSH = bracket curl_share_init curl_share_cleanup


-------------------------------------------------------------------------------
-- | Create a shared object
--   (<http://curl.haxx.se/libcurl/c/curl_share_init.html>).
-------------------------------------------------------------------------------
curl_share_init :: IO CURLSH
curl_share_init = do
  ccurlsh <- withGlobalInitCheck ccurl_share_init
  if (ccurlsh == nullPtr)
    then throwIO CURLE_FAILED_INIT -- should be some CURLSHE error
    else newCURLSH ccurlsh

newCURLSH :: Ptr CCURLSH -> IO CURLSH
newCURLSH ccurlsh = do
  shlocks <- newSHLocks
  let getFLockPtr = wrap_ccurl_lock_function (lock_function shlocks)
  flock <- bracketOnError getFLockPtr freeHaskellFunPtr $ \fptr -> do
    withCURLSHE $ ccurl_share_setopt'FLOCK ccurlsh cCURLSHOPT_LOCKFUNC fptr
    return fptr
  let getFUnlockPtr = wrap_ccurl_unlock_function (unlock_function shlocks)
  funlock <- bracketOnError getFUnlockPtr freeHaskellFunPtr $ \fptr -> do
    withCURLSHE $ ccurl_share_setopt'FUNLOCK ccurlsh cCURLSHOPT_UNLOCKFUNC fptr
    return fptr
  return $ CURLSH ccurlsh flock funlock


-------------------------------------------------------------------------------
lock_function :: SHLocks -> CCURL_lock_function
lock_function shlocks _ccurl lockdata _lockaccess _usrptr =
  lookupSHLock shlocks lockdata >>= getSHLock

unlock_function :: SHLocks -> CCURL_unlock_function
unlock_function shlocks _ccurl lockdata _usrptr =
  lookupSHLock shlocks lockdata >>= putSHLock


-------------------------------------------------------------------------------
type SHLockID = CCURL_lock_data
type SHLock   = MVar ()
type SHLocks  = MVar [(SHLockID, SHLock)]

newSHLocks :: IO SHLocks
newSHLocks = newMVar []

getSHLock :: SHLock -> IO ()
getSHLock shlock = takeMVar shlock

putSHLock :: SHLock -> IO ()
putSHLock shlock = tryPutMVar shlock () >> return ()

lookupSHLock :: SHLocks -> SHLockID -> IO SHLock
lookupSHLock mvar shlockid =
  modifyMVar mvar $ \shlocks ->
    case (lookup shlockid shlocks) of
      Just shlock -> return (shlocks, shlock)
      Nothing     -> newMVar () >>= \nl -> return ((shlockid, nl):shlocks, nl)


-------------------------------------------------------------------------------
-- | Clean up a shared object
--   (<http://curl.haxx.se/libcurl/c/curl_share_cleanup.html>).
-------------------------------------------------------------------------------
curl_share_cleanup :: CURLSH -> IO ()
curl_share_cleanup curlsh = do
  withCURLSHE $ ccurl_share_cleanup (asCCURLSH curlsh)
  freeHaskellFunPtr (cb_lock curlsh)
  freeHaskellFunPtr (cb_unlock curlsh)


-------------------------------------------------------------------------------
-- | Set options for a shared object
--   (<http://curl.haxx.se/libcurl/c/curl_share_setopt.html>).
-------------------------------------------------------------------------------
curl_share_setopt :: CURLSH -> [CURLSHoption] -> IO ()
curl_share_setopt curlsh opts = flip mapM_ opts $ \opt -> case opt of
  CURLSHOPT_SHARE   x -> setopt'Lock curlsh cCURLSHOPT_SHARE   x
  CURLSHOPT_UNSHARE x -> setopt'Lock curlsh cCURLSHOPT_UNSHARE x

setopt'Lock :: CURLSH -> CCURLSHoption'Lock -> CURLSHlockdata -> IO ()
setopt'Lock curlsh copt val =
  withCURLSHE $ ccurl_share_setopt'Lock (asCCURLSH curlsh) copt cval
    where cval = case val of
            CURL_LOCK_DATA_COOKIE      -> cCURL_LOCK_DATA_COOKIE
            CURL_LOCK_DATA_DNS         -> cCURL_LOCK_DATA_DNS
            CURL_LOCK_DATA_SSL_SESSION -> cCURL_LOCK_DATA_SSL_SESSION


