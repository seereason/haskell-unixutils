{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Unix.Shadow
-- Copyright   :  (c) 2010 Jeremy Shaw, The University of Glasgow
-- License     :  BSD3
-- 
-- Maintainer  :  jeremy@seereason.com
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- support for /etc/shadow
--
-- TODO: This module is modelled after System.Posix.User but lacks many 
-- of the #ifdefs. Those are probably important.
-----------------------------------------------------------------------------
module System.Unix.Shadow 
    ( SUserEntry(..)
    , getSUserEntryForName
    ) where

import Control.Exception
import Control.Monad
import Foreign
import Foreign.C
import System.Posix.Types
import System.IO.Error

#include "shadow.h"

type CSpwd = ()

-- | Entry returned by 'getSUserEntryForName'
--
-- TODO: add other fields
data SUserEntry =
 SUserEntry {
   sUserName      :: String,     -- ^ Textual name of this user (pw_name)
   sUserPassword  :: String      -- ^ Password -- may be empty or fake if shadow is in use (pw_passwd)
 } deriving (Show, Read, Eq)



-- | @getSUserEntryForName name@ calls @getspnam@ to obtain
--   the @SUserEntry@ information associated with the user login
--   @name@.p
getSUserEntryForName :: String -> IO SUserEntry
-- #if HAVE_GETPWNAM_R
getSUserEntryForName name = do
  allocaBytes (#const sizeof(struct spwd)) $ \ppw ->
    alloca $ \ pppw ->
      withCString name $ \ pstr -> do
	throwErrorIfNonZero_ "getsUserEntryForName" $
	  doubleAllocWhile isERANGE pwBufSize $ \s b ->
	    c_getspnam_r pstr ppw b (fromIntegral s) pppw
	r <- peekElemOff pppw 0
	when (r == nullPtr) $
	  ioError $ flip ioeSetErrorString "no user name"
		  $ mkIOError doesNotExistErrorType
			      "getUserEntryForName"
			      Nothing
			      (Just name)
	unpackSUserEntry ppw

foreign import ccall unsafe "getspnam_r"
  c_getspnam_r :: CString -> Ptr CSpwd
               -> CString -> CSize -> Ptr (Ptr CSpwd) -> IO CInt
{-
#elif HAVE_GETPWNAM
getUserEntryForName name = do
  withCString name $ \ pstr -> do
    withMVar lock $ \_ -> do
      ppw <- throwErrnoIfNull "getUserEntryForName" $ c_getpwnam pstr
      unpackUserEntry ppw

foreign import ccall unsafe "getpwnam" 
  c_getpwnam :: CString -> IO (Ptr CPasswd)
#else
getUserEntryForName = error "System.Posix.User.getUserEntryForName: not supported"
#endif
-}

unpackSUserEntry :: Ptr CSpwd -> IO SUserEntry
unpackSUserEntry ptr = do
   name   <- (#peek struct spwd, sp_namp)   ptr >>= peekCString
   passwd <- (#peek struct spwd, sp_pwdp)   ptr >>= peekCString
   return (SUserEntry name passwd)

isERANGE :: Integral a => a -> Bool
isERANGE = (== eRANGE) . Errno . fromIntegral

doubleAllocWhile :: (a -> Bool) -> Int -> (Int -> Ptr b -> IO a) -> IO a
doubleAllocWhile p s m = do
  r <- allocaBytes s (m s)
  if p r then doubleAllocWhile p (2 * s) m else return r


pwBufSize :: Int
pwBufSize = 1024

-- Used when calling re-entrant system calls that signal their 'errno' 
-- directly through the return value.
throwErrorIfNonZero_ :: String -> IO CInt -> IO ()
throwErrorIfNonZero_ loc act = do
    rc <- act
    if (rc == 0) 
     then return ()
     else ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)
