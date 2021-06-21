{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Unix.Crypt
-- Copyright   :  (c) 2010
-- License     :  BSD3
--
-- Maintainer  :  jeremy@seereason.com
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- support for crypt() and /etc/shadow
--
-----------------------------------------------------------------------------

module System.Unix.Crypt
    ( crypt
    )
    where

import Foreign.C

foreign import ccall unsafe "unistd.h crypt"
  c_crypt :: CString -> CString -> IO CString

-- | calls crypt(3)
crypt :: String -- ^ key
      -> String -- ^ salt
      -> IO String -- ^ encrypted password
crypt key salt =
    withCString key $ \ckey ->
        withCString salt $ \csalt ->
            do cpassword <- throwErrnoIfNull "crypt" (c_crypt ckey csalt)
               peekCString cpassword

