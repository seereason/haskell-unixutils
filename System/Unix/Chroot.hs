{-# LANGUAGE ForeignFunctionInterface #-}
-- | This module, except for useEnv, is copied from the build-env package.
module System.Unix.Chroot
    ( fchroot
    , useEnv
    , forceList
    , forceList'
    ) where

import Control.Exception (finally, evaluate)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Foreign.C.Error
import Foreign.C.String
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropTrailingPathSeparator, dropFileName)
import System.IO (hPutStr, stderr)
import System.Posix.Env (getEnv)
import System.Posix.IO
import System.Posix.Directory
import System.Unix.Process (Output(..))
import System.Unix.Progress (lazyCommandF)
import System.Unix.QIO (quieter)

foreign import ccall unsafe "chroot" c_chroot :: CString -> IO Int

-- |chroot changes the root directory to filepath
-- NOTE: it does not change the working directory, just the root directory
-- NOTE: will throw IOError if chroot fails
chroot :: FilePath -> IO ()
chroot fp = withCString fp $ \cfp -> throwErrnoIfMinus1_ "chroot" (c_chroot cfp)

-- |fchroot runs an IO action inside a chroot
-- fchroot performs a chroot, runs the action, and then restores the
-- original root and working directory. This probably affects the
-- chroot and working directory of all the threads in the process,
-- so...
-- NOTE: will throw IOError if internal chroot fails
fchroot :: FilePath -> IO a -> IO a
fchroot path action =
    do origWd <- getWorkingDirectory
       rootFd <- openFd "/" ReadOnly Nothing defaultFileFlags
       chroot path
       changeWorkingDirectory "/"
       action `finally` (breakFree origWd rootFd)
    where
      breakFree origWd rootFd =
          do changeWorkingDirectoryFd rootFd
             closeFd rootFd
             chroot "."
             changeWorkingDirectory origWd

-- |The ssh inside of the chroot needs to be able to talk to the
-- running ssh-agent.  Therefore we mount --bind the ssh agent socket
-- dir inside the chroot (and umount it when we exit the chroot.
useEnv :: FilePath -> (a -> IO a) -> IO a -> IO a
useEnv rootPath force action =
    do sockPath <- getEnv "SSH_AUTH_SOCK"
       home <- getEnv "HOME"
       copySSH home
       -- We need to force the output before we exit the changeroot.
       -- Otherwise we lose our ability to communicate with the ssh
       -- agent and we get errors.
       withSock sockPath . fchroot rootPath $ (action >>= force)
    where
      copySSH Nothing = return ()
      copySSH (Just home) =
          -- Do NOT preserve ownership, files must be owned by root.
          system' ("rsync -rlptgDHxS --delete " ++ home ++ "/.ssh/ " ++ rootPath ++ "/root/.ssh")
      withSock Nothing action = action
      withSock (Just sockPath) action =
          withMountBind dir (rootPath ++ dir) action
          where dir = dropTrailingPathSeparator (dropFileName sockPath)
      withMountBind toMount mountPoint action =
          doMount
          where
            doMount =
                do createDirectoryIfMissing True mountPoint
                   system' $ "mount --bind " ++ escapePathForMount toMount ++ " " ++ escapePathForMount mountPoint
                   result <- action
                   system' $ "umount " ++ escapePathForMount mountPoint
                   return result
      escapePathForMount = id	-- FIXME - Path arguments should be escaped
      system' s = lazyCommandF s L.empty >> return ()
          -- system s >>= testcode
          -- where testcode (ExitFailure n) = error (show s ++ " -> " ++ show n)
          --       testcode ExitSuccess = return ()

-- |A function to force the process output by examining it but not
-- printing anything.
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

-- |First send the process output to the and then force it.
forceList' :: [Output] -> IO [Output]
forceList' output = printOutput output >>= forceList

-- |Print all the output to the appropriate output channel
printOutput :: [Output] -> IO [Output]
printOutput output =
    mapM print output
    where
      print x@(Stdout s) = putStr (B.unpack s) >> return x
      print x@(Stderr s) = hPutStr stderr (B.unpack s) >> return x
      print x = return x

{-
printDots :: Int -> [Output] -> IO [Output]
printDots cpd output =
    foldM f 0 output >> return output
    where
      print rem (Stdout s) =
          let (dots, rem') = quotRem (rem + length s) in
          hPutStr stderr (replicate dots '.')
          return rem'
      print rem (Stderr s) = print rem (Stdout s)
-}
