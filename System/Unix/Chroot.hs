{-# LANGUAGE ForeignFunctionInterface #-}
-- | This module, except for useEnv, is copied from the build-env package.
module System.Unix.Chroot
    ( fchroot
    , useEnv
    -- , forceList  -- moved to progress
    -- , forceList'
    ) where

import Control.Exception (evaluate)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Foreign.C.Error
import Foreign.C.String
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (dropTrailingPathSeparator, dropFileName)
import System.IO (hPutStr, stderr)
import System.Posix.Env (getEnv)
import System.Posix.IO
import System.Posix.Directory
import System.Process (readProcessWithExitCode, showCommandForUser)

foreign import ccall unsafe "chroot" c_chroot :: CString -> IO Int

{-# DEPRECATED forceList "If you need forceList enable it in progress-System.Unix.Process." #-}
forceList = undefined
{-# DEPRECATED forceList' "If you need forceList' enable it in progress-System.Unix.Process." #-}
forceList' = undefined

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
fchroot :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
fchroot path action =
    do origWd <- liftIO $ getWorkingDirectory
       rootFd <- liftIO $ openFd "/" ReadOnly defaultFileFlags
       liftIO $ chroot path
       liftIO $ changeWorkingDirectory "/"
       action `finally` (liftIO $ breakFree origWd rootFd)
    where
      breakFree origWd rootFd =
          do changeWorkingDirectoryFd rootFd
             closeFd rootFd
             chroot "."
             changeWorkingDirectory origWd

-- |The ssh inside of the chroot needs to be able to talk to the
-- running ssh-agent.  Therefore we mount --bind the ssh agent socket
-- dir inside the chroot (and umount it when we exit the chroot.
useEnv :: (MonadIO m, MonadMask m) => FilePath -> (a -> m a) -> m a -> m a
useEnv rootPath force action =
    do -- In order to minimize confusion, this QIO message is output
       -- at default quietness.  If you want to suppress it while seeing
       -- the output from your action, you need to say something like
       -- quieter (+ 1) (useEnv (quieter (\x->x-1) action))
       sockPath <- liftIO $ getEnv "SSH_AUTH_SOCK"
       home <- liftIO $ getEnv "HOME"
       liftIO $ copySSH home
       -- We need to force the output before we exit the changeroot.
       -- Otherwise we lose our ability to communicate with the ssh
       -- agent and we get errors.
       withSock sockPath . fchroot rootPath $ (action >>= force)
    where
      copySSH Nothing = return ()
      copySSH (Just home) =
          -- Do NOT preserve ownership, files must be owned by root.
          createDirectoryIfMissing True (rootPath ++ "/root") >>
          run "/usr/bin/rsync" ["-rlptgDHxS", "--delete", home ++ "/.ssh/", rootPath ++ "/root/.ssh"]
      withSock :: (MonadIO m, MonadMask m) => Maybe FilePath -> m a -> m a
      withSock Nothing action = action
      withSock (Just sockPath) action =
          withMountBind dir (rootPath ++ dir) action
          where dir = dropTrailingPathSeparator (dropFileName sockPath)
      withMountBind :: (MonadIO m, MonadMask m) => FilePath -> FilePath -> m a -> m a
      withMountBind toMount mountPoint action =
          (do liftIO $ createDirectoryIfMissing True mountPoint
              liftIO $ run "/bin/mount" ["--bind", escapePathForMount toMount, escapePathForMount mountPoint]
              action) `finally` (liftIO $ run "/bin/umount" [escapePathForMount mountPoint])
      escapePathForMount = id   -- FIXME - Path arguments should be escaped

      run cmd args =
          do (code, out, err) <- readProcessWithExitCode cmd args ""
             case code of
               ExitSuccess -> return ()
               _ -> error ("Exception in System.Unix.Chroot.useEnv: " ++ showCommandForUser cmd args ++ " -> " ++ show code ++
                           "\n\nstdout:\n " ++ prefix "> " out ++ "\n\nstderr:\n" ++ prefix "> " err)
      prefix pre s = unlines (map (pre ++) (lines s))

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
