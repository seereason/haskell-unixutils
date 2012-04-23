{-# LANGUAGE ScopedTypeVariables #-}
-- |A place to collect and hopefully retire all the random ways of
-- running shell commands that have accumulated over the years.
module System.Unix.KillByCwd
    ( killByCwd
    ) where

import Control.Exception (catch)
import Control.Monad (liftM, filterM)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Prelude hiding (catch)
import System.Directory (getDirectoryContents)
import System.Posix.Files (readSymbolicLink)
import System.Posix.Signals (signalProcess, sigTERM)

{-
NOTE:

+ We should make sure this works if we are inside a chroot.

+ path needs to be absolute or we might kill processes living in
  similarly named, but different directories.

+ path is an canoncialised, absolute path, such as what realpath returns

-}
-- | Kill the processes whose working directory is in or under the
-- given directory.
killByCwd :: FilePath -> IO [(String, Maybe String)]
killByCwd path =
    do pids <- liftM (filter (all isDigit)) (getDirectoryContents "/proc")
       cwdPids <- filterM (isCwd path) pids
       exePaths <- mapM exePath cwdPids
       mapM_ kill cwdPids
       return (zip cwdPids exePaths)
    where
      isCwd :: FilePath -> String -> IO Bool
      isCwd cwd pid =
          (liftM (isPrefixOf cwd) (readSymbolicLink ("/proc/" ++ pid ++"/cwd"))) `catch` (\ (_ :: IOError) -> return False)
      exePath :: String -> IO (Maybe String)
      exePath pid = (readSymbolicLink ("/proc/" ++ pid ++"/exe") >>= return . Just) `catch` (\ (_ :: IOError) -> return Nothing)
      kill :: String -> IO ()
      kill pidStr = signalProcess sigTERM (read pidStr)
