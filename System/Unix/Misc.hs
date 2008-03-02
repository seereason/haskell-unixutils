-- |Wrappers around some handy unix shell commands.  Please let
-- me know if you think of better module names to hold these
-- functions.  -dsf
module System.Unix.Misc
    ( md5sum
    , gzip)
    where

import Control.Exception
import Data.Maybe
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Posix.Files
import System.Process
import System.Unix.Process

md5sum :: FilePath -> IO String
md5sum path =
    do
      (text, _, exitCode) <- simpleProcess "md5sum" [path]
      let output = listToMaybe (words text)
      case exitCode of
        ExitSuccess ->
            case output of
              Nothing -> error ("Error in output of 'md5sum " ++ path ++ "'")
              Just checksum -> return checksum
        ExitFailure _ -> error ("Error running 'md5sum " ++ path ++ "'")

gzip :: FilePath -> IO ()
gzip path =
    do
      result <- system ("gzip < " ++ path ++ " > " ++ path ++ ".gz")
      case result of
        ExitSuccess -> return ()
        e -> error (show e)
