-- |Wrappers around some handy unix shell commands.  Please let
-- me know if you think of better module names to hold these
-- functions.  -dsf
module System.Unix.Misc
    ( md5sum
    , gzip)
    where

import Control.Exception
import Data.ByteString.Lazy.Char8 (empty)
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
      (text, _, exitCodes) <- lazyProcess "md5sum" [path] Nothing Nothing empty >>= return . collectOutputUnpacked
      let output = listToMaybe (words text)
      case exitCodes of
        [ExitSuccess] ->
            case output of
              Nothing -> error ("Error in output of 'md5sum " ++ path ++ "'")
              Just checksum -> return checksum
        _ -> error ("Error running 'md5sum " ++ path ++ "'")

{-# WARNING gzip "System.Unix.Misc.gzip does not properly escape its path arguments" #-}
gzip :: FilePath -> IO ()
gzip path =
    do
      result <- system ("gzip < " ++ path ++ " > " ++ path ++ ".gz")
      case result of
        ExitSuccess -> return ()
        e -> error (show e)
