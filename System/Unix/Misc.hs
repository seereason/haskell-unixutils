-- |Wrappers around some handy unix shell commands.  Please let
-- me know if you think of better module names to hold these
-- functions.  -dsf
module System.Unix.Misc
    ( md5sum
    , gzip)
    where

import Control.Exception
import qualified Codec.Compression.GZip
import Data.ByteString.Lazy.Char8 (empty, readFile, writeFile)
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Posix.Files
import System.Process
import System.Unix.Process

{-# DEPRECATED md5sum "Use Data.ByteString.Lazy.Char8.readFile path >>= return . show . Data.Digest.Pure.MD5.md5" #-}
md5sum :: FilePath -> IO String
md5sum path = Data.ByteString.Lazy.Char8.readFile path >>= return . show . md5
{-
    do
      (text, _, exitCode) <- lazyProcess "md5sum" [path] Nothing Nothing empty >>= return . collectOutputUnpacked
      let output = listToMaybe (words text)
      case exitCode of
        ExitSuccess ->
            case output of
              Nothing -> error ("Error in output of 'md5sum " ++ path ++ "'")
              Just checksum -> return checksum
        _ -> error ("Error running 'md5sum " ++ path ++ "'")
-}

{-# DEPRECATED gzip "Use Data.ByteString.Lazy.Char8.readFile path >>= Data.ByteString.Lazy.Char8.writeFile (path ++ ".gz")" #-}
gzip :: FilePath -> IO ()
gzip path = Data.ByteString.Lazy.Char8.readFile path >>= Data.ByteString.Lazy.Char8.writeFile (path ++ ".gz")
{-
    do
      result <- system ("gzip < " ++ path ++ " > " ++ path ++ ".gz")
      case result of
        ExitSuccess -> return ()
        e -> error (show e)
-}
