{-# LANGUAGE ForeignFunctionInterface #-}
-- | The function splitFileName is taken from missingh, at the moment
-- missingh will not build under sid.

module System.Unix.FilePath 
    (dirName,
     baseName,
     realpath,
     -- * From MissingH
     splitFileName)
    where

import Data.List
import Text.Regex
import Foreign.C
import Foreign.Marshal.Array

#include <limits.h>
#include <stdlib.h>

dirName :: FilePath -> FilePath
dirName path = fst (splitFileName path)

baseName :: FilePath -> String
baseName path = snd (splitFileName path)

canon :: FilePath -> FilePath
-- ^ Weak attempt at canonicalizing a file path.
canon path =
    let re = mkRegex "/" in
    let names = splitRegex re path in
    concat (intersperse "/" (merge names))
    where
      merge (".." : xs) = ".." : (merge xs)
      merge ("." : xs) = "." : (merge xs)
      merge (_ : ".." : xs) = (merge xs)
      merge (x : "." : xs) = (merge (x : xs))
      merge (x : xs) = x : merge xs
      merge [] = []

-------------- From MissingH --------------


-- | Split the path into directory and file name
--
-- Examples:
--
-- \[Posix\]
--
-- > splitFileName "/"            == ("/",    ".")
-- > splitFileName "/foo/bar.ext" == ("/foo", "bar.ext")
-- > splitFileName "bar.ext"      == (".",    "bar.ext")
-- > splitFileName "/foo/."       == ("/foo", ".")
-- > splitFileName "/foo/.."      == ("/foo", "..")
--
-- \[Windows\]
--
-- > splitFileName "\\"               == ("\\",      "")
-- > splitFileName "c:\\foo\\bar.ext" == ("c:\\foo", "bar.ext")
-- > splitFileName "bar.ext"          == (".",       "bar.ext")
-- > splitFileName "c:\\foo\\."       == ("c:\\foo", ".")
-- > splitFileName "c:\\foo\\.."      == ("c:\\foo", "..")
--
-- The first case in the Windows examples returns an empty file name.
-- This is a special case because the \"\\\\\" path doesn\'t refer to
-- an object (file or directory) which resides within a directory.
splitFileName :: FilePath -> (String, String)
splitFileName p = (reverse path1, reverse fname1)
  where
    (fname,path) = break isPathSeparator (reverse p)
    path1 = case path of
      "" -> "."
      _  -> case dropWhile isPathSeparator path of
	"" -> [pathSeparator]
	p  -> p
    fname1 = case fname of
      "" -> "."
      _  -> fname

-- | Checks whether the character is a valid path separator for the host
-- platform. The valid character is a 'pathSeparator' but since the Windows
-- operating system also accepts a slash (\"\/\") since DOS 2, the function
-- checks for it on this platform, too.
isPathSeparator :: Char -> Bool
isPathSeparator ch =
  ch == '/'

-- | Provides a platform-specific character used to separate directory levels in
-- a path string that reflects a hierarchical file system organization. The
-- separator is a slash (@\"\/\"@) on Unix and Macintosh, and a backslash
-- (@\"\\\"@) on the Windows operating system.
pathSeparator :: Char
pathSeparator = '/'

-- |resolve all references to /./, /../, extra slashes, and symlinks
realpath :: FilePath -> IO FilePath
realpath fp =
    withCString fp $ \cfp ->
        allocaArray (#const PATH_MAX) $ \res ->
            throwErrnoIfNull "realpath" (c_realpath cfp res) >>= peekCString

foreign import ccall unsafe "realpath" c_realpath :: CString -> CString -> IO CString
