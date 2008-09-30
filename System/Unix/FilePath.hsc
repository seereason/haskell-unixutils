{-# LANGUAGE ForeignFunctionInterface #-}
{-# DEPRECATED baseName, dirName "Use System.FilePath.takeFileName for baseName,  System.FilePath.dropFileName for dirName  #-}

-- | The function splitFileName is taken from missingh, at the moment
-- missingh will not build under sid.

module System.Unix.FilePath 
    (dirName,
     baseName,
     realpath,
     (<++>))
    where

import Data.List
import System.FilePath (makeRelative, (</>), takeFileName, dropFileName)
import Text.Regex
import Foreign.C
import Foreign.Marshal.Array

#include <limits.h>
#include <stdlib.h>

-- |Concatenate two paths, making sure there is exactly one path separator.
a <++> b = a </> (makeRelative "" b)

-- |Use dropFileName
dirName :: FilePath -> FilePath
dirName = dropFileName

-- |Use takeFileName
baseName :: FilePath -> String
baseName = takeFileName

-- |resolve all references to /./, /../, extra slashes, and symlinks
realpath :: FilePath -> IO FilePath
realpath fp =
    withCString fp $ \cfp ->
        allocaArray (#const PATH_MAX) $ \res ->
            throwErrnoIfNull "realpath" (c_realpath cfp res) >>= peekCString

foreign import ccall unsafe "realpath" c_realpath :: CString -> CString -> IO CString
