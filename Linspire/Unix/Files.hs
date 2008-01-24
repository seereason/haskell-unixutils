module Linspire.Unix.Files where

import System.Posix.Files
import System.IO.Error

-- |calls 'createSymbolicLink' but will remove the target and retry if
-- 'createSymbolicLink' raises EEXIST.
forceSymbolicLink :: FilePath -> FilePath -> IO ()
forceSymbolicLink target linkName =
    createSymbolicLink target linkName `catch`
      (\e -> if isAlreadyExistsError e 
             then do removeLink linkName
                     createSymbolicLink target linkName
             else ioError e)
