module System.Unix.Files where

import Control.Exception (catch)
import Prelude hiding (catch)
import System.Posix.Files (createSymbolicLink, removeLink)
import System.IO.Error (isAlreadyExistsError)

-- |calls 'createSymbolicLink' but will remove the target and retry if
-- 'createSymbolicLink' raises EEXIST.
forceSymbolicLink :: FilePath -> FilePath -> IO ()
forceSymbolicLink target linkName =
    createSymbolicLink target linkName `catch`
      (\e -> if isAlreadyExistsError e
             then do removeLink linkName
                     createSymbolicLink target linkName
             else ioError e)
