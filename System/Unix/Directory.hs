{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module System.Unix.Directory
    ( find
    , removeRecursiveSafely
    , unmountRecursiveSafely
    , renameFileWithBackup
    , withWorkingDirectory
    , withTemporaryDirectory
    , mkdtemp
    )
    where

import Control.Exception
import Data.List (isSuffixOf)
import System.Process
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import System.Posix.Types
import Foreign.C

-- | Traverse a directory and return a list of all the (path,
-- fileStatus) pairs.
find :: FilePath -> IO [(FilePath, FileStatus)]
find path =
    do
      status <- getSymbolicLinkStatus path
      case isDirectory status of
        True -> 
            do
              subs <- getDirectoryContents path >>=
                      return . map (path </>) . filter (not . flip elem [".", ".."]) >>=
                      mapM find >>=
                      return . concat
              return $ (path, status) : subs
        False ->
            return [(path, status)]

traverse :: FilePath -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> IO ()
-- ^ Traverse a file system directory applying D to every directory, F
-- to every non-directory file, and M to every mount point.
-- NOTE: It is tempting to use the "find" function to returns a list
-- of the elements of the directory and then map that list over an
-- "unmount and remove" function.  However, because we are unmounting
-- as we traverse, the contents of the file list may change in ways
-- that could confuse the find function.
traverse path f d m =
    do
      result <- try $ getSymbolicLinkStatus path
      either (\ (_ :: SomeException) -> return ()) (doPath path) result
    where
      doPath path status =
          if isDirectory status then
              do
                getDirectoryContents path >>= mapM (doDirectoryFile 1 status path)
                d path else
              f path

      doDirectoryFile :: Int -> FileStatus -> FilePath -> String -> IO ()
      doDirectoryFile _ _ _ "." = return ()
      doDirectoryFile _ _ _ ".." = return ()
      doDirectoryFile tries _ _ _ | tries >= 5 =
          error ("Couldn't unmount file system on " ++ path)
      doDirectoryFile tries status path name =
          do
            let child = path </> name
            childStatus <- getSymbolicLinkStatus child
            if deviceID status == deviceID childStatus then
                doPath child childStatus else
                do
                  if tries > 1 then hPutStrLn stderr ("try " ++ show tries ++ ":") else return ()
                  m child
                  doDirectoryFile (tries + 1) status path name

-- |Recursively remove a directory contents on a single file system.
-- The adjective \"Safely\" refers to these features:
--   1. It will not follow symlinks
--   2. If it finds a directory that seems to be a mount point,
--	it will attempt to unmount it up to five times.  If it
--	still seems to be a mount point it gives up
--   3. It doesn't use /proc/mounts, which is ambiguous or wrong
--	when you are inside a chroot.
removeRecursiveSafely :: FilePath -> IO ()
removeRecursiveSafely path =
    System.Unix.Directory.traverse path removeFile removeDirectory umount
    where
      umount path =
          do
            hPutStrLn stderr ("-- removeRecursiveSafely: unmounting " ++ path)
            -- This is less likely to hang and more likely to succeed
            -- than regular umount.
            let cmd = "umount -l " ++ path
            result <- system cmd
            case result of
              ExitSuccess -> return ()
              ExitFailure n -> error ("Failure: " ++ cmd ++ " -> " ++ show n)

unmountRecursiveSafely :: FilePath -> IO ()
-- ^ Like removeRecursiveSafely but doesn't remove any files, just
-- unmounts anything it finds mounted.  Note that this can be much
-- slower than Mount.umountBelow, use that instead.
unmountRecursiveSafely path =
    System.Unix.Directory.traverse path noOp noOp umount
    where
      noOp _ = return ()
      umount path =
          do
            hPutStrLn stderr ("-- unmountRecursiveSafely: unmounting " ++ path)
            -- This is less likely to hang and more likely to succeed
            -- than regular umount.
            let cmd = "umount -l " ++ path
            code <- system cmd
            case code of
              ExitSuccess -> return ()
              ExitFailure n -> error ("Failure: " ++ cmd ++ " -> " ++ show n)

-- |Rename src to dst, and if dst already exists move it to dst~.
-- If dst~ exists it is removed.
renameFileWithBackup :: FilePath -> FilePath -> IO ()
renameFileWithBackup src dst =
    do
      removeIfExists (dst ++ "~")
      renameIfExists dst (dst ++ "~")
      System.Directory.renameFile src dst
    where
      removeIfExists path =
          do exists <- doesFileExist path
             if exists then removeFile path else return ()
      renameIfExists src dst =
          do exists <- doesFileExist src
             if exists then System.Directory.renameFile src dst else return ()

-- |temporarily change the working directory to |dir| while running |action|
withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory dir action = 
    bracket getCurrentDirectory setCurrentDirectory (\ _ -> setCurrentDirectory dir >> action)

-- |create a temporary directory, run the action, remove the temporary directory
-- the first argument is a template for the temporary directory name
-- the directory will be created as a subdirectory of the directory returned by getTemporaryDirectory
-- the temporary directory will be automatically removed afterwards.
-- your working directory is not altered
withTemporaryDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withTemporaryDirectory fp f =
     do sysTmpDir <- getTemporaryDirectory
        bracket (mkdtemp (sysTmpDir </> fp))
                removeRecursiveSafely
                f

foreign import ccall unsafe "stdlib.h mkdtemp"
  c_mkdtemp :: CString -> IO CString

mkdtemp :: FilePath -> IO FilePath
mkdtemp template = 
      withCString (if "XXXXXX" `isSuffixOf` template then template else (template ++ "XXXXXX")) $ \ ptr -> do
        cname <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
        name <- peekCString cname
        return name
