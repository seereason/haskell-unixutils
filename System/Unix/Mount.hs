{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
-- |functions for mounting, umounting, parsing \/proc\/mounts, etc
module System.Unix.Mount
    ( umountBelow	-- FilePath -> IO [(FilePath, (String, String, ExitCode))]
    , umount		-- [String] -> IO (String, String, ExitCode)
    , isMountPoint	-- FilePath -> IO Bool

    , withMount
    , WithProcAndSys(runWithProcAndSys)
    , withProcAndSys
    , withTmp
    ) where

-- Standard GHC modules

import Control.Monad
import Data.ByteString.Lazy.Char8 (empty)
import Data.List
import System.Directory
import System.Exit
import System.IO (readFile, hPutStrLn, stderr)
import System.Posix.Files
import System.Process (readProcessWithExitCode)

import Control.Applicative (Applicative)
import Control.Exception (catch)
import Control.Monad.Catch (bracket, MonadCatch, MonadMask)
import Control.Monad.Trans (MonadTrans, lift, liftIO, MonadIO)
-- import Control.Monad.Trans.Except ({- ExceptT instances -})
import Data.ByteString.Lazy as L (ByteString, empty)
import GHC.IO.Exception (IOErrorType(OtherError))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error
import System.Process (CreateProcess, proc)
import System.Process.ListLike (readCreateProcess, showCreateProcessForUser)

-- Local Modules

-- In ghc610 readFile "/proc/mounts" hangs.  Use this instead.
-- rf path = lazyCommand ("cat '" ++ path ++ "'") empty >>= return . (\ (o, _, _) -> o) . collectOutputUnpacked

-- |'umountBelow' - unmounts all mount points below /belowPath/
-- \/proc\/mounts must be present and readable.  Because of the way
-- linux handles changeroots, we can't trust everything we see in
-- \/proc\/mounts.  However, we make the following assumptions:
--
--  (1) there is a one-to-one correspondence between the entries in
--      \/proc\/mounts and the actual mounts, and
--  (2) every mount point we might encounter is a suffix of one of
--      the mount points listed in \/proc\/mounts (because being in a
--      a chroot doesn't affect \/proc\/mounts.)
--
-- So we can search \/proc\/mounts for an entry has the mount point
-- we are looking for as a substring, then add the extra text on
-- the right to our path and try to unmount that.  Then we start
-- again since nested mounts might have been revealed.
--
-- For example, suppose we are chrooted into
-- \/home\/david\/environments\/sid and we call "umountBelow \/proc".  We
-- might see the mount point \/home\/david\/environments\/sid\/proc\/bus\/usb
-- in \/proc\/mounts, which means we need to run "umount \/proc\/bus\/usb".
--
-- See also: 'umountSucceeded'
umountBelow :: Bool     -- ^ Lazy (umount -l flag) if true
            -> FilePath -- ^ canonicalised, absolute path
            -> IO [(FilePath, (ExitCode, String, String))] -- ^ paths that we attempted to umount, and the responding output from the umount command
umountBelow lazy belowPath =
    do procMount <- readFile "/proc/mounts"
       let mountPoints = map (unescape . (!! 1) . words) (lines procMount)
           maybeMounts = filter (isPrefixOf belowPath) (concat (map tails mountPoints))
           args path = ["-f"] ++ if lazy then ["-l"] else [] ++ [path]
       needsUmount <- filterM isMountPoint maybeMounts
       results <- mapM (\ path -> hPutStrLn stderr ("umountBelow: umount " ++ intercalate " " (args path)) >> umount (args path) >>= return . ((,) path)) needsUmount
       let results' = map fixNotMounted results
       mapM_ (\ (result, result') -> hPutStrLn stderr (show result ++ (if result /= result' then " -> " ++ show result' else ""))) (zip results results')
       -- Did /proc/mounts change?  If so we should try again because
       -- nested mounts might have been revealed.
       procMount' <- readFile "/proc/mounts"
       results'' <- if procMount /= procMount' then umountBelow lazy belowPath else return []
       return $ results' ++ results''
    where
      fixNotMounted (path, (ExitFailure 1, "", err)) | err == ("umount: " ++ path ++ ": not mounted\n") = (path, (ExitSuccess, "", ""))
      fixNotMounted x = x

-- |umountSucceeded - predicated suitable for filtering results of 'umountBelow'
umountSucceeded :: (FilePath, (String, String, ExitCode)) -> Bool
umountSucceeded (_, (_,_,ExitSuccess)) = True
umountSucceeded _ = False

-- |'unescape' - unescape function for strings in \/proc\/mounts
unescape :: String -> String
unescape [] = []
unescape ('\\':'0':'4':'0':rest) = ' ' : (unescape rest)
unescape ('\\':'0':'1':'1':rest) = '\t' : (unescape rest)
unescape ('\\':'0':'1':'2':rest) = '\n' : (unescape rest)
unescape ('\\':'1':'3':'4':rest) = '\\' : (unescape rest)
unescape (c:rest) = c : (unescape rest)

-- |'escape' - \/proc\/mount style string escaper
escape :: String -> String
escape [] = []
escape (' ':rest)  = ('\\':'0':'4':'0':escape rest)
escape ('\t':rest) = ('\\':'0':'1':'1':escape rest)
escape ('\n':rest) = ('\\':'0':'1':'2':escape rest)
escape ('\\':rest) = ('\\':'1':'3':'4':escape rest)
escape (c:rest)    = c : (escape rest)


-- |'umount' - run umount with the specified args
-- NOTE: this function uses exec, so you do /not/ need to shell-escape
-- NOTE: we don't use the umount system call because the system call
-- is not smart enough to update \/etc\/mtab
umount :: [String] -> IO (ExitCode, String, String)
umount args = readProcessWithExitCode "umount" args ""

isMountPoint :: FilePath -> IO Bool
-- This implements the functionality of mountpoint(1), deciding
-- whether a path is a mountpoint by seeing whether it is on a
-- different device from its parent.  It would fail if a file system
-- is mounted directly inside itself, but I think maybe that isn't
-- allowed.
isMountPoint path =
    do
      exists <- doesDirectoryExist (path ++ "/.")
      parentExists <- doesDirectoryExist (path ++ "/..")
      case (exists, parentExists) of
        (True, True) ->
            do
              id <- getFileStatus (path ++ "/.") >>= return . deviceID
              parentID <- getFileStatus (path ++ "/..") >>= return . deviceID
              return $ id /= parentID
        _ ->
            -- It is hard to know what is going on if . or .. don't exist.
            -- Assume we are seeing some sort of mount point.
            return True

readProcess :: CreateProcess -> L.ByteString -> IO L.ByteString
readProcess p input = do
  (code, out, _err) <- readCreateProcess p input :: IO (ExitCode, L.ByteString, L.ByteString)
  case code of
    ExitFailure n -> ioError (mkIOError OtherError (showCreateProcessForUser p ++ " -> " ++ show n) Nothing Nothing)
    ExitSuccess -> return out

-- | Do an IO task with a file system remounted using mount --bind.
-- This was written to set up a build environment.
withMount :: (MonadIO m, MonadMask m) => FilePath -> FilePath -> m a -> m a
withMount directory mountpoint task =
    bracket pre (\ _ -> post) (\ _ -> task)
    where
      mount = proc "mount" ["--bind", directory, mountpoint]
      umount = proc "umount" [mountpoint]
      umountLazy = proc "umount" ["-l", mountpoint]

      pre = liftIO $ do -- hPutStrLn stderr $ "mounting /proc at " ++ show mountpoint
                        createDirectoryIfMissing True mountpoint
                        readProcess mount L.empty

      post = liftIO $ do -- hPutStrLn stderr $ "unmounting /proc at " ++ show mountpoint
                         readProcess umount L.empty
                           `catch` (\ (e :: IOError) ->
                                        do hPutStrLn stderr ("Exception unmounting " ++ mountpoint ++ ", trying -l: " ++ show e)
                                           readProcess umountLazy L.empty)

-- | Monad transformer to ensure that /proc and /sys are mounted
-- during a computation.
newtype WithProcAndSys m a = WithProcAndSys { runWithProcAndSys :: m a } deriving (Functor, Monad, Applicative)

instance MonadTrans WithProcAndSys where
    lift = WithProcAndSys

instance MonadIO m => MonadIO (WithProcAndSys m) where
    liftIO task = WithProcAndSys (liftIO task)

-- | Mount /proc and /sys in the specified build root and execute a
-- task.  Typically, the task would start with a chroot into the build
-- root.  If the build root given is "/" it is assumed that the file
-- systems are already mounted, no mounting or unmounting is done.
withProcAndSys :: (MonadIO m, MonadMask m) => FilePath -> WithProcAndSys m a -> m a
withProcAndSys "/" task = runWithProcAndSys task
withProcAndSys root task = do
  exists <- liftIO $ doesDirectoryExist root
  case exists of
    True -> withMount "/proc" (root </> "proc") $ withMount "/sys" (root </> "sys") $ runWithProcAndSys task
    False -> liftIO $ ioError $ mkIOError doesNotExistErrorType "chroot directory does not exist" Nothing (Just root)

-- | Do an IO task with /tmp remounted.  This could be used
-- to share /tmp with a build root.
withTmp :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withTmp root task = withMount "/tmp" (root </> "tmp") task
