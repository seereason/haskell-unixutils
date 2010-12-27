-- |functions for mounting, umounting, parsing \/proc\/mounts, etc
module System.Unix.Mount 
    (umountBelow,	-- FilePath -> IO [(FilePath, (String, String, ExitCode))]
     umount,		-- [String] -> IO (String, String, ExitCode)
     isMountPoint)	-- FilePath -> IO Bool
    where

-- Standard GHC modules

import Control.Monad
import Data.ByteString.Lazy.Char8 (empty)
import Data.List
import System.Directory
import System.Exit
import System.Posix.Files
import System.Unix.Process

-- Local Modules

import System.Unix.Process

-- In ghc610 readFile "/proc/mounts" hangs.  Use this instead.
rf path = lazyCommand ("cat '" ++ path ++ "'") empty >>= return . (\ (o, _, _) -> o) . collectOutputUnpacked

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
umountBelow :: FilePath -- ^ canonicalised, absolute path
            -> IO [(FilePath, (String, String, ExitCode))] -- ^ paths that we attempted to umount, and the responding output from the umount command
umountBelow belowPath =
    do procMount <- rf "/proc/mounts"
       let mountPoints = map (unescape . (!! 1) . words) (lines procMount)
           maybeMounts = filter (isPrefixOf belowPath) (concat (map tails mountPoints))
       needsUmount <- filterM isMountPoint maybeMounts
       result <- mapM (\path -> umount [path,"-f","-l"] >>= return . ((,) path)) needsUmount
       -- Did /proc/mounts change?  If so we should try again because
       -- nested mounts might have been revealed.
       procMount' <- rf "/proc/mounts"
       result' <- if procMount /= procMount' then
                      umountBelow belowPath else
                      return []
       return $ result ++ result'

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

-- |'escape' - \/proc\/mount stytle string escaper
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
umount :: [String] -> IO (String, String, ExitCode)
umount args = lazyProcess "umount" args Nothing Nothing empty >>= return . collectOutputUnpacked

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
