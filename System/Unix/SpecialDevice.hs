{-# LANGUAGE PatternSignatures #-}
-- | Construct an ADT representing block and character devices
-- (but mostly block devices) by interpreting the contents of
-- the Linux sysfs filesystem.
module System.Unix.SpecialDevice 
    (SpecialDevice,
     sysMountPoint,	-- IO String
     ofNode,		-- FilePath -> IO (Maybe SpecialDevice)
     ofNodeStatus,	-- FileStatus -> Maybe SpecialDevice
     ofPath,		-- FilePath -> IO (Maybe SpecialDevice)
     rootPart,		-- IO (Maybe SpecialDevice)
     ofDevNo,		-- (DeviceID -> SpecialDevice) -> Int -> SpecialDevice
     ofSysName,		-- String -> IO (Maybe SpecialDevice)
     ofSysPath,		-- (DeviceID -> SpecialDevice) -> FilePath -> IO (Maybe SpecialDevice)
     toDevno,		-- SpecialDevice -> Int
     --major,		-- SpecialDevice -> Int
     --minor,		-- SpecialDevice -> Int
     ofMajorMinor,	-- (DeviceID -> SpecialDevice) -> Int -> Int -> SpecialDevice
     node,		-- SpecialDevice -> IO (Maybe FilePath)
     nodes,		-- SpecialDevice -> IO [FilePath]
     sysName,		-- SpecialDevice -> IO (Maybe String)
     splitPart,		-- String -> (String, Int)
     sysDir,		-- SpecialDevice -> IO (Maybe FilePath)
     diskOfPart,	-- SpecialDevice -> IO (Maybe SpecialDevice)
     getAllDisks,	-- IO [SpecialDevice]
     getAllPartitions,	-- IO [SpecialDevice]
     getAllCdroms,	-- IO [SpecialDevice]
     getAllRemovable,	-- IO [SpecialDevice]
--     toDevName,
--     getBlkidAlist,
--     getBlkidInfo,
--     deviceOfUuid,
--     devicesOfLabel,
--     updateBlkidFns,
--     update
    )
    where

import Control.OldException
import System.IO
import System.Directory
import Data.Char
import Data.List
import Data.Maybe
import System.FilePath
import System.Posix.Types
import System.Posix.Files
import System.Posix.User
import Text.Regex

data SpecialDevice =
    BlockDevice DeviceID | CharacterDevice DeviceID
    deriving (Show, Ord, Eq)

-- | FIXME: We should really get this value from the mount table.
sysMountPoint :: FilePath
sysMountPoint = "/sys"

ofPath :: FilePath -> IO (Maybe SpecialDevice)
ofPath path =
    -- Catch the exception thrown on an invalid symlink
    (try $ getFileStatus path) >>=
    return . either (const Nothing) (Just . BlockDevice . deviceID)

rootPart :: IO (Maybe SpecialDevice)
rootPart = ofPath "/"

-- | Return the device represented by a device node, such as \/dev\/sda2.
-- Returns Nothing if there is an exception trying to stat the node, or
-- if the node turns out not to be a special device.
ofNode :: FilePath -> IO (Maybe SpecialDevice)
ofNode "/dev/root" = ofPath "/"
ofNode node = (try $ getFileStatus node) >>= return . either (const Nothing) ofNodeStatus

ofNodeStatus :: FileStatus -> Maybe SpecialDevice
ofNodeStatus status =
    if isBlockDevice status then
        (Just . BlockDevice . specialDeviceID $ status) else
        if isCharacterDevice status then
            (Just . CharacterDevice . specialDeviceID $ status) else
            Nothing    

ofSysName :: String -> IO (Maybe SpecialDevice)
ofSysName name =
    do
      paths <- directory_find False (sysMountPoint ++ "/block") >>= return . map fst . filter isDev
      case filter (\ x -> basename (dirname x) == name) paths of
        [path] -> ofSysPath BlockDevice (dirname path)
    where
      isDev (path, status) = basename path == "dev"

ofSysPath :: (DeviceID -> SpecialDevice) -> FilePath -> IO (Maybe SpecialDevice)
ofSysPath typ path = readFile (path ++ "/dev") >>= return . parseSysDevFile typ

parseSysDevFile :: (DeviceID -> SpecialDevice) -> String -> Maybe SpecialDevice
parseSysDevFile typ text =
    case filter (all isDigit) . groupBy (\ a b -> isDigit a && isDigit b) $ text of
      [major, minor] -> Just (ofMajorMinor typ (read major) (read minor))
      _ -> Nothing

ofMajorMinor :: (DeviceID -> SpecialDevice) -> Int -> Int -> SpecialDevice
ofMajorMinor typ major minor = ofDevNo typ $ major * 256 + minor

ofDevNo :: (DeviceID -> SpecialDevice) -> Int -> SpecialDevice
ofDevNo typ n = typ . fromInteger . toInteger $ n

{-
major :: SpecialDevice -> Integer
major dev = toInteger (toDevno dev)
minor :: SpecialDevice -> Int
minor dev = mod (fromInteger (toInteger (toDevno dev))) 256
-}
toDevno :: SpecialDevice -> DeviceID
toDevno (BlockDevice n) = n
toDevno (CharacterDevice n) = n

node :: SpecialDevice -> IO (Maybe FilePath)
node dev@(BlockDevice _) = nodes dev >>= return . listToMaybe

nodes :: SpecialDevice -> IO [FilePath]
nodes dev@(BlockDevice _) =
    do
      pairs <- directory_find True "/dev" >>=
               return .
                      filter (not . isPrefixOf "/dev/.static/" . fst) .
                             filter (not . isPrefixOf "/dev/.udevdb/" . fst)
      let pairs' = filter (\ (node, status) -> (ofNodeStatus status) == Just dev) pairs
      return . map fst $ pairs'
    where
      mapSnd f (a, b) = (a, f b)

splitPart :: String -> (String, Int)
splitPart name =
    mapSnd read (break isDigit name)
    where mapSnd f (a, b) = (a, f b)

diskOfPart :: SpecialDevice -> IO (Maybe SpecialDevice)
diskOfPart part =
    sysName part >>=
    return . maybe Nothing (Just . fst . splitPart) >>=
    maybe (return Nothing) ofSysName

sysName :: SpecialDevice -> IO (Maybe String)
sysName dev = sysDir dev >>= return . maybe Nothing (Just . basename)

sysDir :: SpecialDevice -> IO (Maybe FilePath)
sysDir dev@(BlockDevice _) = 
    do
      (pairs' :: [(FilePath, FileStatus)]) <- directory_find False (sysMountPoint ++ "/block")
      let (paths :: [FilePath]) = map fst . filter isDev $ pairs'
      devs <- mapM readFile paths >>= return . map (parseSysDevFile BlockDevice)
      let pairs = zip devs (map dirname paths)
      return . lookup (Just dev) $ pairs
    where
      isDev (path, status) = basename path == "dev"

diskGroup :: IO GroupID
diskGroup = getGroupEntryForName "disk" >>= return . groupID

cdromGroup :: IO GroupID
cdromGroup = getGroupEntryForName "cdrom" >>= return . groupID

-- | Removable devices, such as USB keys, are in this group.
floppyGroup :: IO GroupID
floppyGroup = getGroupEntryForName "floppy" >>= return . groupID

getDisksInGroup :: GroupID -> IO [SpecialDevice]
getDisksInGroup group =
    directory_find True "/dev/disk/by-path" >>=
    return . filter (inGroup group) >>=
    return . catMaybes . map (ofNodeStatus . snd)
    where
      inGroup group (_, status) = fileGroup status == group

getAllDisks :: IO [SpecialDevice]
getAllDisks =
    do
      group <- diskGroup
      devs <- directory_find True "/dev/disk/by-path" >>=
              return . filter (not . isPart) . filter (inGroup group) >>=
              return . map (ofNodeStatus . snd)
      return (catMaybes devs)
    where
      inGroup group (_, status) = fileGroup status == group
      isPart (path, _) = maybe False (const True) (matchRegex (mkRegex "-part[0-9]+$") path)

getAllPartitions :: IO [SpecialDevice]
getAllPartitions =
    directory_find True "/dev/disk/by-path" >>= return . filter isPart >>= return . catMaybes . map (ofNodeStatus . snd)
    where
      isPart (path, _) = maybe False (const True) (matchRegex (mkRegex "-part[0-9]+$") path)

getAllCdroms :: IO [SpecialDevice]
getAllCdroms = cdromGroup >>= getDisksInGroup 

getAllRemovable :: IO [SpecialDevice]
getAllRemovable = floppyGroup >>= getDisksInGroup 

-- ofNode "/dev/sda1" >>= maybe (return Nothing) sysDir >>= putStrLn . show
-- -> Just "/sys/block/sda/sda1/dev"

-- | Traverse a directory and return a list of all the (path,
-- fileStatus) pairs.
directory_find :: Bool -> FilePath -> IO [(FilePath, FileStatus)]
directory_find follow path =
    do
      maybeStatus <- 
          if follow then
              -- Catch the exception exception thrown on an invalid symlink
              try . getFileStatus $ path else
              getSymbolicLinkStatus path >>= return . Right
      case maybeStatus of
        Left _ -> return []
        Right status ->
            case isDirectory status of
              True -> 
                  do
                    -- Catch the exception thrown if we lack read permission
                    subs <- (try $ getDirectoryContents path) >>=
                            return . either (const []) id >>=
                            return . map (path </>) . filter (not . flip elem [".", ".."]) >>=
                            mapM (directory_find follow) >>=
                            return . concat
                    return $ (path, status) : subs
              False ->
                  return [(path, status)]

dirname path = reverse . tail . snd . break (== '/') . reverse $ path
basename path = reverse . fst . break (== '/') . reverse $ path
