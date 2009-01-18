{-# LANGUAGE ScopedTypeVariables #-}
-- |functions for killing processes, running processes, etc
module System.Unix.Process
    (
    -- * Strict process running
      simpleProcess	-- FilePath -> [String] -> IO (String, String, ExitCode)
    , processResult	-- FilePath -> [String] -> IO (Either Int (String, String))
    , processOutput	-- FilePath -> [String] -> IO (Either Int String)
    , simpleCommand	-- String -> IO (String, String, ExitCode)
    , commandResult	-- String -> IO (Either Int (String, String))
    , commandOutput	-- String -> IO (Either Int String)
    -- * Lazy process running
    , Process
    , Output(Stdout, Stderr, Result)
    , lazyRun		-- L.ByteString -> Process -> IO [Output]
    , lazyCommand	-- String -> IO [Output]
    , lazyProcess	-- FilePath -> [String] -> Maybe FilePath
			--     -> Maybe [(String, String)] -> IO [Output]
    , stdoutOnly	-- [Output] -> L.ByteString
    , stderrOnly	-- [Output] -> L.ByteString
    , outputOnly	-- [Output] -> L.ByteString
    , checkResult
    , discardStdout
    , discardStderr
    , discardOutput
    , mergeToStderr
    , mergeToStdout
    , collectStdout
    , collectStderr
    , collectOutput
    , collectOutputUnpacked
    , ExitCode(ExitSuccess, ExitFailure)
    , exitCodeOnly	-- [Output] -> [ExitCode]
    , hPutNonBlocking	-- Handle -> B.ByteString -> IO Int
    -- * Process killing
    , killByCwd		-- FilePath -> IO [(String, Maybe String)]
    ) where

import Control.Monad
import Control.Exception hiding (catch)
import Control.Parallel.Strategies
import Data.Char
import qualified Data.ByteString as B
--import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
--import qualified Data.ByteString.Internal as I
import Data.ByteString.Internal(toForeignPtr)	-- for hPutNonBlocking only
import Data.List
import Data.Word
import Data.Int
import System.Process
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Exit
import System.Posix.Files
import System.Posix.Signals
import System.Posix.Unistd (usleep)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (plusPtr)			-- for hPutNonBlocking only
import Foreign.ForeignPtr (withForeignPtr)	-- for hPutNonBlocking only

{-
NOTE:

+ We should make sure this works if we are inside a chroot.

+ path needs to be absolute or we might kill processes living in
  similar named, but different directories.

+ path is an canoncialised, absolute path, such as what realpath returns

-}
-- | Kill the processes whose working directory is in or under the
-- given directory.
killByCwd :: FilePath -> IO [(String, Maybe String)]
killByCwd path =
    do pids <- liftM (filter (all isDigit)) (getDirectoryContents "/proc")
       cwdPids <- filterM (isCwd path) pids
       exePaths <- mapM exePath cwdPids
       mapM_ kill cwdPids
       return (zip cwdPids exePaths)
    where
      isCwd :: FilePath -> String -> IO Bool
      isCwd cwd pid =
          catch (liftM (isPrefixOf cwd) (readSymbolicLink ("/proc/" ++ pid ++"/cwd"))) (const (return False))
      exePath :: String -> IO (Maybe String)
      exePath pid = catch (readSymbolicLink ("/proc/" ++ pid ++"/exe") >>= return . Just) (const (return Nothing))
      kill :: String -> IO ()
      kill pidStr = signalProcess sigTERM (read pidStr)

-- |'simpleProcess' - run a process returning (stdout, stderr, exitcode)
--
-- /Warning/ - stdout and stderr will be read strictly so that we do
-- not deadlock when trying to check the exitcode. Do not try doing
-- something like, @simpleProcess [\"yes\"]@
--
-- NOTE: this may still dead-lock because we first strictly read
-- outStr and then errStr. Perhaps we should use forkIO or something?
simpleProcess :: FilePath -> [String] -> IO (String, String, ExitCode)
simpleProcess exec args =
    do (inp,out,err,pid) <- runInteractiveProcess exec args Nothing Nothing
       hClose inp
       outStr <- hGetContents out
       errStr <- hGetContents err
       evaluate (rnf outStr) -- read output strictly
       evaluate (rnf errStr) -- read stderr strictly
       ec <- waitForProcess pid
       return (outStr, errStr, ec)

processResult :: FilePath -> [String] -> IO (Either Int (String, String))
processResult exec args =
    simpleProcess exec args >>= return . resultOrCode
    where
      resultOrCode (_, _, ExitFailure n) = Left n
      resultOrCode (out, err, ExitSuccess) = Right (out, err)

processOutput :: FilePath -> [String] -> IO (Either Int String)
processOutput exec args =
    simpleProcess exec args >>= return . outputOrCode
    where
      outputOrCode (_, _, ExitFailure n) = Left n
      outputOrCode (out, _, ExitSuccess) = Right out

simpleCommand :: String -> IO (String, String, ExitCode)
simpleCommand cmd =
    do (inp,out,err,pid) <- runInteractiveCommand cmd
       hClose inp
       outStr <- hGetContents out
       errStr <- hGetContents err
       evaluate (rnf outStr) -- read output strictly
       evaluate (rnf errStr) -- read stderr strictly
       ec <- waitForProcess pid
       return (outStr, errStr, ec)

commandResult :: String -> IO (Either Int (String, String))
commandResult cmd =
    simpleCommand cmd >>= return . resultOrCode
    where
      resultOrCode (_, _, ExitFailure n) = Left n
      resultOrCode (out, err, ExitSuccess) = Right (out, err)

commandOutput :: String -> IO (Either Int String)
commandOutput cmd =
    simpleCommand cmd >>= return . outputOrCode
    where
      outputOrCode (_, _, ExitFailure n) = Left n
      outputOrCode (out, _, ExitSuccess) = Right out

{- Functions to run a process and return a lazy list of chunks from
   standard output, standard error, and at the end of the list an
   object indicating the process result code.  If neither output
   handle is ready for reading the process sleeps and tries again,
   with the sleep intervals increasing from 8 microseconds to a
   maximum of 0.1 seconds. -}

-- | This is the type returned by 'System.Process.runInteractiveProcess' et. al.
type Process = (Handle, Handle, Handle, ProcessHandle)

-- | The process returns a list of objects of type 'Output'.  There will be
-- one Result object at the end of the list (if the list has an end.)
data Output
    = Stdout B.ByteString
    | Stderr B.ByteString
    | Result ExitCode
      deriving Show

-- | Display up to thirty characters of a ByteString followed by an
-- ellipsis if some of it was omitted.
showBrief :: B.ByteString -> String
showBrief s = 
    let l = B.length s in
    show (B.take (min 30 l) s) ++
         if l > 30 then " ... (" ++ show (l - 30) ++ " additional bytes)" else ""

{-
instance Show Output where
    show (Stdout s) =
        let l = B.length s in
        "Stdout: " ++ showBrief s
    show (Stderr s) =
        let l = B.length s in
        "Stderr: " ++ showBrief s
    -- show (Sleep n) = "Slept " ++ show n ++ " usec"
    show (Result e) = show e
-}

bufSize = 65536		-- maximum chunk size
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs = 100000	-- maximum wait time (microseconds)

--stringToByteString = B.pack . map (fromInteger . toInteger . ord)

-- | Debugging output
ePut :: Int -> String -> IO ()
ePut minv s = if curv >= minv then hPutStr stderr s else return ()
ePut0 = ePut 0
ePut1 = ePut 1
ePut2 = ePut 2

-- | Current verbosity level.
curv = 0

-- | Create a process with 'runInteractiveCommand' and run it with 'lazyRun'.
lazyCommand :: String -> L.ByteString -> IO [Output]
lazyCommand cmd input = runInteractiveCommand cmd >>= lazyRun input

-- | Create a process with 'runInteractiveProcess' and run it with 'lazyRun'.
lazyProcess :: FilePath -> [String] -> Maybe FilePath
            -> Maybe [(String, String)] -> L.ByteString -> IO [Output]
lazyProcess exec args cwd env input =
    runInteractiveProcess exec args cwd env >>= lazyRun input

-- | Take the tuple like that returned by 'runInteractiveProcess',
-- create a process, send the list of inputs to its stdin and return
-- the lazy list of 'Output' objects.
lazyRun :: L.ByteString -> Process -> IO [Output]
lazyRun input (inh, outh, errh, pid) =
    elements (L.toChunks input, Just inh, Just outh, Just errh, [])
    where
      elements :: ([B.ByteString], Maybe Handle, Maybe Handle, Maybe Handle, [Output]) -> IO [Output]
      -- EOF on both output descriptors, get exit code
      elements (_, _, Nothing, Nothing, elems) =
          do result <- waitForProcess pid
             return $ elems ++ [Result result]
      -- The available output has been processed, send input and read
      -- from the ready handles
      elements tl@(_, _, _, _, []) = ready uSecs tl >>= elements
      -- Add some output to the result value
      elements (input, inh, outh, errh, elems) =
          do
            etc <- unsafeInterleaveIO (elements (input, inh, outh, errh, []))
            return $ elems ++ etc

-- A quick fix for the issue where hWaitForInput has actually started
-- raising the isEOFError exception in ghc 6.10.
data Readyness = Ready | Unready | EndOfFile

hReady' :: Handle -> IO Readyness
hReady' h = (hReady h >>= (\ flag -> return (if flag then Ready else Unready))) `catch` (\ e -> return EndOfFile)

-- | Wait until at least one handle is ready and then write input or
-- read output.  Note that there is no way to check whether the input
-- handle is ready except to try to write to it and see if any bytes
-- are accepted.  If no input is accepted, or the input handle is
-- already closed, and none of the output descriptors are ready for
-- reading the function sleeps and tries again.
ready :: Int -> ([B.ByteString], Maybe Handle, Maybe Handle, Maybe Handle, [Output])
      -> IO ([B.ByteString], Maybe Handle, Maybe Handle, Maybe Handle, [Output])
ready waitUSecs (input, inh, outh, errh, elems) =
    do
      outReady <- maybe (return Unready) hReady' outh
      errReady <- maybe (return Unready) hReady' errh
      case (input, inh, outReady, errReady) of
        -- Input exhausted, close the input handle.
        ([], Just handle, Unready, Unready) ->
            do hClose handle
               ready  waitUSecs ([], Nothing, outh, errh, elems)
        -- Input handle closed and there are no ready output handles,
        -- wait a bit
        ([], Nothing, Unready, Unready) ->
            do usleep uSecs
               --ePut0 ("Slept " ++ show uSecs ++ " microseconds\n")
               ready (min maxUSecs (2 * waitUSecs)) (input, inh, outh, errh, elems)
        -- Input is available and there are no ready output handles
        (input : etc, Just handle, Unready, Unready)
            -- Discard a zero byte input
            | input == B.empty -> ready waitUSecs (etc, inh, outh, errh, elems)
            -- Send some input to the process
            | True ->
                do count' <- hPutNonBlocking handle input >>= return . fromInteger . toInteger
                   case count' of
                     -- Input buffer is full too, sleep.
                     0 -> do usleep uSecs
                             ready (min maxUSecs (2 * waitUSecs)) (input : etc, inh, outh, errh, elems)
                     -- We wrote some input, discard it and continue
                     n -> do let input' = B.drop count' input : etc
                             return (input', Just handle, outh, errh, elems)
        -- One or both output handles are ready, try to read from them
        _ ->
            do (out1, errh') <- nextOut errh errReady Stderr
               (out2, outh') <- nextOut outh outReady Stdout
               return (input, inh, outh', errh', elems ++ out1 ++ out2)

-- | Return the next output element and the updated handle
-- from a handle which is assumed ready.
nextOut :: (Maybe Handle) -> Readyness -> (B.ByteString -> Output) -> IO ([Output], Maybe Handle)
nextOut Nothing _ _ = return ([], Nothing)	-- Handle is closed
nextOut _ EndOfFile _ = return ([], Nothing)	-- Handle is closed
nextOut handle Unready _ = return ([], handle)	-- Handle is not ready
nextOut (Just handle) Ready constructor =	-- Perform a read 
    do
      a <- B.hGetNonBlocking handle bufSize
      case B.length a of
        -- A zero length read, unlike a zero length write, always
        -- means EOF.
        0 -> do hClose handle
                return ([], Nothing)
        -- Got some input
        n -> return ([constructor a], Just handle)

-- | Filter everything except stdout from the output list.
stdoutOnly :: [Output] -> L.ByteString
stdoutOnly out =
    L.fromChunks $ f out
    where 
      f (Stdout s : etc) = s : f etc
      f (_ : etc) = f etc
      f [] = []

-- | Filter everything except stderr from the output list.
stderrOnly :: [Output] -> L.ByteString
stderrOnly out =
    L.fromChunks $ f out
    where
      f (Stderr s : etc) = s : f etc
      f (_ : etc) = f etc
      f [] = []

-- | Filter the exit codes output list and merge the two output
-- streams in the order they appear.
outputOnly :: [Output] -> L.ByteString
outputOnly out =
    L.fromChunks $ f out
    where
      f (Stderr s : etc) = s : f etc
      f (Stdout s : etc) = s : f etc
      f (_ : etc) = f etc
      f [] = []

-- | Filter everything except the exit code from the output list.
exitCodeOnly :: [Output] -> [ExitCode]
exitCodeOnly (Result code : etc) = code : exitCodeOnly etc
exitCodeOnly (_ : etc) = exitCodeOnly etc
exitCodeOnly [] = []

-- | This belongs in Data.ByteString.  See ticket 1070,
-- <http://hackage.haskell.org/trac/ghc/ticket/1070>.
hPutNonBlocking :: Handle -> B.ByteString -> IO Int64
hPutNonBlocking h b =
    case toForeignPtr b of
      (_, _, 0) -> return 0
      (ps, s, l) -> withForeignPtr ps $ \ p-> hPutBufNonBlocking h (p `plusPtr` s) l >>= return . fromInteger . toInteger

-- Examples:
--
-- > runInteractiveCommand "ls -l /usr/bin" >>= lazyRun [] >>= mapM_ (putStrLn . show)
-- Stdout: \"total 137411\n-rwxr-xr-x 1 root\" ... (4066 additional bytes)
-- Stdout: \"oot       7642 2006-12-07 17:0\" ... (65506 additional bytes)
-- Stdout: \"oot      57220 2006-10-24 01:1\" ... (31961 additional bytes)
-- ExitSuccess
--
-- > lazyCommand "cat -n" (map stringToByteString ["abc\n", "def\n"]) >>= mapM_ (putStrLn . show)
--
-- > lazyCommand "yes" [] >>= return . stdoutOnly >>= lazyCommand "cat -n" >>= mapM_ (putStrLn . show)


checkResult :: (Int -> a) -> a -> [Output] -> a
checkResult _ _ [] = error $ "*** FAILURE: Missing exit code"
checkResult _ onSuccess (Result ExitSuccess : _) = onSuccess
checkResult onFailure _ (Result (ExitFailure n) : _) = onFailure n
checkResult onFailure onSuccess (_ : more) = checkResult onFailure onSuccess more

discardStdout :: [Output] -> [Output]
discardStdout (Stdout _ : more) = discardStdout more
discardStdout (x : more) = x : discardStdout more
discardStdout [] = []

discardStderr :: [Output] -> [Output]
discardStderr (Stderr _ : more) = discardStderr more
discardStderr (x : more) = x : discardStderr more
discardStderr [] = []

discardOutput :: [Output] -> [Output]
discardOutput = discardStdout . discardStderr

-- |Turn all the Stdout text into Stderr, preserving the order.
mergeToStderr :: [Output] -> [Output]
mergeToStderr output =
    map merge output
    where
      merge (Stdout s) = Stderr s
      merge x = x

-- |Turn all the Stderr text into Stdout, preserving the order.
mergeToStdout :: [Output] -> [Output]
mergeToStdout output =
    map merge output
    where
      merge (Stderr s) = Stdout s
      merge x = x

-- |Split out and concatenate Stdout
collectStdout :: [Output] -> (L.ByteString, [Output])
collectStdout output =
    (L.fromChunks out, other)
    where
      (out, other) = foldr collect ([], []) output
      collect (Stdout s) (text, result) = (s : text, result)
      collect x (text, result) = (text, x : result)

-- |Split out and concatenate Stderr
collectStderr :: [Output] -> (L.ByteString, [Output])
collectStderr output =
    (L.fromChunks err, other)
    where
      (err, other) = foldr collect ([], []) output
      collect (Stderr s) (text, result) = (s : text, result)
      collect x (text, result) = (text, x : result)

-- |Split out and concatenate both Stdout and Stderr, leaving only the exit code.
collectOutput :: [Output] -> (L.ByteString, L.ByteString, [ExitCode])
collectOutput output =
    (L.fromChunks out, L.fromChunks err, code)
    where
      (out, err, code) = foldr collect ([], [], []) output
      collect (Stdout s) (out, err, result) = (s : out, err, result)
      collect (Stderr s) (out, err, result) = (out, s : err, result)
      collect (Result r) (out, err, result) = (out, err, r : result)

-- |Collect all output, unpack and concatenate.
collectOutputUnpacked :: [Output] -> (String, String, [ExitCode])
collectOutputUnpacked =
    unpack . collectOutput
    where unpack (out, err, result) = (L.unpack out, L.unpack err, result)
