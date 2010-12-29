{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures -Werror #-}
-- |functions for killing processes, running processes, etc
module System.Unix.Process
    (
    -- * Lazy process running
      Process
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
    , collectResult
    , ExitCode(ExitSuccess, ExitFailure)
    , exitCodeOnly	-- [Output] -> ExitCode
    , hPutNonBlocking	-- Handle -> B.ByteString -> IO Int
    -- * Process killing
    , killByCwd		-- FilePath -> IO [(String, Maybe String)]
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (liftM, filterM)
--import Control.Exception hiding (catch)
--import Control.Parallel.Strategies (rnf)
import Data.Char (isDigit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Internal(toForeignPtr)	-- for hPutNonBlocking only
import Data.List (isPrefixOf, partition)
import Data.Int (Int64)
import qualified GHC.IO.Exception as E
import System.Process (ProcessHandle, waitForProcess, runInteractiveProcess, runInteractiveCommand)
import System.IO (Handle, hSetBinaryMode, hReady, hPutBufNonBlocking, hClose {-, hGetContents-})
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Posix.Files (readSymbolicLink)
import System.Posix.Signals (signalProcess, sigTERM)

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

{- Functions to run a process and return a lazy list of chunks from
   standard output, standard error, and at the end of the list an
   object indicating the process result code.  If neither output
   handle is ready for reading the process sleeps and tries again,
   with the sleep intervals increasing from 8 microseconds to a
   maximum of 0.1 seconds. -}

-- | This is the type returned by 'System.Process.runInteractiveProcess' et. al.
type Process = (Handle, Handle, Handle, ProcessHandle)

-- | The lazyCommand, lazyProcess and lazyRun functions each return a
-- list of 'Output'.  There will generally be one Result value at or
-- near the end of the list (if the list has an end.)
data Output
    = Stdout B.ByteString
    | Stderr B.ByteString
    | Result ExitCode
      deriving Show

-- |An opaque type would give us additional type safety to ensure the
-- semantics of 'exitCodeOnly'.
type Outputs = [Output]

bufSize = 65536		-- maximum chunk size
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs = 100000	-- maximum wait time (microseconds)

-- | Create a process with 'runInteractiveCommand' and run it with 'lazyRun'.
lazyCommand :: String -> L.ByteString -> IO Outputs
lazyCommand cmd input = runInteractiveCommand cmd >>= lazyRun input

-- | Create a process with 'runInteractiveProcess' and run it with 'lazyRun'.
lazyProcess :: FilePath -> [String] -> Maybe FilePath
            -> Maybe [(String, String)] -> L.ByteString -> IO Outputs
lazyProcess exec args cwd env input =
    runInteractiveProcess exec args cwd env >>= lazyRun input

-- | Take the tuple like that returned by 'runInteractiveProcess',
-- create a process, send the list of inputs to its stdin and return
-- the lazy list of 'Output' objects.
lazyRun :: L.ByteString -> Process -> IO Outputs
lazyRun input (inh, outh, errh, pid) =
    hSetBinaryMode inh True >>
    hSetBinaryMode outh True >>
    hSetBinaryMode errh True >>
    elements (L.toChunks input, Just inh, Just outh, Just errh, [])
    where
      elements :: ([B.ByteString], Maybe Handle, Maybe Handle, Maybe Handle, Outputs) -> IO Outputs
      -- EOF on both output descriptors, get exit code.  It can be
      -- argued that the list will always contain exactly one exit
      -- code if traversed to its end, because the only case of
      -- elements that does not recurse is the one that adds a Result,
      -- and there is nowhere else where a Result is added.  However,
      -- the process doing the traversing may die before that end is
      -- reached.
      elements (_, _, Nothing, Nothing, elems) =
          do result <- waitForProcess pid
             -- Note that there is no need to insert the result code
             -- at the end of the list.
             return $ Result result : elems
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
hReady' h = (hReady h >>= (\ flag -> return (if flag then Ready else Unready))) `catch` (\ (e :: IOError) ->
                                                                                             case E.ioe_type e of
                                                                                               E.EOF -> return EndOfFile
                                                                                               _ -> error (show e))

-- | Wait until at least one handle is ready and then write input or
-- read output.  Note that there is no way to check whether the input
-- handle is ready except to try to write to it and see if any bytes
-- are accepted.  If no input is accepted, or the input handle is
-- already closed, and none of the output descriptors are ready for
-- reading the function sleeps and tries again.
ready :: Int -> ([B.ByteString], Maybe Handle, Maybe Handle, Maybe Handle, Outputs)
      -> IO ([B.ByteString], Maybe Handle, Maybe Handle, Maybe Handle, Outputs)
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
            do threadDelay waitUSecs
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
                     0 -> do threadDelay uSecs
                             ready (min maxUSecs (2 * waitUSecs)) (input : etc, inh, outh, errh, elems)
                     -- We wrote some input, discard it and continue
                     _n -> do let input' = B.drop count' input : etc
                              return (input', Just handle, outh, errh, elems)
        -- One or both output handles are ready, try to read from them
        _ ->
            do (out1, errh') <- nextOut errh errReady Stderr
               (out2, outh') <- nextOut outh outReady Stdout
               return (input, inh, outh', errh', elems ++ out1 ++ out2)

-- | Return the next output element and the updated handle
-- from a handle which is assumed ready.
nextOut :: (Maybe Handle) -> Readyness -> (B.ByteString -> Output) -> IO (Outputs, Maybe Handle)
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
        _n -> return ([constructor a], Just handle)

-- | Filter everything except stdout from the output list.
stdoutOnly :: Outputs -> L.ByteString
stdoutOnly out =
    L.fromChunks $ f out
    where 
      f (Stdout s : etc) = s : f etc
      f (_ : etc) = f etc
      f [] = []

-- | Filter everything except stderr from the output list.
stderrOnly :: Outputs -> L.ByteString
stderrOnly out =
    L.fromChunks $ f out
    where
      f (Stderr s : etc) = s : f etc
      f (_ : etc) = f etc
      f [] = []

-- | Filter the exit codes output list and merge the two output
-- streams in the order they appear.
outputOnly :: Outputs -> L.ByteString
outputOnly out =
    L.fromChunks $ f out
    where
      f (Stderr s : etc) = s : f etc
      f (Stdout s : etc) = s : f etc
      f (_ : etc) = f etc
      f [] = []

-- | Filter everything except the exit code from the output list.  See
-- discussion in 'lazyRun' of why we are confident that the list will
-- contain exactly one 'Result'.  An opaque type to hold the 'Output'
-- list would lend additional safety here.
exitCodeOnly :: Outputs -> ExitCode
exitCodeOnly (Result code : _) = code
exitCodeOnly (_ : etc) = exitCodeOnly etc
exitCodeOnly [] = error "exitCodeOnly - no Result found"

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


checkResult :: (Int -> a) -> a -> Outputs -> a
checkResult _ _ [] = error $ "*** FAILURE: Missing exit code"
checkResult _ onSuccess (Result ExitSuccess : _) = onSuccess
checkResult onFailure _ (Result (ExitFailure n) : _) = onFailure n
checkResult onFailure onSuccess (_ : more) = checkResult onFailure onSuccess more

discardStdout :: Outputs -> Outputs
discardStdout (Stdout _ : more) = discardStdout more
discardStdout (x : more) = x : discardStdout more
discardStdout [] = []

discardStderr :: Outputs -> Outputs
discardStderr (Stderr _ : more) = discardStderr more
discardStderr (x : more) = x : discardStderr more
discardStderr [] = []

discardOutput :: Outputs -> Outputs
discardOutput = discardStdout . discardStderr

-- |Turn all the Stdout text into Stderr, preserving the order.
mergeToStderr :: Outputs -> Outputs
mergeToStderr output =
    map merge output
    where
      merge (Stdout s) = Stderr s
      merge x = x

-- |Turn all the Stderr text into Stdout, preserving the order.
mergeToStdout :: Outputs -> Outputs
mergeToStdout output =
    map merge output
    where
      merge (Stderr s) = Stdout s
      merge x = x

-- |Split out and concatenate Stdout
collectStdout :: Outputs -> (L.ByteString, Outputs)
collectStdout output =
    (L.fromChunks out, other)
    where
      (out, other) = foldr collect ([], []) output
      collect (Stdout s) (text, result) = (s : text, result)
      collect x (text, result) = (text, x : result)

-- |Split out and concatenate Stderr
collectStderr :: Outputs -> (L.ByteString, Outputs)
collectStderr output =
    (L.fromChunks err, other)
    where
      (err, other) = foldr collect ([], []) output
      collect (Stderr s) (text, result) = (s : text, result)
      collect x (text, result) = (text, x : result)

-- |Split out and concatenate both Stdout and Stderr, leaving only the exit code.
collectOutput :: Outputs -> (L.ByteString, L.ByteString, ExitCode)
collectOutput output =
    (L.fromChunks out, L.fromChunks err, code)
    where
      (out, err, code) = foldr collect ([], [], ExitFailure 666) output
      collect (Stdout s) (out, err, result) = (s : out, err, result)
      collect (Stderr s) (out, err, result) = (out, s : err, result)
      collect (Result result) (out, err, _) = (out, err, result)

-- |Collect all output, unpack and concatenate.
collectOutputUnpacked :: Outputs -> (String, String, ExitCode)
collectOutputUnpacked =
    unpack . collectOutput
    where unpack (out, err, result) = (C.unpack out, C.unpack err, result)

-- |Partition the exit code from the outputs.
collectResult :: Outputs -> (Outputs, ExitCode)
collectResult output =
    unResult (partition isResult output)
    where
      isResult (Result _) = True
      isResult _ = False
      unResult (out, [Result x]) = (out, x)
      unResult _ = error "Internal error - wrong number of results"
