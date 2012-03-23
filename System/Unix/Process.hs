-- | Variations of the readProcess and readProcessWithExitCode functions
-- from System.Process which read and write ByteStrings and have an
-- extra argument to modify the CreateProcess value before the process
-- is started.
module System.Unix.Process
    ( readProcess
    , readProcessWithExitCode
    ) where

import Control.Concurrent (newEmptyMVar, forkIO, putMVar, takeMVar)
import qualified Control.Exception as C
import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.IO.Exception (IOErrorType(OtherError))
import System.Exit (ExitCode(..))
import System.IO (hFlush, hClose)
import System.IO.Error (mkIOError)
import System.Process (CreateProcess(std_in, std_out, std_err, cwd), createProcess, waitForProcess, proc, StdStream(CreatePipe, Inherit), showCommandForUser)

readProcessWithExitCode
    :: FilePath                         -- ^ command to run
    -> [String]                         -- ^ any arguments
    -> (CreateProcess -> CreateProcess) -- ^ Modify process with this - use id for System.Process.readProcessWithExitCode behavior
    -> B.ByteString                     -- ^ standard input
    -> IO (ExitCode, B.ByteString, B.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args modify input = do
    let modify' p = modify (p {std_in  = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (modify' (proc cmd args))

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- B.hGetContents outh
    _ <- forkIO $ C.evaluate (B.length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- B.hGetContents errh
    _ <- forkIO $ C.evaluate (B.length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (B.null input)) $ do B.hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)

readProcess
    :: FilePath		-- ^ command to run
    -> [String]		-- ^ any arguments
    -> (CreateProcess -> CreateProcess) -- ^ modifies CreateProcess before passing to createProcess
    -> B.ByteString	-- ^ standard input
    -> IO B.ByteString	-- ^ stdout
readProcess cmd args modify input = do
    let modify' p = modify (p {std_in  = CreatePipe, std_out = CreatePipe, std_err = Inherit })
    (Just inh, Just outh, _, pid) <-
        createProcess (modify' (proc cmd args))

    -- fork off a thread to start consuming the output
    output  <- B.hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (B.length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (B.null input)) $ do B.hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
      ExitSuccess   -> return output
      ExitFailure r ->
          ioError (mkIOError OtherError ("readProcess: " ++ showCommandForUser cmd args ++
                                                             " (exit " ++ show r ++ ")")
                   Nothing Nothing)
