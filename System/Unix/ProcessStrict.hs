{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures -Werror #-}
-- | Strict process running
module System.Unix.ProcessStrict
    (
      simpleProcess	-- FilePath -> [String] -> IO (String, String, ExitCode)
    , processResult	-- FilePath -> [String] -> IO (Either Int (String, String))
    , processOutput	-- FilePath -> [String] -> IO (Either Int String)
    , simpleCommand	-- String -> IO (String, String, ExitCode)
    , commandResult	-- String -> IO (Either Int (String, String))
    , commandOutput	-- String -> IO (Either Int String)
    ) where
    
import Control.Exception hiding (catch)
import Control.Parallel.Strategies (rnf)
import System.Process (waitForProcess, runInteractiveProcess, runInteractiveCommand)
import System.IO (hSetBinaryMode, hClose, hGetContents)
import System.Unix.Process

{-# DEPRECATED simpleProcess "use lazyProcess exec args Nothing Nothing L.empty >>= return . collectOutputUnpacked" #-}
{-# DEPRECATED processOutput "use lazyProcess exec args Nothing Nothing L.empty" #-}
{-# DEPRECATED simpleCommand "use lazyCommand cmd L.empty >>= return . collectOutputUnpacked" #-}
{-# DEPRECATED commandResult "use lazyCommand cmd L.empty" #-}
{-# DEPRECATED commandOutput "use lazyCommand cmd L.empty" #-}

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
       hSetBinaryMode out True
       hSetBinaryMode err True
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
