-- | Functions to manage the verbosity of a console program by storing
-- the quietness level in the system environment, specifically in the
-- $QUIETNESS variable.  This lets you avoid creating a StateT monad
-- to hold the quietness level.  Note that you don't attach a
-- verbosity level to individual message commands, you control the
-- quietness level for entire regions of your program and messages
-- only appear when quietness is less than one.
{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -Wall -Werror #-}
module System.Unix.QIO
    ( ePutStr
    , ePutStrLn
    , eMessage
    , eMessageLn
    -- * Get/set quietness levels
    , defaultQuietness
    , quietness
    -- * Do task with modified quietness level
    , modQuietness
    , quieter
    -- * Do a task if quietness < 1
    , qDo
    -- * Output to stderr when quietness < 1
    , qPutStr
    , qPutStrLn
    , qMessage
    , qMessageLn
    ) where

import Control.Exception (try, SomeException)
import "mtl" Control.Monad.Trans ( MonadIO, liftIO )
import System.Environment (getArgs, getEnv)
import System.IO (hPutStrLn, stderr, hPutStr)
import System.Posix.Env (setEnv)

ePutStr :: MonadIO m => String -> m ()
ePutStr s = liftIO $ hPutStr stderr s

ePutStrLn :: MonadIO m => String -> m ()
ePutStrLn s = liftIO $ hPutStrLn stderr s

eMessage :: MonadIO m => String -> b -> m b
eMessage s x = liftIO (hPutStr stderr s) >> return x

eMessageLn :: MonadIO m => String -> b -> m b
eMessageLn s x = liftIO (hPutStrLn stderr s) >> return x

-- | Compute an initial value for $QUIETNESS by examining the
-- $QUIETNESS and $VERBOSITY variables and counting the -v and -q
-- options on the command line.
defaultQuietness :: MonadIO m => m Int
defaultQuietness = liftIO $
    do v1 <- try (getEnv "VERBOSITY" >>= return . read) >>= either (\ (_ :: SomeException) -> return 0) return
       v2 <- getArgs >>= return . length . filter (== "-v")
       q1 <- try (getEnv "QUIETNESS" >>= return . read) >>= either (\ (_ :: SomeException) -> return 0) return
       q2 <- getArgs >>= return . length . filter (== "-q")
       return $ q1 - v1 + q2 - v2

-- |Get the current quietness level from the QUIETNESS environment variable.
quietness :: MonadIO m => m Int
quietness = liftIO (try (getEnv "QUIETNESS" >>= return . read)) >>=
            either (\ (_ :: SomeException) -> return 0) return

-- |Perform a task with the quietness level returned by f.  Use
-- @defaultQuietness >>= modQuietness . const@ to initialize the --
-- verbosity for a program.
modQuietness :: MonadIO m => (Int -> Int) -> m a -> m a
modQuietness f task =
    quietness >>= \ q0 ->
    setQuietness (f q0) >>
    task >>= \ result ->
    setQuietness q0 >>
    return result
    where
      -- Set the value of QUIETNESS in the environment.
      setQuietness :: MonadIO m => Int -> m ()
      setQuietness q = liftIO $ setEnv "QUIETNESS" (show q) True

-- |Increase quietness by N and do an IO task.
quieter :: MonadIO m => Int -> m a -> m a
quieter q task = modQuietness (+ q) task

qDo :: MonadIO m => m () -> m ()
qDo task = quietness >>= \ q -> if (q < 1) then task else return ()

-- |If the current quietness level is less than one print a message.
-- Control the quietness level using @quieter@.
qPutStr :: MonadIO m => String -> m ()
qPutStr s = qDo (ePutStr s)

-- |@qPutStr@ with a terminating newline.
qPutStrLn :: MonadIO m => String -> m ()
qPutStrLn s = qDo (ePutStrLn s)

-- |@eMessage@ controlled by the quietness level.
qMessage :: MonadIO m => String -> a -> m a
qMessage message output = qDo (ePutStr message) >> return output

-- |@qMessage@ with a terminating newline.
qMessageLn :: MonadIO m => String -> a -> m a
qMessageLn message output = qDo (ePutStrLn message) >> return output
