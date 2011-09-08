-- | Functions to manage the verbosity of a console program by storing
-- the quietness level in the system environment, specifically in the
-- $QUIETNESS variable.  This lets you avoid creating a StateT monad
-- to hold the quietness level.  Note that you don't attach a
-- verbosity level to individual message commands, you control the
-- quietness level for entire regions of your program and messages
-- only appear when quietness is less than one.
{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module System.Unix.QIO
    ( ePutStr
    , ePutStrLn
    , eMessage
    , eMessageLn
    -- * Get/set quietness levels
    , initialQuietness
    , quietness
    -- * Do task with modified quietness level
    -- , modQuietness
    , quieter
    , quieter'
    -- , qZero
    -- * Do a task if quietness < 1
    , qDo
    -- * Output to stderr when quietness < 1
    , qPutStr
    , qPutStrLn
    , qMessage
    , qMessageLn
    -- * Some idioms
    , q12
    , q02
    , v1
    , v2
    , v3
    , showQ
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
initialQuietness :: MonadIO m => m Int
initialQuietness = liftIO $
    do v1 <- try (getEnv "VERBOSITY" >>= return . read) >>= either (\ (_ :: SomeException) -> return 0) return
       v2 <- getArgs >>= return . length . filter (== "-v")
       q1 <- try (getEnv "QUIETNESS" >>= return . read) >>= either (\ (_ :: SomeException) -> return 0) return
       q2 <- getArgs >>= return . length . filter (== "-q")
       return $ q1 - v1 + q2 - v2

-- |Get the current quietness level from the QUIETNESS environment variable.
quietness :: MonadIO m => m Int
quietness = liftIO (try (getEnv "QUIETNESS" >>= return . read)) >>=
            either (\ (_ :: SomeException) -> return 0) return

-- |Perform a task with the quietness level tansformed by f.  Use
-- @defaultQuietness >>= modQuietness . const@ to initialize the --
-- verbosity for a program.
quieter :: MonadIO m => (Int -> Int) -> m a -> m a
quieter f task =
    quietness >>= \ q0 ->
    setQuietness (f q0) >>
    task >>= \ result ->
    setQuietness q0 >>
    return result
    where
      -- Set the value of QUIETNESS in the environment.
      setQuietness :: MonadIO m => Int -> m ()
      setQuietness q = liftIO $ setEnv "QUIETNESS" (show q) True

-- |Dummy version of quieter, sometimes you want to strip out all the
-- quieter calls and see how things look, then restore them gradually.
-- Use this to help remember where they were.
quieter' :: MonadIO m => (Int -> Int) -> m a -> m a
quieter' _ x = x

-- |Peform a task only if quietness < 1.
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

-- |Print a message at quietness +1 and then do a task at quietness +3.
-- This is a pattern which gives the following behaviors:
-- Normally there is no output.  With -v only the messages are printed.
-- With -v -v the messages and the shell commands are printed with dots
-- to show progress.  With -v -v -v everything is printed.
q12 :: MonadIO m => String -> m a -> m a
q12 s a = quieter (+ 1) $ qPutStrLn s >> quieter (+ 2) a

q02 :: MonadIO m => String -> m a -> m a
q02 s a = qPutStrLn s >> quieter (+ 2) a

v1 a = quieter (\x->x-1) a
v2 a = quieter (\x->x-2) a
v3 a = quieter (\x->x-3) a

-- |For debugging
showQ :: MonadIO m => String -> m a -> m a
showQ s a = quietness >>= \ n -> ePutStrLn (s ++ ": quietness=" ++ show n) >> a
