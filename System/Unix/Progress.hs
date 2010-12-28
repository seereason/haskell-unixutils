{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
-- |Control the progress reporting and output of subprocesses.
module System.Unix.Progress
    ( Progress
    , ProgressFlag(..)
    , runProgress
    , lazyCommandP
    , lazyProcessP
    , prefixes
    , printOutput
    , dotOutput
    , timeTask
    , showElapsed
    , ePutStr
    , ePutStrLn
    , eMessage
    , eMessageLn
    , lazyCommandV -- Verbose
    , lazyCommandF -- Like V, but fails
    , lazyProcessF
    , lazyCommandQ -- Quiet
    , lazyCommandD -- Dots
    , quieter
    ) where

import Control.Exception (evaluate {- , try, SomeException -})
import Control.Monad (when)
import Control.Monad.State (StateT, get, evalStateT)
import "mtl" Control.Monad.Trans ( MonadIO, liftIO, lift )
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (intercalate)
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr, hPutStr)
import System.Unix.Process (lazyProcess, lazyCommand, Output(Stdout, Stderr), exitCodeOnly)
import qualified Data.Set as Set

type Progress a = StateT (Set.Set ProgressFlag) IO a
data ProgressFlag = Echo | EchoOnFail | Dots | All | AllOnFail | Result | ResultOnFail | FailOnFailure deriving (Ord, Eq)
runProgress :: [ProgressFlag]  -- ^ Additional flags, such as FailOnFailure
            -> Progress a      -- ^ The progress task to be run
            -> IO a
runProgress flags action =
    quietness >>= \ q ->
    evalStateT action (Set.fromList (flags ++ quietFlags q))
    where
      quietFlags n
          | n <= 0 = [Echo, All, Result]
          | n == 1 = [Echo, Dots, AllOnFail, Result]
          | n == 2 = [Echo, AllOnFail, ResultOnFail]
          | n == 3 = [EchoOnFail, AllOnFail, ResultOnFail]
          | True   = []

-- |Look at the number of -v and -q arguments to get the baseline
-- quietness / verbosity level for progress reporting.
quietness :: IO Int
quietness =
    do -- v1 <- try (getEnv "VERBOSITY" >>= return . read) >>= either (\ (_ :: SomeException) -> return 0) return
       v2 <- getArgs >>= return . length . filter (== "-v")
       -- q1 <- try (getEnv "QUIETNESS" >>= return . read) >>= either (\ (_ :: SomeException) -> return 0) return
       q2 <- getArgs >>= return . length . filter (== "-q")
       return ({- v1 - q1 + -} q2 - v2)

-- |Do an IO task with additional -v or -q arguments so that the
-- progress reporting becomes more or less verbose.
quieter :: Int -> IO a -> IO a
quieter n task =
    do args <- getArgs
       withArgs (extra ++ args) task
    where
      extra = replicate n (if n < 0 then "-v" else "-q")

-- |Inject a command's output into the Progress monad, handling command echoing,
-- output formatting, result code reporting, and exception on failure.
doProgress :: String -> [Output] -> Progress [Output]
doProgress cmd output =
    get >>= \ s ->
    doEcho s output >>= doOutput s >>= doResult s >>= doFail s
    where
      doEcho s output
          | Set.member Echo s =
              lift (hPutStrLn stderr ("-> " ++ cmd)) >> return output
          | Set.member EchoOnFail s =
              case exitCodeOnly output of
                ExitSuccess -> return output
                _ -> lift (hPutStrLn stderr ("-> " ++ cmd)) >> return output
          | True = return output
      doOutput s output
          | Set.member All s =
              lift (printOutput (prefixes opre epre output))
          | Set.member AllOnFail s =
              -- This will force all the output
              case exitCodeOnly output of
                ExitSuccess -> return output
                _ ->
                    -- Make sure the command gets echoed if we fail
                    when (not (Set.member Echo s)) (lift (hPutStrLn stderr ("> " ++ cmd))) >>
                    lift (printOutput (prefixes opre epre output))
          | Set.member Dots s = lift (dotOutput 128 output)
          | True = return output
      doResult s output =
          when (Set.member Result s) 
               (lift (hPutStrLn stderr ("<- " ++ show (exitCodeOnly output)))) >>
          return output
      doFail :: Set.Set ProgressFlag -> [Output] -> Progress [Output]
      doFail s output
          | Set.member FailOnFailure s =
              case exitCodeOnly output of
                ExitSuccess -> return output
                result -> fail ("*** FAILURE: " ++ cmd ++ " -> " ++ show result)
          | True = return output
      opre = B.pack " 1> "
      epre = B.pack " 2> "

-- |Print one dot to stderr for every COUNT characters of output.
dotOutput :: Int -> [Output] -> IO [Output]
dotOutput groupSize output =
    mapM (\ (count, elem) -> ePutStr (replicate count '.') >> return elem) pairs >>= eMessageLn ""
    where
      pairs = zip (dots 0 (map length output)) output
      dots _ [] = []
      dots rem (count : more) =
          let (count', rem') = divMod (count + rem) groupSize in
          count' : dots rem' more
      length (Stdout s) = B.length s
      length (Stderr s) = B.length s
      length _ = 0

-- |Add prefixes to the output stream after every newline that is followed
-- by additional text, and at the beginning 
prefixes :: B.ByteString -> B.ByteString -> [Output] -> [(Output, Output)]
prefixes opre epre output =
    f True output
    where
      f :: Bool -> [Output] -> [(Output, Output)]
      f _ [] = []
      f bol (x@(Stdout s) : output') = let (s', bol') = doOutput bol opre s in (x, Stdout s') : f bol' output'
      f bol (x@(Stderr s) : output') = let (s', bol') = doOutput bol epre s in (x, Stderr s') : f bol' output'
      f bol (x : output') = (x, Stdout B.empty) : f bol output'
      doOutput :: Bool -> B.ByteString -> B.ByteString -> (B.ByteString, Bool)
      doOutput bol pre s =
          let (a, b) = B.span (/= '\n') s in
          if B.null a
          then if B.null b
               then (B.empty, bol)
               else let x = (if bol then pre else B.empty)
                        (s', bol') = doOutput True pre (B.tail b) in
                    (B.concat [x, (B.pack "\n"), s'], bol')
          -- There is some text before a possible newline
          else let x = (if bol then B.append pre a else a)
                   (s', bol') = doOutput False pre b in 
               (B.append x s', bol')

-- |Print all the output to the appropriate output channel
printOutput :: [(Output, Output)] -> IO [Output]
printOutput output =
    mapM print' output
    where
      print' (x, y) = print y >> return x
      print (Stdout s) = putStr (B.unpack s)
      print (Stderr s) = hPutStr stderr (B.unpack s)
      print _ = return ()

-- |Run a task and return the elapsed time along with its result.
timeTask :: MonadIO m => m a -> m (a, NominalDiffTime)
timeTask x =
    do start <- liftIO getCurrentTime
       result <- x >>= liftIO . evaluate
       finish <- liftIO getCurrentTime
       return (result, diffUTCTime finish start)

-- |Perform a task, print the elapsed time it took, and return the result.
showElapsed :: String -> IO a -> IO a
showElapsed label f =
    do (result, time) <- timeTask f
       ePutStr (label ++ formatTime' time)
       return result

formatTime' :: NominalDiffTime -> String
formatTime' diff = show diff
{-
    case () of
      _ | isPrefixOf "00:00:0" hms -> drop 7 hms ++ printf ".%03d" ms ++ " s."
      _ | isPrefixOf "00:00:" hms -> drop 6 hms ++ printf ".%03d" ms ++ " s."
      _ | isPrefixOf "00:" hms -> drop 3 hms
      _ -> hms
    where
      hms = formatTime defaultTimeLocale "%T" diff
      (s, ms) = second toMilliseconds (properFraction diff) :: (Integer, Integer)
      toMilliseconds :: (RealFrac a, Integral b) => a -> b
      toMilliseconds f = round (f * 1000)
-}

ePutStr :: String -> IO ()
ePutStr = hPutStr stderr

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

-- |Print a message without forcing the command's output
eMessage :: String -> a -> IO a
eMessage message output = ePutStr message >> return output

-- |Print a message without forcing the command's output
eMessageLn :: String -> a -> IO a
eMessageLn message output = ePutStrLn message >> return output

lazyCommandP :: [ProgressFlag] -> String -> L.ByteString -> IO [Output]
lazyCommandP flags cmd input = runProgress flags (lift (lazyCommand cmd input) >>= doProgress cmd)

lazyCommandV :: String -> L.ByteString -> IO [Output]
lazyCommandV = lazyCommandP []

lazyCommandF :: String -> L.ByteString -> IO [Output]
lazyCommandF = lazyCommandP [FailOnFailure]

lazyProcessP :: [ProgressFlag] -> FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> IO [Output]
lazyProcessP flags exec args cwd env input =
    runProgress flags (lift (lazyProcess exec args cwd env input) >>= doProgress (intercalate " " (exec : args)))

lazyProcessF :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> IO [Output]
lazyProcessF = lazyProcessP [FailOnFailure]

lazyCommandD :: String -> L.ByteString -> IO [Output]
lazyCommandD cmd input = quieter 1 $ lazyCommandP [EchoOnFail, AllOnFail, ResultOnFail] cmd input

lazyCommandQ :: String -> L.ByteString -> IO [Output]
lazyCommandQ cmd input = quieter 3 $ lazyCommandP [EchoOnFail, AllOnFail, ResultOnFail] cmd input
