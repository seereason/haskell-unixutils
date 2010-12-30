{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
-- |Control the progress reporting and output of subprocesses.
module System.Unix.Progress
    ( -- * The Progress Monad
      Progress
    , ProgressFlag(..)
    , runProgress
    -- * Process launching
    , lazyCommandP
    , lazyProcessP
    , lazyCommandV -- Print everything
    , lazyProcessV
    , lazyCommandF -- Like V, but throws exception on failure
    , lazyProcessF
    , lazyCommandE -- Print everything on failure
    , lazyProcessE
    , lazyCommandEF -- E and F combo
    , lazyProcessEF
    , lazyCommandD -- Dots
    , lazyCommandQ -- Quiet
    , lazyCommandS -- Silent
    , lazyCommandSF
    -- * Quietness control
    , defaultQuietness
    , modQuietness
    , quieter
    -- * Output stream processing
    -- , prefixes
    -- , printOutput
    -- , dotOutput
    , timeTask
    , showElapsed
    , ePutStr
    , ePutStrLn
    , qPutStr
    , qPutStrLn
    , eMessage
    , eMessageLn
    , qMessage
    , qMessageLn
    -- Unit tests
    , tests
    ) where

import Control.Exception (evaluate, try, SomeException)
import Control.Monad (when)
import Control.Monad.State (StateT, get, evalStateT)
import "mtl" Control.Monad.Trans ( MonadIO, liftIO, lift )
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr, hPutStr)
import System.Posix.Env (setEnv)
import System.Unix.Process (lazyProcess, lazyCommand, Output(Stdout, Stderr),
                            exitCodeOnly, stdoutOnly, mergeToStdout)
import Test.HUnit

-- |A monad for controlling progress reporting of subprocesses.
type Progress a = StateT (Set.Set ProgressFlag) IO a

-- |The flags that control what type of output will be sent to stdout
-- and stderr.  Also, the ExceptionOnFail flag controls whether an
-- exception will be thrown if the @ExitCode@ is not @ExitSuccess@.
data ProgressFlag
    = Echo
    | Dots
    | All
    | Errors
    | Result
    | EchoOnFail
    | AllOnFail
    | ErrorsOnFail
    | ResultOnFail
    | ExceptionOnFail
    deriving (Ord, Eq)

-- |Run the Progress monad with the given flags.  The flag set is
-- compute from the current quietness level, <= 0 the most verbose
-- and >= 3 the least.
runProgress :: [ProgressFlag]  -- ^ Additional flags, such as FailOnFail
            -> Progress a      -- ^ The progress task to be run
            -> IO a
runProgress flags action =
    quietness >>= \ q ->
    evalStateT action (Set.fromList (flags ++ quietFlags q))
    where
      quietFlags n
          | n <= 0 = [Echo, All, Result]
          -- [Echo, Errors, Result]
          | n == 1 = [Echo, Dots, Result]
          -- [Echo, Result]
          | n == 2 = [Echo]
          | True   = []

lazyCommandP :: [ProgressFlag] -> String -> L.ByteString -> IO [Output]
lazyCommandP flags cmd input = runProgress flags (lift (lazyCommand cmd input) >>= doProgress cmd)

lazyProcessP :: [ProgressFlag] -> FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> IO [Output]
lazyProcessP flags exec args cwd env input =
    runProgress flags (lift (lazyProcess exec args cwd env input) >>= doProgress (intercalate " " (exec : args)))

lazyCommandV :: String -> L.ByteString -> IO [Output]
lazyCommandV = lazyCommandP []

lazyProcessV :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> IO [Output]
lazyProcessV = lazyProcessP []

lazyCommandF :: String -> L.ByteString -> IO [Output]
lazyCommandF = lazyCommandP [ExceptionOnFail]

lazyProcessF :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> IO [Output]
lazyProcessF = lazyProcessP [ExceptionOnFail]

lazyCommandE :: String -> L.ByteString -> IO [Output]
lazyCommandE = lazyCommandP [EchoOnFail, AllOnFail, ResultOnFail]

lazyProcessE :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> IO [Output]
lazyProcessE = lazyProcessP [EchoOnFail, AllOnFail, ResultOnFail]

lazyCommandEF :: String -> L.ByteString -> IO [Output]
lazyCommandEF = lazyCommandP [EchoOnFail, AllOnFail, ResultOnFail, ExceptionOnFail]

lazyProcessEF :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> IO [Output]
lazyProcessEF = lazyProcessP [EchoOnFail, AllOnFail, ResultOnFail, ExceptionOnFail]

lazyCommandD :: String -> L.ByteString -> IO [Output]
lazyCommandD cmd input = quieter 1 $ lazyCommandP [EchoOnFail, AllOnFail, ResultOnFail] cmd input

lazyCommandQ :: String -> L.ByteString -> IO [Output]
lazyCommandQ cmd input = quieter 3 $ lazyCommandP [EchoOnFail, AllOnFail, ResultOnFail] cmd input

lazyCommandS :: String -> L.ByteString -> IO [Output]
lazyCommandS cmd input = quieter 4 $ lazyCommandP [EchoOnFail, AllOnFail, ResultOnFail] cmd input

lazyCommandSF :: String -> L.ByteString -> IO [Output]
lazyCommandSF cmd input = quieter 4 $ lazyCommandP [EchoOnFail, AllOnFail, ResultOnFail, ExceptionOnFail] cmd input

-- |Look for occurrences of -v and -q in the command line arguments
-- and the current values of environment variables VERBOSITY and
-- QUIETNESS to compute a new value for QUIETNESS.  If you want to
-- ignore the current QUIETNESS value say @setQuietness 0 >>
-- computeQuietness@.
defaultQuietness :: IO Int
defaultQuietness =
    do v1 <- try (getEnv "VERBOSITY" >>= return . read) >>= either (\ (_ :: SomeException) -> return 0) return
       v2 <- getArgs >>= return . length . filter (== "-v")
       q1 <- try (getEnv "QUIETNESS" >>= return . read) >>= either (\ (_ :: SomeException) -> return 0) return
       q2 <- getArgs >>= return . length . filter (== "-q")
       return $ q1 - v1 + q2 - v2

-- |Look at the number of -v and -q arguments to get the baseline
-- quietness / verbosity level for progress reporting.
quietness :: IO Int
quietness = try (getEnv "QUIETNESS" >>= return . read) >>= either (\ (_ :: SomeException) -> return 0) return

-- |Perform a task with the given quietness level.
modQuietness :: (Int -> Int) -> IO a -> IO a
modQuietness f task =
    quietness >>= \ q0 ->
    setQuietness (f q0) >>
    task >>= \ result ->
    setQuietness q0 >>
    return result
    where
      -- Set the value of QUIETNESS in the environment.
      setQuietness :: Int -> IO ()
      setQuietness q = setEnv "QUIETNESS" (show q) True

-- |Do an IO task with additional -v or -q arguments so that the
-- progress reporting becomes more or less verbose.
quieter :: Int -> IO a -> IO a
quieter q task = modQuietness (+ q) task

-- |Inject a command's output into the Progress monad, handling command echoing,
-- output formatting, result code reporting, and exception on failure.
doProgress :: String -> [Output] -> Progress [Output]
doProgress cmd output =
    get >>= \ s ->
    doEcho s output >>= doOutput s >>= doResult s >>= doFail s
    where
      doEcho s output
          | Set.member Echo s || (Set.member EchoOnFail s && exitCodeOnly output /= ExitSuccess) =
              lift (hPutStrLn stderr ("-> " ++ cmd)) >> return output
          | True = return output
      doOutput s output
          | Set.member All s || (Set.member AllOnFail s && exitCodeOnly output /= ExitSuccess) =
              lift (printOutput (prefixes opre epre output))
          | Set.member Dots s =
              lift (dotOutput 128 output)
          | Set.member Errors s || (Set.member ErrorsOnFail s && exitCodeOnly output /= ExitSuccess) =
              lift (printErrors (prefixes opre epre output))
          | True = return output
      doResult s output
          | Set.member Result s || (Set.member ResultOnFail s && exitCodeOnly output /= ExitSuccess) =
              lift (hPutStrLn stderr ("<- " ++ show (exitCodeOnly output))) >> return output
          | True = return output
      doFail :: Set.Set ProgressFlag -> [Output] -> Progress [Output]
      doFail s output
          | Set.member ExceptionOnFail s =
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

-- |Print all the output to the appropriate output channel.  Each pair
-- is the original input (to be returned) and the modified input (to
-- be printed.)
printOutput :: [(Output, Output)] -> IO [Output]
printOutput output =
    mapM print' output
    where
      print' (x, y) = print y >> return x
      print (Stdout s) = putStr (B.unpack s)
      print (Stderr s) = hPutStr stderr (B.unpack s)
      print _ = return ()

-- |Print all the error output to the appropriate output channel
printErrors :: [(Output, Output)] -> IO [Output]
printErrors output =
    mapM print' output
    where
      print' (x, y) = print y >> return x
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

-- |Send a string to stderr.
ePutStr :: String -> IO ()
ePutStr = hPutStr stderr

-- |@ePutStr@ with a terminating newline.
ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

-- |If the current quietness level is less than one print a message.
-- Control the quietness level using @quieter@.
qPutStr :: String -> IO ()
qPutStr s = quietness >>= \ q -> when (q <= 0) (ePutStr s)

-- |@qPutStr@ with a terminating newline.
qPutStrLn :: String -> IO ()
qPutStrLn s = quietness >>= \ q -> when (q <= 0) (ePutStrLn s)

-- |Print a message and return the second argument unevaluated.
eMessage :: String -> a -> IO a
eMessage message output = ePutStr message >> return output

-- |@eMessage@ with a terminating newline.
eMessageLn :: String -> a -> IO a
eMessageLn message output = ePutStrLn message >> return output

-- |@eMessage@ controlled by the quietness level.
qMessage :: String -> a -> IO a
qMessage message output = quietness >>= \ q -> when (q <= 0) (ePutStr message) >> return output

-- |@qMessage@ with a terminating newline.
qMessageLn :: String -> a -> IO a
qMessageLn message output = quietness >>= \ q -> when (q <= 0) (ePutStrLn message) >> return output

tests :: [Test]
tests =
    [TestCase (assertEqual "Check behavior of code to insert prefixes into Output"
               (collect (prefixes (p "[1] ") (p "[2] ")
                         [Stdout (p "abc\ndef\n\n"), Stderr (p "\nghi\njkl\n")]))
               "[1] abc\n[1] def\n[1] \n[2] \n[2] ghi\n[2] jkl\n")]
    where
      p = B.pack
      collect :: [(Output, Output)] -> String
      collect = L.unpack . stdoutOnly . mergeToStdout . snd . unzip
