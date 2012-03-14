-- | Support for changing the output of a lazyCommand in several ways:
-- 
--     * Output a dot for every 128 characters of the original output
-- 
--     * Increase the quietness level before running the command
-- 
--     * Output only if (and when) the command fails
-- 
--     * Throw an exception if the command fails
-- 
--     * No output
{-# LANGUAGE FlexibleContexts, PackageImports, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}
module System.Unix.Progress
    ( -- * The Progress Monad
      ProgressFlag(..)
    -- * Process launching
    , lazyCommandP
    , lazyProcessP
    , timeTask
    , showElapsed
    -- * Unit tests
    , tests
    -- * A set of lazyCommand functions for an example set of verbosity levels
    , lazyCommandV -- Print everything - command, stdout, stderr, result
    , lazyProcessV
    , lazyCommandF -- Like V, but throws exception on failure
    , lazyProcessF
    , lazyCommandE -- Print everything on failure
    , lazyProcessE
    , lazyCommandEF -- E and F combo
    , lazyProcessEF
    ) where

import Control.Exception (evaluate)
import "mtl" Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import System.Exit (ExitCode(..))
import System.Unix.Process (lazyProcess, lazyCommand, Output(Stdout, Stderr),
                            exitCodeOnly, stdoutOnly, mergeToStdout)
import System.Unix.QIO (quietness, ePutStr, ePutStrLn, qPutStr)
import Test.HUnit

type ProgressState = Set.Set ProgressFlag

-- |The flags that control what type of output will be sent to stdout
-- and stderr.  Also, the ExceptionOnFail flag controls whether an
-- exception will be thrown if the @ExitCode@ is not @ExitSuccess@.
data ProgressFlag
    = Echo
    | Dots
    | All
    | Errors
    | Result
    | EchoOnFail      -- ^ If Echo is present this has no effect
    | AllOnFail       -- ^ If All is present this has no effect
    | ErrorsOnFail
    | ResultOnFail
    | ExceptionOnFail
    deriving (Ord, Eq)

lazyCommandV :: MonadIO m => String -> L.ByteString -> m [Output]
lazyCommandV cmd input =
    progressFlags Set.empty >>= lazyCommandP cmd input

lazyProcessV :: MonadIO m => FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> m [Output]
lazyProcessV cmd args wd env input =
    progressFlags Set.empty >>= lazyProcessP cmd args wd env input

lazyCommandF :: MonadIO m => String -> L.ByteString -> m [Output]
lazyCommandF cmd input =
    progressFlags (Set.fromList [ExceptionOnFail]) >>= lazyCommandP cmd input

lazyProcessF :: MonadIO m => FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> m [Output]
lazyProcessF cmd args wd env input =
    progressFlags (Set.fromList [ExceptionOnFail]) >>= lazyProcessP cmd args wd env input

lazyCommandE :: MonadIO m => String -> L.ByteString -> m [Output]
lazyCommandE cmd input =
    progressFlags (Set.fromList [EchoOnFail, AllOnFail, ResultOnFail]) >>= lazyCommandP cmd input

lazyProcessE :: MonadIO m => FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> m [Output]
lazyProcessE cmd args wd env input =
    progressFlags (Set.fromList [EchoOnFail, AllOnFail, ResultOnFail]) >>= lazyProcessP cmd args wd env input

lazyCommandEF :: MonadIO m => String -> L.ByteString -> m [Output]
lazyCommandEF cmd input =
    progressFlags (Set.fromList [EchoOnFail, AllOnFail, ResultOnFail, ExceptionOnFail]) >>= lazyCommandP cmd input

lazyProcessEF :: MonadIO m => FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> m [Output]
lazyProcessEF cmd args wd env input =
    progressFlags (Set.fromList [EchoOnFail, AllOnFail, ResultOnFail, ExceptionOnFail]) >>= lazyProcessP cmd args wd env input

-- A usable example of the construction of a verbosity level
-- specification.  You can supply your own defaultLevels list and
-- build the flags* and lazyCommand* functions in a similar way.

-- |Normally (when quietness is 0) we see the command echoed and dots
-- for progress indication.  If quietness is -1 we also see the
-- command's output as it runs.  If quietness is 1 we don't see the
-- dots or the result code, and if it is 2 we don't see anything.
progressFlags :: MonadIO m => Set.Set ProgressFlag -> m (Set.Set ProgressFlag)
progressFlags extra =
    quietness >>= return . merge extra . Set.fromList . flags
    where
      flags n | n < 0 = [Echo, All, Result]
      flags 0 = [Echo, Dots, Result]
      flags 1 = [Echo]
      flags _ = []
      merge extra flags =
          (if Set.member All flags then Set.delete AllOnFail else id) .
          (if Set.member Echo flags then Set.delete EchoOnFail else id) .
          (if Set.member Result flags then Set.delete ResultOnFail else id) $ Set.union extra flags

{-
defaultLevels :: Int -> Set.Set ProgressFlag
defaultLevels n =
    if n < 0 then default
    where
      levels = map Set.fromList
               [ [Echo, All, Result]  -- when quietness == 0
               , [Echo, Dots, Result] -- when quietness == 1
               , [Echo]               -- when quietness == 2
               , [] ]                 -- when quietness == 2

-- |Create a function that returns the flags used for a given
-- quietness level.
quietnessLevels :: [Set.Set ProgressFlag] -> Int -> Set.Set ProgressFlag
quietnessLevels flagLists i =
    a ! (min r . max l $ i)
    where a = array (0, length flagLists - 1) (zip [0..] flagLists)
          (l, r) = bounds a
-}

{-
-- |A monad for controlling progress reporting of subprocesses.
type Progress m a = MonadIO m => StateT ProgressState m a

-- |Run the Progress monad with the given flags.  The flag set is
-- compute from the current quietness level, <= 0 the most verbose
-- and >= 3 the least.
runProgress :: MonadIO m =>
            -> Progress m a      -- ^ The progress task to be run
            -> m a
runProgress flags action =
    quietness >>= evalStateT action . flags
-}

-- |The @P@ versions are the most general cases, here you can specify
-- a function that turns the quietness level into any set of progress
-- flags you like.
lazyCommandP :: MonadIO m => String -> L.ByteString -> Set.Set ProgressFlag -> m [Output]
lazyCommandP cmd input flags =
    liftIO (lazyCommand cmd input) >>= doProgress flags cmd

lazyProcessP :: MonadIO m => FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> L.ByteString -> Set.Set ProgressFlag -> m [Output]
lazyProcessP exec args cwd env input flags =
    liftIO (lazyProcess exec args cwd env input) >>= doProgress flags (intercalate " " (exec : args))

-- |Inject a command's output into the Progress monad, handling
-- command echoing, output formatting, result code reporting, and
-- exception on failure.  The flag set we get from the monad already
-- reflects the program's current quietness level, so calling quieter
-- and using the qPutStr functions here is not necessary.
doProgress :: MonadIO m => ProgressState -> String -> [Output] -> m [Output]
doProgress flags cmd output =
    doEcho flags output >>= doOutput flags >>= doFailOutput flags >>= doResult flags >>= doFail flags
    where
      doEcho flags output
          | Set.member Echo flags =
              ePutStrLn ("-> " ++ cmd) >> return output
          | Set.member EchoOnFail flags && exitCodeOnly output /= ExitSuccess =
              ePutStrLn ("-> " ++ cmd) >> return output
          | True = return output
      doOutput flags output
          | Set.member All flags =
              printOutput (prefixes opre epre output)
          | Set.member Dots flags =
              dotOutput 128 output
          | Set.member Errors flags =
              printErrors (prefixes opre epre output)
          | True = return output
      doFailOutput flags output
          | Set.member All flags =
              return output
          | Set.member AllOnFail flags && exitCodeOnly output /= ExitSuccess =
              ePutStrLn ("*** FAILURE: " ++ cmd ++ " -> " ++ show (exitCodeOnly output)) >>
              printOutput (prefixes opre epre output)
          | Set.member Errors flags =
              return output
          | Set.member ErrorsOnFail flags && exitCodeOnly output /= ExitSuccess =
              ePutStrLn ("*** FAILURE: " ++ cmd ++ " -> " ++ show (exitCodeOnly output)) >>
              printErrors (prefixes opre epre output)
          | True =
              return output
      doResult flags output
          | Set.member Result flags =
              ePutStrLn ("<- " ++ show (exitCodeOnly output)) >> return output
          | Set.member ResultOnFail flags && exitCodeOnly output /= ExitSuccess =
              ePutStrLn ("<- " ++ show (exitCodeOnly output)) >> return output
          | True = return output
      doFail flags output
          | Set.member ExceptionOnFail flags =
              case exitCodeOnly output of
                ExitSuccess -> return output
                result -> fail ("*** FAILURE: " ++ cmd ++ " -> " ++ show result)
          | True = return output
      opre = B.pack " 1> "
      epre = B.pack " 2> "

-- |Print one dot to stderr for every COUNT characters of output.
dotOutput :: MonadIO m => Int -> [Output] -> m [Output]
dotOutput groupSize output =
    ePutStr "." >> mapM (\ (count, elem) -> ePutStr (replicate count '.') >> return elem) pairs >>= \ x -> ePutStr ".\n" >> return x
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
printOutput :: MonadIO m => [(Output, Output)] -> m [Output]
printOutput output =
    mapM (liftIO . print') output
    where
      print' (x, y) = print y >> return x
      print (Stdout s) = liftIO $ putStr (B.unpack s)
      print (Stderr s) = ePutStr (B.unpack s)
      print _ = return ()

-- |Print all the error output to the appropriate output channel
printErrors :: MonadIO m => [(Output, Output)] -> m [Output]
printErrors output =
    mapM print' output
    where
      print' (x, y) = print y >> return x
      print (Stderr s) = ePutStr (B.unpack s)
      print _ = return ()

-- |Run a task and return the elapsed time along with its result.
timeTask :: MonadIO m => m a -> m (a, NominalDiffTime)
timeTask x =
    do start <- liftIO getCurrentTime
       result <- x >>= liftIO . evaluate
       finish <- liftIO getCurrentTime
       return (result, diffUTCTime finish start)

-- |Perform a task, print the elapsed time it took, and return the result.
showElapsed :: MonadIO m => String -> m a -> m a
showElapsed label f =
    do (result, time) <- timeTask f
       qPutStr (label ++ formatTime' time)
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

-- |Unit tests.
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
