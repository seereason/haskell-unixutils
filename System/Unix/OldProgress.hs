{-# LANGUAGE ScopedTypeVariables #-}
-- |Run shell commands with various types of progress reporting.
--
-- Author: David Fox
module System.Unix.OldProgress {-# DEPRECATED "Use System.Unix.Progress" #-}
    (
     systemTask, 	-- [Style] -> String -> IO TimeDiff
     otherTask,		-- [Style] -> IO a -> IO a
     Style (Start, Finish, Error, Output, Echo, Elapsed, Verbosity, Indent),
     readStyle,		-- String -> Maybe Style
     Output (Indented, Dots, Done, Quiet),
     msg,		-- [Style] -> String -> IO ()
     msgLn,		-- [Style] -> String -> IO ()
     -- * Accessors
     output,		-- [Style] -> Maybe Output
     verbosity,		-- [Style] -> Int
     -- * Style Set modification
     setStyles,		-- [Style] -> [Style] -> [Style]
     setStyle,		-- Style -> [Style] -> [Style]
     addStyles,		-- [Style] -> [Style] -> [Style]
     addStyle,		-- Style -> [Style] -> [Style]
     removeStyle,	-- Style -> [Style] -> [Style]
     -- * Utilities
     stripDist,		-- FilePath -> FilePath
     showElapsed,	-- String -> IO a -> IO a
     System.Time.TimeDiff,
     System.Time.noTimeDiff,
     fixedTimeDiffToString
    ) where

import Control.Exception
import Data.List
import System.Exit
import System.IO
import System.Process
import System.Time

data Output
    = Indented |
      -- ^ Print all the command's output with each line
      -- indented using (by default) the string ' > '.
      Dots |
      -- ^ Print a dot for every 1024 characters the command
      -- outputs
      Done |
      -- ^ Print an ellipsis (...) when the command starts
      -- and then "done." when it finishes.
      Quiet
      -- ^ Print nothing.

instance Show Output where
    show Indented = "Indented"
    show Dots = "Dots"
    show Done = "Done"
    show Quiet  = "Quiet "

data Style
    = Start String |
      -- ^ Message printed before the execution begins
      Finish String |
      -- ^ Message printed on successful termination
      Error String |
      -- ^ Message printed on failure
      Output Output |
      -- ^ Type of output to generate during execution
      Echo Bool |
      -- ^ If true, echo the shell command before beginning
      Elapsed Bool |
      -- ^ If true print the elapsed time on termination
      Verbosity Int |
      -- ^ Set the verbosity level.  This value can be queried
      -- using the verbosity function, but is not otherwise used
      -- by the -- functions in this module.
      Indent String
      -- ^ Set the indentation string for the generated output.

instance Show Style where
    show (Start s) = "Start " ++ show s
    show (Finish s) = "Finish " ++ show s
    show (Error s) = "Error " ++ show s
    show (Output output) = "Output " ++ show output
    show (Echo flag) = "Echo " ++ show flag
    show (Elapsed flag) = "Elapsed " ++ show flag
    show (Verbosity n) = "Verbosity " ++ show n
    show (Indent s) = "Verbosity " ++ show s

styleClass (Start _) = "Start"
styleClass (Finish _) = "Finish"
styleClass (Error _) = "Error"
styleClass (Output _) = "Progress"
styleClass (Echo _) = "Echo"
styleClass (Elapsed _) = "Elapsed"
styleClass (Verbosity _) = "Verbosity"
styleClass (Indent _) = "Indent"

-- This definition of equivalence is used to add or replace a style
-- parameter - for example, supply a Start message if none is present.
instance Eq Style where
    a == b = styleClass a == styleClass b

-- |Create a task that sends its output to a handle and then can be
-- terminated using an IO operation that returns an exit status.
-- Throws an error if the command fails.
systemTask :: [Style] -> String -> IO TimeDiff
systemTask style command =
    do
      start <- getClockTime
      putIndent style
      startMessage style
      taskStart style
      (_, _, outputHandle, processHandle) <- runInteractiveCommand ("{ " ++ command ++ "; } 1>&2")
      text <- progressOutput (maybe Indented id (output style)) outputHandle;
      result <- waitForProcess processHandle
      finish <- getClockTime
      let elapsed = diffClockTimes finish start
      case result of
        ExitSuccess -> finishMessage style elapsed
        ExitFailure _ -> errorMessage style text
      return elapsed
    where
      taskStart (Echo True : etc) = do hPutStrLn stderr ("\n -> " ++ command); taskStart etc
      taskStart (_ : etc) = taskStart etc
      taskStart [] = return ()

otherTask :: [Style] -> IO a -> IO a
otherTask style task =
    do
      start <- getClockTime
      putIndent style
      startMessage style
      taskStart style
      result <- try task
      hPutStr stderr "..."
      finish <- getClockTime
      let elapsed = diffClockTimes finish start
      case result of
        Left (e :: SomeException) ->
            do errorMessage style (show e)
               error (show e)
        Right a ->
            do finishMessage style elapsed
               return a
    where
      taskStart (_ : etc) = taskStart etc
      taskStart [] = return ()

-- FIXME: these two should break up the text into lines and prepend
-- the indentation to each.
msg :: [Style] -> String -> IO ()
msg style text =
    do
      putIndent style
      hPutStr stderr text        

msgLn :: [Style] -> String -> IO ()
msgLn style text =
    do
      putIndent style
      hPutStrLn stderr text        

putIndent :: [Style] -> IO ()
putIndent style = hPutStr stderr (indent style)

startMessage :: [Style] -> IO ()
startMessage (Start message : etc) = do hPutStr stderr message; startMessage etc
startMessage (_ : etc) = startMessage etc
startMessage [] = return ()

progressOutput :: Output -> Handle -> IO String

progressOutput Dots handle =
    do
      hPutStr stderr "..."
      doText 0 ""
    where
      doText count text =
          do
            eof <- hIsEOF handle
            case eof of
              False ->
                  do
                    line <- hGetLine handle
                    let count' = count + length line + 1
                    let text' = text ++ line ++ "\n"
                    let (n, m) = quotRem count' 1024
                    hPutStr stderr (replicate n '.')
                    doText m text'
              True ->
                  do
                    -- hPutStr stderr "done."
                    return text

progressOutput Done handle =
    do
      hPutStr stderr "..."
      doText ""
    where
      doText text =
          do
            eof <- hIsEOF handle
            case eof of
              False ->
                  do
                    line <- hGetLine handle
                    let text' = text ++ line ++ "\n"
                    doText text'
              True ->
                  do
                    -- hPutStr stderr "done."
                    return text

progressOutput Indented handle =
    do
      hPutStrLn stderr ""
      doText
    where
      doText =
          do
            eof <- hIsEOF handle
            case eof of
              True -> return ""
              False ->
                  do
                    line <- hGetLine handle
                    -- Not collecting text here since it gets output.
                    -- This is a judgement call.
                    -- let text' = text ++ line ++ "\n"
                    hPutStrLn stdout (prefix ++ line)
                    hFlush stdout
                    doText
      prefix = " >    "

progressOutput Quiet handle =
    do
      doText ""
    where
      doText text =
          do
            eof <- hIsEOF handle
            case eof of
              False ->
                  do
                    line <- hGetLine handle
                    let text' = text ++ line ++ "\n"
                    doText text'
              True -> return text

finishMessage :: [Style] -> TimeDiff -> IO ()
finishMessage (Elapsed True : etc) elapsed =
    do
      hPutStr stderr ("  (Elapsed: " ++ fixedTimeDiffToString elapsed ++ ")")
      finishMessage etc elapsed
finishMessage (Finish message : etc) elapsed = do hPutStr stderr message; finishMessage etc elapsed
finishMessage (_ : etc) elapsed = finishMessage etc elapsed
finishMessage [] _ = do hPutStrLn stderr ""; return ()

errorMessage :: [Style] -> String -> IO ()
errorMessage (Error message : _) text =
    do
      hPutStrLn stderr text
      error message
errorMessage (_ : etc) text = errorMessage etc text
errorMessage [] text = errorMessage [Error "failed"] text

-- |Remove styles by class
removeStyle :: Style -> [Style] -> [Style]
removeStyle (Start _) style = filter (not . isStart) style 
removeStyle (Finish _) style = filter (not . isFinish) style 
removeStyle (Error _) style = filter (not. isError) style 
removeStyle (Output _) style = filter (not . isOutput) style 
removeStyle (Echo _) style = filter (not . isEcho) style 
removeStyle old style = filter (/= old) style

-- |Add styles, replacing old ones if present
setStyles :: [Style] -> [Style] -> [Style]
setStyles [] style = style
setStyles (x:xs) style = setStyles xs (x : (removeStyle x style))

-- |Singleton case of setStyles
setStyle :: Style -> [Style] -> [Style]
setStyle new style = setStyles [new] style

-- |Singleton case of addStyles
addStyle :: Style -> [Style] -> [Style]
addStyle x@(Start _) style = case filter isStart style of [] -> x : style; _ -> style
addStyle x@(Finish _) style = case filter isFinish style of [] -> x : style; _ -> style
addStyle x@(Error _) style = case filter isError style of [] -> x : style; _ -> style
addStyle x@(Output _) style = case filter isOutput style of [] -> x : style; _ -> style
addStyle x@(Echo _) style = case filter isEcho style of [] -> x : style; _ -> style
addStyle x style = if elem x style then style else x : style

isStart (Start _) = True
isStart _ = False
isFinish (Finish _) = True
isFinish _ = False
isError (Error _) = True
isError _ = False
isOutput (Output _) = True
isOutput _ = False
isEcho (Echo _) = True
isEcho _ = False

output :: [Style] -> Maybe Output
output (Output x : _) = Just x
output (_  : xs) = output xs
output [] = Nothing

-- |Add styles only if not present
addStyles :: [Style] -> [Style] -> [Style]
addStyles styles style = foldr addStyle style styles

stripDist :: FilePath -> FilePath
stripDist path = maybe path (\ n -> "..." ++ drop (n + 7) path) (isSublistOf "/dists/" path)

verbosity :: [Style] -> Int
verbosity [] = 0
verbosity (Verbosity n : _) = n
verbosity (_ : etc) = verbosity etc

indent :: [Style] -> String
indent [] = ""
indent (Indent s : _) = s
indent (_ : etc) = indent etc

readStyle :: String -> Maybe Style
-- FIXME: implement this
readStyle text =
    case (mapSnd tail . break (== '=')) text of
      ("Start", message) -> Just $ Start message
      ("Finish", message) -> Just $ Finish message
      ("Error", message) -> Just $ Error message
      ("Output", "Indented") -> Just $ Output Indented
      ("Output", "Dots") -> Just $ Output Dots
      ("Output", "Done") -> Just $ Output Done
      ("Output", "Quiet") -> Just $ Output Quiet
      ("Echo", flag) -> Just $ Echo (readFlag flag)
      ("Elapsed", flag) -> Just $ Elapsed (readFlag flag)
      ("Verbosity", value) -> Just $ Verbosity (read value)
      ("Indent", prefix) -> Just $ Indent prefix
      _ -> Nothing
    where
      readFlag "yes" = True
      readFlag "no" = False
      readFlag "true" = True
      readFlag "false" = True
      readFlag text = error ("Unrecognized bool: " ++ text)

-- |The timeDiffToString function returns the empty string for
-- the zero time diff, this is not the behavior I need.
fixedTimeDiffToString :: TimeDiff -> [Char]
fixedTimeDiffToString diff =
    case timeDiffToString diff of
      "" -> "0 sec"
      s -> s

showElapsed :: String -> IO a -> IO a
showElapsed label f =
    do
      (result, time) <- elapsed f
      ePut (label ++ fixedTimeDiffToString time)
      return result

elapsed :: IO a -> IO (a, TimeDiff)
elapsed f =
    do
      start <- getClockTime
      result <- f
      finish <- getClockTime
      return (result, diffClockTimes finish start)

isSublistOf :: Eq a => [a] -> [a] -> Maybe Int
isSublistOf sub lst =
    maybe Nothing (\ s -> Just (length s - length sub))
              (find (isSuffixOf sub) (inits lst))

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

ePut :: String -> IO ()
ePut s = hPutStrLn stderr s
