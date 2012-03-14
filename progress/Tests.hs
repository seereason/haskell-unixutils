import System.Exit
import System.Unix.Progress (tests)
import Test.HUnit

main =
    do (c,st) <- runTestText putTextToShowS (TestList $ System.Unix.Progress.tests)
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         n -> exitFailure
