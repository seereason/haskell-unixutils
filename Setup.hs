#!/usr/bin/runhaskell

import Distribution.Simple
import System.Cmd
import System.Exit

main = defaultMainWithHooks simpleUserHooks {
         postBuild = runTestScript
       , runTests = runTestScript
       }

runTestScript _args _flag _pd _lbi =
    system "runhaskell Tests.hs" >>=
    \ code -> if code == ExitSuccess then return () else error "Test Failure"
