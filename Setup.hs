#!/usr/bin/runhaskell

import Distribution.Simple
import System.Cmd
import System.Exit

main = defaultMainWithHooks simpleUserHooks
