#!/usr/bin/runhaskell

import Distribution.Simple
import System.Process
import System.Exit

main = defaultMainWithHooks simpleUserHooks
