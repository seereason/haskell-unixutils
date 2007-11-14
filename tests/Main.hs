module Main where

import Test.HUnit

test1 = TestCase (assertEqual "1==1" True (1==1))

tests = TestList [test1]

main = runTestTT tests
