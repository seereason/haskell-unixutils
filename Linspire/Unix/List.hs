-- | A function taken From missingh, which will not build under
-- sid at the moment.

module Linspire.Unix.List
    (join,
     consperse)
    where

import Data.List

join :: [a] -> [[a]] -> [a]
join x l = concat . intersperse x $ l

consperse :: [a] -> [[a]] -> [a]
consperse = join
