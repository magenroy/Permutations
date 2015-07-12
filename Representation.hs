-- vim: set expandtab:
module Representation where

import Cycle
import Permutation
import Data.Array
import Data.Set (findMin, findMax)

import Data.List (groupBy)

linearizeCycle :: Ix i => Cycle i -> Array i a -> Array i a
linearizeCycle c arr = ixmap (bounds arr) (applyCycle c) arr

linearizePermutation :: Ix i => Permutation i -> Array i a -> Array i a
linearizePermutation p arr = ixmap (bounds arr) (applyPermutation p) arr

permutationMatrix :: Num a => Ix i => Permutation i -> Array (i,i) a
permutationMatrix p = let s = permutee p
                          (first, last) = (findMin s, findMax s)
                          vals = range ((first, first), (last, last))
                      in array ((first, first), (last, last)) [((i,j), if j == applyPermutation p i then 1 else 0) | (i,j) <- vals]

showMatrix :: (Ix i, Show a) => Array (i,i) a -> String
showMatrix = unlines . map (unwords . map show) . toList
    where toList a = map (map snd) . groupBy (\((a,_),_) ((b,_),_) -> a == b) . assocs $ a
