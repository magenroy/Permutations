module Main where

import Permutation
import Cycle
import System.Environment (getArgs)
import Data.List
import Data.Set

helptext = "invalid input"


parse :: String -> Permutation Int
parse = toPermutation . fmap (fmap read . words . Data.List.filter (`notElem` "()")) . groupBy (\_ b -> b /= '(')


main = getArgs >>= permuteIO

permuteIO ("apply":x:s) = print $ applyPermutation (parse $ unwords s) $ read x
permuteIO ("simplify":s) = print $ parse $ unwords s
permuteIO _ = putStrLn helptext
