module Main where

import Permutation (Permutation, applyPermutation, simplifyPermutation)
import System.Environment (getArgs)

helptext = "invalid input"


parse = read :: String -> Permutation Int
--parse = toPermutation . fmap (fmap read . words . Data.List.filter (`notElem` "()")) . groupBy (\_ b -> b /= '(')


main = getArgs >>= permuteIO

permuteIO :: [String] -> IO ()
permuteIO ("apply":x:s) = print $ applyPermutation (parse $ unwords s) $ read x
permuteIO ("simplify":s) = print $ simplifyPermutation $ parse $ unwords s
permuteIO _ = putStrLn helptext
