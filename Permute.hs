module Main where

import Permutation (Permutation, applyPermutation, cycleType)
import System.Environment (getArgs)

helptext = "invalid input"


-- maybe I can use one of those typeclasses that deal with generic types and
-- related things in order to be able select the type of the permutees?
parse = read :: String -> Permutation Int
--parse = toPermutation . fmap (fmap read . words . Data.List.filter (`notElem` "()")) . groupBy (\_ b -> b /= '(')

get :: String -> Permutation Int
get = parse -- this relies on the preservation of permutations in simplified form (otherwise we need simplyPermutation)

dispatch = [("simplify", simplify),
            ("apply", apply),
            ("type", ctype)]

simplify, apply, ctype :: [String] -> IO ()
simplify = print . get . unwords
apply (x:s) = print . applyPermutation (parse $ unwords s) $ read x
ctype = putStrLn . unwords . fmap show . cycleType . get . unwords

-- should print nicer text for no parse errors.
permuteIO :: [String] -> IO ()
--permuteIO ("apply":x:s) = print $ applyPermutation (parse $ unwords s) $ read x
--permuteIO ("simplify":s) = print $ simplifyPermutation $ parse $ unwords s
--permuteIO _ = putStrLn helptext
permuteIO (('-':'-':opt):s) = case lookup opt dispatch of
                        Just f -> f s
                        Nothing -> putStrLn helptext
permuteIO s = simplify s >> ctype s


main = getArgs >>= permuteIO
