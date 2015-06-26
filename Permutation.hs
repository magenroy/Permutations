-- vim: set expandtab:
module Permutation(
    Permutation(Permute),
    cyclicPermutation,
    disjComp,
    generatePermutation,
    applyPermutation,
    simplifyPermutation, -- do I want to export this?
    cycleType,
    cycleGenericType,
    inversePermutation,
    conjugateBy
)
where

import Data.Set (Set, empty, insert, map, singleton, union, unions, minView, (\\), filter, toAscList)
import Prelude hiding (map, filter)
import Control.Applicative (liftA2)
import Cycle

newtype Permutation a = Permute {getCycles :: Set (Cycle a)}

instance Show a => Show (Permutation a) where
    show (Permute cs) = foldMap show cs

cyclicPermutation :: Cycle a -> Permutation a
cyclicPermutation = Permute . singleton

permutee :: Ord a => Permutation a -> Set a
permutee = foldr (union . cyclee) empty . getCycles

disjComp :: Ord a => Permutation a -> Permutation a -> Permutation a
Permute p `disjComp` Permute q = Permute $ union p q

generatePermutation :: Ord a => (a -> a) -> Set a -> Permutation a
generatePermutation f = Permute . filter (/= emptyCycle) . go
    where go s = case minView s of
                    Nothing -> empty
                    Just (e,es) -> let c = generateCycle f e in insert c $ go (es \\ cyclee c)

applyPermutation :: Eq a => Permutation a -> a -> a
applyPermutation (Permute p) x = foldr applyCycle x p

simplifyPermutation :: Ord a => Permutation a -> Permutation a
simplifyPermutation = liftA2 generatePermutation applyPermutation permutee

instance Ord a => Monoid (Permutation a) where
    mappend p q = generatePermutation (applyPermutation p . applyPermutation q) (permutee p `union` permutee q)
    mempty = Permute empty
    mconcat ps = generatePermutation (foldr ((.) . applyPermutation) id ps) (unions $ fmap permutee ps)

-- The format of these relies on the implementation of Ord for Cycle
cycleType :: Permutation a -> [Int]
cycleType = fmap cycleLength . toAscList . getCycles

cycleGenericType :: Num n => Permutation a -> [n]
cycleGenericType = fmap cycleGenericLength . toAscList . getCycles

inversePermutation :: Ord a => Permutation a -> Permutation a
inversePermutation (Permute p) = Permute $ map inverseCycle p

conjugateBy :: Ord a => Permutation a -> Permutation a -> Permutation a
conjugateBy p q = mconcat [inversePermutation p, q, p]
