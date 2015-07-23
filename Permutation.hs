-- vim: set expandtab:
module Permutation(
    Permutation(Permute),
    cyclicPermutation,
    toPermutation,
    permutee,
    disjComp,
    generatePermutation,
    applyPermutation,
    simplifyPermutation, -- do I want to export this?
    emptyPermutation,
    cycleType,
    cycleGenericType,
    inversePermutation,
    conjugatedBy
)
where

import Data.Set (Set, empty, insert, map, singleton, union, unions, minView, (\\), filter, toAscList)
import Prelude hiding (map, filter, foldr)
import Control.Applicative (liftA2)
import Data.Foldable (foldr)
import Cycle

-- consider making an "unsimplified" permutation type that uses a list
-- internally and does not rely on the composing cycles being disjoint, and
-- another type for "simplified" permutations, that is like this one and uses a
-- set of disjoint cycles
newtype Permutation a = Permute {getCycles :: Set (Cycle a)}

instance Show a => Show (Permutation a) where
    show (Permute cs) = foldMap show cs

instance (Read a, Ord a) => Read (Permutation a) where
    -- this probably can't work with Permutations of Cycles or of Permutations
    -- because of the limitations of the instance for Cycle
    readsPrec _ input = [(mconcat $ fmap cyclicPermutation cs, str) | (cs, str) <- parse input]
        where parse "" = [([], "")]
              parse str = reads str >>= (\(c, rest) -> [(c:cs, str) | (cs,str) <- parse rest])

cyclicPermutation :: Cycle a -> Permutation a
cyclicPermutation = Permute . singleton

toPermutation :: Ord a => [[a]] -> Permutation a
toPermutation = mconcat . fmap (cyclicPermutation . toCycle)

permutee :: Ord a => Permutation a -> Set a
permutee = foldr (union . cyclee) empty . getCycles

disjComp :: Ord a => Permutation a -> Permutation a -> Permutation a
Permute p `disjComp` Permute q = Permute $ union p q

applyPermutation :: Eq a => Permutation a -> a -> a
applyPermutation (Permute p) x = foldr applyCycle x p

emptyPermutation :: Permutation a
emptyPermutation = Permute empty

instance Ord a => Monoid (Permutation a) where
    mempty = emptyPermutation
    mappend = compose
    mconcat = combine

-- The format of these relies on the implementation of Ord for Cycle
cycleType :: Permutation a -> [Int]
cycleType = fmap cycleLength . toAscList . getCycles

cycleGenericType :: Num n => Permutation a -> [n]
cycleGenericType = fmap cycleGenericLength . toAscList . getCycles

inversePermutation :: Ord a => Permutation a -> Permutation a
inversePermutation (Permute p) = Permute $ map inverseCycle p

conjugatedBy :: Ord a => Permutation a -> Permutation a -> Permutation a
conjugatedBy p q = mconcat [inversePermutation q, p, q]

generatePermutation :: Ord a => (a -> a) -> Set a -> Permutation a
generatePermutation f = Permute . filter (/= emptyCycle) . go
    where go s = case minView s of
                    Nothing -> empty
                    Just (e,es) -> let c = generateCycle f e in insert c $ go (es \\ cyclee c)


simplifyPermutation :: Ord a => Permutation a -> Permutation a
simplifyPermutation = liftA2 generatePermutation applyPermutation permutee

compose :: Ord a => Permutation a -> Permutation a -> Permutation a
compose p q = generatePermutation (applyPermutation p . applyPermutation q) (permutee p `union` permutee q)

combine :: (Functor t, Foldable t, Ord a) => t (Permutation a) -> Permutation a
combine ps = generatePermutation (foldr ((.) . applyPermutation) id ps) (foldr union empty $ fmap permutee ps)
