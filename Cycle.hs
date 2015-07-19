-- vim: set expandtab:
module Cycle (
    Cycle,
    toCycle,
    emptyCycle,
    cyclee,
    cycleLength,
    cycleGenericLength,
    validCycle,
    generateCycle,
    applyCycle,
    inverseCycle
)
where

import Data.List (tails, inits, genericLength, nub)
import Data.Set (Set, fromList, empty, member, insert, size)

newtype Cycle a = Cycle [a]

toCycle :: Eq a => [a] -> Cycle a
toCycle = Cycle . nub

emptyCycle :: Cycle a
emptyCycle = Cycle []

cyclee :: Ord a => Cycle a -> Set a
cyclee (Cycle xs) = fromList xs

cycleLength :: Cycle a -> Int
cycleLength (Cycle c) = length c

cycleGenericLength :: Num n => Cycle a -> n
cycleGenericLength (Cycle c) = genericLength c


instance Show a => Show (Cycle a) where
    show (Cycle xs) = "(" ++ unwords (map show xs) ++ ")" -- is this efficient?

instance Read a => Read (Cycle a) where
    -- can't parse Cycle of Cycles
    readsPrec _ ('(':input) = [(Cycle xs, str) | (xs, str) <- parse input]
        where parse (')':rest) = [([], rest)]
              parse str = reads str >>= (\(v, rest) -> [(v:xs, str) | (xs, str) <- parse rest])
    readsPrec _ _ = []

instance Eq a => Eq (Cycle a) where
    Cycle [] == Cycle [] = True
    Cycle xs == Cycle ys = any (==ys) $ zipWith (++) (tails xs) (init $ inits xs)

instance Ord a => Ord (Cycle a) where
    Cycle xs `compare` Cycle ys = go xs ys
        where go [] [] = EQ
              go [] _ = LT
              go _ [] = GT
              go (x:xs) (y:ys) = go xs ys `mappend` compare x y

instance Functor Cycle where
    fmap f (Cycle xs) = Cycle $ fmap f xs

validCycle :: Ord a => Cycle a -> Bool
validCycle c = cycleLength c == size (cyclee c)

generateCycleRaw :: Ord a => (a -> a) -> a -> Cycle a
generateCycleRaw = curry $ Cycle . go empty . uncurry iterate
    where go s (x:xs) | member x s = []
                      | otherwise = x:go (insert x s) xs

generateCycle :: Ord a => (a -> a) -> a -> Cycle a
generateCycle f x = let c@(Cycle xs) = generateCycleRaw f x
                    in case xs of
                        [a] -> emptyCycle
                        _ -> c
                                                                

applyCycle :: Eq a => Cycle a -> a -> a
applyCycle (Cycle xs) x = go xs $ tail $ cycle xs
    where go [] _ = x
          go (x':xs) (xx:xxs) | x == x' = xx
                              | otherwise = go xs xxs

inverseCycle :: Cycle a -> Cycle a
inverseCycle (Cycle xs) = Cycle $ reverse xs
