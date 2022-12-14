{-# LANGUAGE GADTs #-}

module Utils
  ( splitWhen, first, second, both, rotate, chunksOf, setList, slidingWin,
    {-Set (..), addS, elemS, toListS, fromListS, mapS,-} dumbNorm, fastNub,
    fst3, fixpoint, loop )
  where

import Data.List (sort, group)
import Control.DeepSeq (($!!), NFData, force)


-- split a list everywhere `p` is true (discard the matched elements)
-- (just like `split` in python).
-- splitWhen (== '.') "red.blue.yellow" -> ["red", "blue", "yellow"]
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s = case dropWhile p s of
                  [] -> []
                  t  -> w : splitWhen p t2
                    where (w, t2) = break p t

-- apply f to the left side of a pair
first :: (a -> a') -> (a, b) -> (a', b)
first f (x, y) = (f x, y)

-- apply f to the right side of a pair
second :: (b -> b') -> (a, b) -> (a, b')
second f (x, y) = (x, f y)

-- apply f to both sides of a pair
both :: (a -> b) -> (a, a) -> (b, b)
both f (a1, a2) = (f a1, f a2)

-- rotate list left (such that "rotate n l" makes "l !! n" the first)
rotate :: Int -> [a] -> [a]
rotate n l = uncurry (flip (++)) $ splitAt (n `mod` length l) l

-- cut up a list in chunks of length n. For example:
-- chunksOf 3 [1, 2, 3, 4, 5, 6, 7, 8] -> [[1, 2, 3], [4, 5, 6], [7, 8]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

-- Unsafe way to modify a list. Apply f to the nth element of the list
setList :: Int -> (a -> a) -> [a] -> [a]
setList n f l = take n l ++ [f (l!!n)] ++ drop (n+1) l

-- "Sliding Window"
-- Gives you the succesives slices you would get with a sliding window of size
-- `sz`.
-- slidingWin 3 "Hello !" -> ["Hel", "ell", "llo", "lo ", "o !"]
-- slidingWin 4 "Hello !" -> ["Hell", "ello", "llo ", "lo !"]
slidingWin :: Int -> [a] -> [[a]]
slidingWin sz lst =
  scanl (\acc x -> tail acc ++ [x]) (take sz lst) (drop sz lst)


-- Set data type implemented as a tree (necesitate to have an order on youre
-- type parameter)
-- data Set a where
--   Branch :: (Ord a, Eq a) => a -> Set a -> Set a -> Set a
--   Leaf :: (Ord a, Eq a) => Set a

-- instance (Show a) => Show (Set a) where
--   show Leaf = "Lf"
--   show (Branch x lft rgt) =
--     show x ++ " (" ++ show lft ++ "), (" ++ show rgt ++ ")"

-- addS :: a -> Set a -> Set a
-- addS x Leaf = Branch x Leaf Leaf
-- addS x b@(Branch x' lft rgt)
--   | x == x' = b
--   | x < x' = Branch x' (addS x lft) rgt
--   | x > x' = Branch x' lft (addS x rgt)

-- elemS :: a -> Set a -> Bool
-- elemS x Leaf = False
-- elemS x (Branch x' lft rgt)
--   | x == x' = True
--   | x < x' = elemS x lft
--   | x > x' = elemS x rgt

-- toListS :: Set a -> [a]
-- toListS Leaf = []
-- toListS (Branch x lft rgt) = x : toListS lft ++ toListS rgt

-- fromListS :: Ord a => [a] -> Set a
-- fromListS = foldl (flip addS) Leaf

-- mapS :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
-- mapS f Leaf = Leaf
-- mapS f (Branch x lft rgt) = Branch (f x) (mapS f lft) (mapS f rgt)


-- "normalise" a number to 1 or -1 (or 0 if 0). Usefull for "step by step"
-- movement
dumbNorm :: Int -> Int
dumbNorm i =
  case compare i 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

-- Just like nub but only on type with an order and doesn't preserve list order.
-- It has a Complexity of O(n log(n)) instead of O(n^2)
fastNub :: (Ord a) => [a] -> [a]
fastNub = map head . group . sort

-- Just like fst but for tuples of size 3
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- loop f on itself until we reach a fixpoint
fixpoint :: (Eq a, NFData a) => (a -> a) -> a -> a
fixpoint f orig
  | new == orig = orig
  | otherwise = fixpoint f new
  where new = force (f orig)

-- loop f on itself x times
loop :: NFData a => Int -> (a -> a) -> a -> a
loop 0 _ init = init
loop n f init = loop (n - 1) f $!! f init
-- loop i f init =
--   foldl (\x _ -> f x)
--         init
--         [1 .. i]
