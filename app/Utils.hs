module Utils
  ( splitWhen, first, second, both, rotate, chunksOf, setList )
  where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s = case dropWhile p s of
                  [] -> []
                  t  -> w : splitWhen p t2
                    where (w, t2) = break p t

first :: (a -> a') -> (a, b) -> (a', b)
first f (x, y) = (f x, y)

second :: (b -> b') -> (a, b) -> (a, b')
second f (x, y) = (x, f y)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a1, a2) = (f a1, f a2)

-- rotate list left (such that "rotate n l" makes "l !! n" the
-- first)
rotate :: Int -> [a] -> [a]
rotate n l = uncurry (flip (++)) $ splitAt (n `mod` length l) l

-- cut up a list in chunks of length n. For example:
-- chunksOf 3 [1, 2, 3, 4, 5, 6, 7, 8] -> [[1, 2, 3], [4, 5, 6], [7, 8]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

-- Unsafe way to modify a list
-- apply f to the nth element of the list
setList :: Int -> (a -> a) -> [a] -> [a]
setList n f l = take n l ++ [f (l!!n)] ++ drop (n+1) l
