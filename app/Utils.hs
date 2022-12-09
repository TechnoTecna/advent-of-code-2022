module Utils
  ( splitWhen, first, second, both, rotate, chunksOf, setList, slidingWin )
  where

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
