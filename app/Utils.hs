module Utils
  ( splitWhen, first, second, both, rotate)
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
