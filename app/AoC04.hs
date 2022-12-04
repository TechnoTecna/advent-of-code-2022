module Main where

import Common
import Utils as U


-- Input parsing
type Input = [((Int, Int), (Int, Int))]

-- "2-4,6-8\n2-3,4-5" -> [((2,4),(6,8)),((2,3),(4,5))]
rawToInput :: String -> Input
rawToInput =
  map (
    U.both (U.both read       -- ("2", "4") -> (2, 4)
            . U.second tail   -- ("2", "-4") -> ("2", "4")
            . break (== '-')) -- "2-4" -> ("2", "-4")
    . U.second tail  -- ("2-4", ",6-8") -> ("2-4", "6-8")
    . break (== ',') -- "2-4,6-8" -> ("2-4", ",6-8")
  )       -- ["2-4,6-8", "2-3,4-5"] -> [((2,4),(6,8)),((2,3),(4,5))]
  . lines -- "2-4,6-8\n2-3,4-5" -> ["2-4,6-8", "2-3,4-5"]

-- Part1
type Result1 = Int

-- Does the range s1..e1 fully contains s2..e2. For example:
-- (2, 4) (6, 8) -> False
-- (6, 6) (4, 6) -> False
-- (2, 8) (3, 7) -> True
containsP :: (Int, Int) -> (Int, Int) -> Bool
containsP (s1, e1) (s2, e2) = (s1 <= s2) && (e1 >= e2)

-- [((2,4),(6,8)),((2,8),(3,7))] -> 1
f1 :: Input -> Result1
f1 =
  sum -- [0, 1] -> 1
  . map (fromEnum                                      -- False -> 0
         . (\(x, y) -> containsP x y || containsP y x) -- ((2,4),(6,8)) -> False
    ) -- [((2,4),(6,8)),((2,8),(3,7))] -> [0, 1]

-- Part2
type Result2 = Int

-- Does the range s1..e1 ovelap s2..e2. For example:
-- (2, 4) (6, 8) -> False
-- (6, 6) (4, 6) -> True
-- (2, 8) (3, 7) -> True
--
-- Note that this function is symmetrical. overlapP a b == overlapP b a
overlapP :: (Int, Int) -> (Int, Int) -> Bool
overlapP (s1, e1) (s2, e2) = e1 >= s2 && s1 <= e2

-- [((2,4),(4,8)),((2,8),(3,7))] -> 2
f2 :: Input -> Result2
f2 = sum -- [1, 1] -> 2
     . map (fromEnum           -- True -> 1
            . uncurry overlapP -- ((2,4),(4,8)) -> True
       ) -- [((2,4),(4,8)),((2,8),(3,7))] -> [1, 1]

-- Main
main :: IO()
main = defMain rawToInput f1 f2
