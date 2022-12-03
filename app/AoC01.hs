module Main where

import Common (defMain)
import Data.List (sort)
import Utils as U


-- Input parsing
type Input = [[Int]]

-- | "1000\n2000\n3000\n\n4000\n" -> [[1000, 2000, 3000], [4000]]
rawToInput :: String -> Input
rawToInput =
  map (map read)    -- [["1", "2", "3"], ["4"]] -> [[1, 2, 3], [4]]
  . splitWhen  null -- ["1", "2", "3", "", "4"] -> [["1", "2", "3"], ["4"]]
  . lines           -- "1\n2\n3\n\n4" -> ["1", "2", "3", "", "4"]

-- Part1
type Result1 = Int

f1 :: Input -> Result1
f1 = maximum   -- [6, 4, 11, 24] -> 45
     . map sum -- [[1, 2, 3], [4], [5, 6], [7, 8, 9]] -> [6, 4, 11, 24]

-- Part2
type Result2 = Int

f2 :: Input -> Result2
f2 = sum       -- [24, 11, 6] -> 41`
     . take 3  -- [24, 11, 6, 4] -> [24, 11, 6]
     . reverse -- [4, 6, 11, 24] -> [24, 11, 6, 4]
     . sort    -- [6, 4, 11, 24] -> [4, 6, 11, 24]
     . map sum -- [[1, 2, 3], [4], [5, 6], [7, 8, 9]] -> [6, 4, 11, 24]

-- Main
main :: IO()
main = defMain rawToInput f1 f2
