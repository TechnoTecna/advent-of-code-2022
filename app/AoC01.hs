module Main where

import Common (defMain)
import Data.List (sort)
import Utils as U


-- Input parsing
type Input = [[Int]]
rawToInput :: String -> Input
rawToInput = map (map read) . splitWhen  null . lines

-- Part1
type Result1 = Int
f1 :: Input -> Result1
f1 = maximum . map sum

-- Part2
type Result2 = Int
f2 :: Input -> Result2
f2 = sum . take 3 . reverse . sort . map sum

-- Main
main :: IO()
main = defMain rawToInput f1 f2
