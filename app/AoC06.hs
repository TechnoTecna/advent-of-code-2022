module Main where

import Common
import Utils as U
import Data.Maybe (fromJust)
import Data.List (findIndex, nub)


-- Input parsing
type Input = String

rawToInput :: String -> String
rawToInput = id

-- Part1
type Result1 = Int

-- find the index of the first 4-length sequence that do not have repeated
-- characteres (the index of the last character of such sequence).
f1 :: Input -> Result1
f1 = (+) 4           -- 3 -> 7
     . fromJust      -- Just 3 -> 3
     . findIndex (
         (==) 4         -- 3 -> False
         . length       -- "mjq" -> 3
         -- delete replicates
         . nub          -- "mjqj" -> "mjq"
       )              -- ["mjqj","jqjp","qjpq","jpqm","pqmg"] -> Just 3
     -- Sliding window the funny way.
     -- Thanks to lazyness we only generate this list up until the first non
     -- repeating sequence (here until "jpqm")
     . U.slidingWin 4 -- "mjqjpqmg" -> ["mjqj","jqjp","qjpq","jpqm","pqmg"]

-- Part2
type Result2 = Int

-- Just like f1 but with a sliding window of 14
f2 :: Input -> Result2
f2 = (+) 14 . fromJust . findIndex ((==) 14 . length . nub) . U.slidingWin 14

-- Main
main :: IO()
main = defMain rawToInput f1 f2
