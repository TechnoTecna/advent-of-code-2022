module Main where

import Common
import Data.List (intersect, elemIndex)
import Data.Maybe (fromJust)


-- Input parsing
type Input = [[Char]]

rawToInput :: String -> Input
rawToInput = lines -- "vJrw\njqHRNq\nPm" -> ["vJrw", "jqHRNq", "Pm"]

-- Part1
type Result1 = Int

priority :: Char -> Int
priority = (+1) . fromJust . flip elemIndex (['a'..'z'] ++ ['A'..'Z'])

f1 :: Input -> Result1
f1 =
  sum -- [22, 34, 42] -> 98
  . map (
      priority -- 'v' -> 22
      . ( \bag ->
        head                               -- "v" -> 'v'
        $ uncurry intersect                -- ("vJ", "rv") -> "v"
        $ splitAt (length bag `div` 2) bag -- "vJrv" -> ("vJ", "rv")
      )        -- "vJrv" -> 'v'
    ) -- ["vJrv", "jqHRHq", "PP"] -> [22, 34, 42]

-- Part2
type Result2 = Int

-- cut up a list in chunks of length n. For example:
-- chunksOf 3 [1, 2, 3, 4, 5, 6, 7, 8] -> [[1, 2, 3], [4, 5, 6], [7, 8]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

f2 :: Input -> Result2
f2 =
  sum          -- [42, 18] -> 60
  . map (
      priority           -- 'P' -> 42
      . head             -- "P" -> 'P'
      . foldr1 intersect -- ["vJrP", "jqHRPq", "Pm"] -> "P"
    )          -- [["vJrP", "jqHRPq", "Pm"], ["wMqrLMZ", "trgJ", "Cr"]] ->
               -- [42, 18]
  . chunksOf 3 -- ["vJrP", "jqHRPq", "Pm", "wMqrLMZ", "trgJ", "Cr"] ->
               -- [["vJrP", "jqHRPq", "Pm"], ["wMqrLMZ", "trgJ", "Cr"]]

-- Main
main :: IO()
main = defMain rawToInput f1 f2
