module Main where

import Common
import Data.List (transpose)


-- Input parsing
type Input = [[Int]]

rawToInput :: String -> Input
rawToInput =
  map (
    map (
      read     -- "3" -> 3
      . (: []) -- '3' -> "3"
    )       -- "30373" -> [3, 0, 3, 7, 3]
  )       -- ["30373", "25512"] -> [[3, 0, 3, 7, 3], [2, 5, 5, 1, 2]]
  . lines -- "30373\n25512" -> ["30373", "25512"]

-- Part1
type Result1 = Int

-- given a rwo of tree aligned one behind the other relative to you. Tell you
-- for each of them wich would be visible or not.
-- [3, 0, 3, 7, 3] -> [True, False, False, True, False]
visibleRow :: [Int] -> [Bool]
-- for each tree, check whether or not it is taller than the previous max height
visibleRow row = zipWith (>) row maxR
        -- cumulative maximum height
  where maxR = scanl max (-1) row -- [3,0,3,7,3] -> [-1,3,3,3,7,7]

f1 :: Input -> Result1
f1 patch =
  -- we sum every row
  -- [3, 3, ...] -> 42
  sum
  -- we cast bools to integer and count the number of visible trees for each row
  -- [[T,T,F,T,F],[T,T,T,F,F], ...] -> [3, 3, ...]
  $ map (sum . map fromEnum)
  -- we accumulate every visibility map from each pov with a bi OR to have an
  -- absolute visibility map
  -- [[[T,F,F,T,F],[T,T,F,F,F], ...],[[T,T,F,F,F],[T,F,T,F,F], ...], ...]
  -- ->
  -- [[T,T,F,T,F],[T,T,T,F,F], ...]
  $ foldr1 (zipWith $ zipWith (||)) -- [[T,F,F,T,F],[T,]]
  -- [[[T,F,F,T,F],[T,T,F,F,F], ...],[[T,T,F,F,F],[T,F,T,F,F], ...], ...]
    [west, east, north, south]
  where
    -- visibility map from each point of view
    west = map visibleRow patch
    east = map (reverse . visibleRow . reverse) patch
    north = transpose $ map visibleRow $ transpose patch
    south = transpose $ map (reverse . visibleRow . reverse) $ transpose patch

-- Part2
type Result2 = Int

-- take a row of tree and each tree gives us this number:
-- if a treehouse was on top of it how many tree could you see facing the end of
-- the row.
-- [3,0,3,7,3] -> [2,1,1,1,0]
forwardVis :: [Int] -> [Int]
forwardVis [] = []
forwardVis (h:t) = nbVis : forwardVis t
  where
    -- we count all the tree we can see (see next (or last really) comment)
    nbVis = length smaller + fromEnum (not $ null taller)
    -- from the current tree you can see all trees in smaller plus the firs in
    -- taller
    (smaller, taller) = break (>= h) t

-- Just like f1 except
-- - we map with forwardVis instead of visibility
-- - we zip with (*) instead of (||)
-- - we fold with maximum instead of sum
f2 :: Input -> Result2
f2 patch =
  maximum
  $ map maximum
  $ foldr1 (zipWith $ zipWith (*))
    [west, east, north, south]
  where
    west = map forwardVis patch
    east = map (reverse . forwardVis . reverse) patch
    north = transpose $ map forwardVis $ transpose patch
    south = transpose $ map (reverse . forwardVis . reverse) $ transpose patch


-- Main
main :: IO()
main = defMain rawToInput f1 f2
