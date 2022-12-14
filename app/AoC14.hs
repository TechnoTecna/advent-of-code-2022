{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Common
import Control.DeepSeq (NFData)
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Utils as U

-- Input parsing
type Input = [[(Int, Int)]]

parsePath :: String -> [(Int, Int)]
parsePath =
  map
    ( U.second (read . takeWhile (/= ' ') . tail)
        . U.first (read . reverse . takeWhile (/= ' ') . reverse)
        . break (== ',')
    )
    . U.splitWhen (== '>')

rawToInput :: String -> Input
rawToInput =
  map parsePath
    . lines

-- Part1
type Result1 = Int

data Tile = A | R | S
  deriving (Eq, Show, Generic, NFData)

lineRock :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
lineRock (xs, ys) (xe, ye) =
  case compare xs xe of
    LT -> [(x, ye) | x <- [xs .. xe]]
    GT -> [(x, ye) | x <- [xe .. xs]]
    EQ -> case compare ys ye of
      LT -> [(xs, y) | y <- [ys .. ye]]
      GT -> [(xs, y) | y <- [ye .. ys]]
      EQ -> [(xs, ys)]

toRocks :: Input -> [(Int, Int)]
toRocks =
  concatMap (concatMap (\[s, e] -> lineRock s e) . U.slidingWin 2)

newGrid :: [(Int, Int)] -> ([[Tile]], (Int, Int))
newGrid rocks =
  ( foldl
      (\grd (x, y) -> U.setList x (U.setList y (const R)) grd)
      (replicate (maxX - minX + 3) (replicate (maxY - minY + 2) A))
      rocks',
    (501 - minX, 0)
  )
  where
    rocks' =
      map (U.second (flip (-) minY) . U.first ((+ 1) . flip (-) minX)) rocks
    maxX = max (500 + (maxY - minY) + 1) . maximum . (500 :) . map fst $ rocks
    minX = min (500 - (maxY - minY) - 1) . minimum . (500 :) . map fst $ rocks
    maxY = maximum . (0 :) . map snd $ rocks
    minY = minimum . (0 :) . map snd $ rocks

step :: [[Tile]] -> (Int, Int) -> (Int, Int)
step grd (x, y)
  | y >= length (head grd) - 1 = (-1, y)
  | grd !! x !! (y + 1) == A = (x, y + 1)
  | grd !! (x - 1) !! (y + 1) == A = (x - 1, y + 1)
  | grd !! (x + 1) !! (y + 1) == A = (x + 1, y + 1)
  | otherwise = (x, y)

oneSand :: (Int, Int) -> [[Tile]] -> [[Tile]]
oneSand src grd =
  if x == -1
    then grd
    else U.setList x (U.setList y (const S)) grd
  where
    (x, y) = U.fixpoint (step grd) src

f1 :: Input -> Result1
f1 = fst . (\(grd, src) -> U.fixcount (oneSand src) 0 grd) . newGrid . toRocks

-- Part2
type Result2 = Int

f2 :: Input -> Result2
f2 =
  fst
    . (\(grd, src) -> U.fixcount (oneSand src) 0 grd)
    . U.first (map (++ [R])) -- Adding floor
    . newGrid
    . toRocks

-- Main
main :: IO ()
main = defMain rawToInput f1 f2
