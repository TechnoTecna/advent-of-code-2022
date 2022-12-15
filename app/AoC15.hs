module Main where

import Common
import Data.List (find, sortBy)
import Data.Maybe (fromJust)
import Utils as U

-- Input parsing
type Input = [((Int, Int), (Int, Int))]

rawToInput :: String -> Input
rawToInput =
  map
    ( (\[xs, ys, xb, yb] -> ((xs, ys), (xb, yb)))
        . map (read . reverse . takeWhile (/= '=') . reverse)
        . concatMap (U.splitWhen (== ','))
        . U.splitWhen (== ':')
    )
    . lines

-- Part1
type Result1 = Int

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (xs, ys) (xe, ye) = abs (xe - xs) + abs (ye - ys)

sectionAt :: Int -> ((Int, Int), (Int, Int)) -> (Int, Int)
sectionAt y ((xs, ys), (xb, yb)) =
  (xs - (width - dist), xs + (width - dist))
  where
    dist = abs (ys - y)
    width = manhattan (xs, ys) (xb, yb)

lineSlice :: Int -> Input -> [(Int, Int)]
lineSlice yS =
  foldl
    ( \l (s, e) ->
        case l of
          [] -> [(s, e)]
          (sl, el) : ta
            | s - el <= 1 -> (sl, max e el) : ta
            | otherwise -> (s, e) : (sl, el) : ta
    )
    []
    . sortBy (\(s1, _) (s2, _) -> compare s1 s2)
    . filter (\(s, e) -> s <= e)
    . map (sectionAt yS)

f1 :: Input -> Result1
f1 =
  sum
    . map (\(s, e) -> e - s)
    . lineSlice 2000000

-- Part2
type Result2 = Int

freeSpace :: Int -> Int -> [(Int, Int)] -> Int
freeSpace s e l
  | length l > 1 = fst (head l) - 1
  | fst (last l) > s = s
  | otherwise = e

f2 :: Input -> Result2
f2 input =
  x * 4000000 + y
  where
    (x, y) =
      (\(y, l) -> (freeSpace 0 4000000 l, y))
        . fromJust
        . find
          ( \(_, l) ->
              length l > 1 || fst (last l) > 0 || snd (head l) < 4000000
          )
        $ zip [0 ..] slices
    slices = map (flip lineSlice input) [0 .. 4000000]

-- Main
main :: IO ()
main = defMain rawToInput f1 f2
