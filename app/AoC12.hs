module Main where

import Common
import Data.List (findIndex, intercalate, sort, transpose)
import Data.Maybe (fromJust)
import Utils as U

-- Input parsing
--            start       end         elevation
type Input = ((Int, Int), (Int, Int), [[Int]])

parseSquare :: Char -> Int
parseSquare =
  fromJust
    . flip lookup ([('S', 1), ('E', 26)] ++ zip ['a' .. 'z'] [1 ..])

rawToInput :: String -> Input
rawToInput raw = ((xs, ys), (xe, ye), elevations)
  where
    xs = fromJust $ findIndex (== 'S') $ lns !! ys
    ys = fromJust $ findIndex (elem 'S') lns
    xe = fromJust $ findIndex (== 'E') $ lns !! ye
    ye = fromJust $ findIndex (elem 'E') lns
    elevations = map (map parseSquare) lns
    lns = lines raw

-- Part1
type Result1 = Int

oneLine :: [(Int, Int)] -> [(Int, Int)]
oneLine =
  map
    ( \[(pe, pd), (ce, cd), (ne, nd)] ->
        ( ce,
          min
            (if (ce - pe) <= 1 then min (pd + 1) cd else cd)
            (if (ce - ne) <= 1 then min (nd + 1) cd else cd)
        )
    )
    . U.slidingWin 3

onePass :: ([(Int, Int)] -> [(Int, Int)]) -> [[(Int, Int)]] -> [[(Int, Int)]]
onePass eval mapP =
  zipWith
    (zipWith (\(e, d1) (_, d2) -> (e, min d1 d2)))
    (map eval (wall mapP))
    (transpose . map eval . wall . transpose $ mapP)
  where
    wall = map ((:) (28, maxDist) . (++ [(28, maxDist)]))
    maxDist = length mapP * length (head mapP)

f1 :: Input -> Result1
f1 ((xs, ys), (xe, ye), elevs) =
  (\l -> snd $ l !! ye !! xe)
    . U.fixpoint (onePass oneLine)
    . U.setList ys (U.setList xs (U.second (const 0)))
    . map (map (\e -> (e, maxDist)))
    $ elevs
  where
    maxDist = length elevs * length (head elevs)

-- Part2
type Result2 = Int

oneLine2 :: [(Int, Int)] -> [(Int, Int)]
oneLine2 lnP =
  map
    ( \[(pe, pd), (ce, cd), (ne, nd)] ->
        ( ce,
          min
            (if (pe - ce) <= 1 then min (pd + 1) cd else cd)
            (if (ne - ce) <= 1 then min (nd + 1) cd else cd)
        )
    )
    . U.slidingWin 3

f2 :: Input -> Result2
f2 (_, (xe, ye), elevs) =
  minimum
    . map snd
    . filter ((==) 1 . fst)
    . concat
    . U.fixpoint (onePass oneLine2)
    . U.setList ye (U.setList xe (U.second (const 0)))
    . map (map (\e -> (e, maxDist)))
    $ elevs
  where
    maxDist = length elevs * length (head elevs)

-- Main
main :: IO ()
main = defMain rawToInput f1 f2
