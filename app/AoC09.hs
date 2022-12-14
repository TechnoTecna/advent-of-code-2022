module Main where

import Common
import Data.Maybe (fromJust)
import Utils as U

-- Input parsing
data Move = R | U | L | D
  deriving (Eq, Show)

type Input = [(Move, Int)]

rawToInput :: String -> Input
rawToInput =
  map
    ( -- ("R", 4) -> (R, 4)
      U.first (fromJust . flip lookup [("R", R), ("U", U), ("L", L), ("D", D)])
        . U.second (read . tail) -- ("R", 4)
        . break (== ' ') -- "R 4" -> ("R", " 4")
    ) -- ["R 4", "U 4", "L 3"] -> [(R,4), (U,4), (L,3)]
    . lines -- "R 4\nU 4\nL 3\n" -> ["R 4", "U 4", "L 3"]

-- Part1
type Result1 = Int

-- take a starting position and a move and gives you an ending position
-- newPos (1, 3) U -> (1, 4)
newPos :: (Int, Int) -> Move -> (Int, Int)
newPos (x, y) mv =
  case mv of
    R -> (x + 1, y)
    U -> (x, y + 1)
    L -> (x - 1, y)
    D -> (x, y - 1)

-- toPos (0,0) [(R,4),(U,4),(L,3)] -> [(0,0),(4,0),(4,4),(1,4)]
toPos :: (Int, Int) -> [Move] -> [(Int, Int)]
toPos = scanl newPos

-- [(R, 4), (U, 4)] -> [R,R,R,R,U,U,U,U]
stepByStep :: Input -> [Move]
stepByStep = concatMap (uncurry $ flip replicate)

-- given the position of the tail and the position of the head gives you the
-- the new position of the tail that touches the head. (Asume we only need one
-- move to do so)
-- traceOne (0, 0) (1, 2) -> (1, 1)
traceOne :: (Int, Int) -> (Int, Int) -> (Int, Int)
traceOne (tx, ty) (hx, hy)
  | abs dx <= 1 && abs dy <= 1 = (tx, ty)
  | otherwise = (tx + dxN, ty + dyN)
  where
    -- normalize delta X and delta Y (-3 -> -1, 45 -> 1, 0 -> 0)
    dxN = U.dumbNorm dx
    dyN = U.dumbNorm dy
    dx = hx - tx
    dy = hy - ty

-- given a starting position and a list of head position, gives you the list of
-- all positions the tail will go throught to follow the head
-- (Note that if the tail doesn't need to move for a few steps there will be
-- successive repeated position [... (0,1),(0,1),(0,1) ...])
-- traceAll (0,0) [(0,1),(1,1),(1,2)] -> [(0,0),(0,0),(0,0),(1,1)]
traceAll :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
traceAll = scanl traceOne

f1 :: Input -> Result1
f1 =
  length -- count unique positions
    . U.fastNub -- eliminate duplicates
    . traceAll (0, 0) -- Path of tail following head ([(0,0),(0,0),(1,0),...])
    . toPos (0, 0) --  [R,R,R,R,U] -> [(0,0),(1,0),(2,0),(3,0),(4,0),(4,1)]
    . stepByStep -- [(R,4),(U,4),(L,3)] -> [R,R,R,R,U,U,U,U,L,L,L]

-- Part2
type Result2 = Int

f2 :: Input -> Result2
f2 input =
  length -- count unique positions
    . U.fastNub -- eliminate duplicates
    -- apply traceAll successivly: calculate all position of 1 following Head
    -- then use those positions as head position followed by 2 and so on util 9
    $ foldl
      (\x _ -> traceAll (0, 0) x)
      (toPos (0, 0) $ stepByStep input) -- path of the head
      -- Note that the only usefull thing about this list is its length.
      -- we discard its content.
      (replicate 9 []) -- [[],[],[],[],[],[],[],[],[]]

-- Main
main :: IO ()
main = defMain rawToInput f1 f2
