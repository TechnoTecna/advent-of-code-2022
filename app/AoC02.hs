module Main where

import Common
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Utils as U


-- Input parsing
data Move = Rock | Paper | Scissors
  deriving (Show, Eq)
data Inst = X | Y | Z
  deriving (Show, Eq)

type Input = [(Move, Inst)]

rawToInput :: String -> Input
rawToInput =
  map (
    U.second (fromJust . flip lookup insts . head) -- (Rock, "Y") -> (Rock, Y)
    . U.first (fromJust . flip lookup mvs . head)  -- ("A ", "Y") -> (Rock, "Y")
    . splitAt 2                                    -- "A Y" -> ("A ", "Y")
  )       -- ["A Y", "B X", "C Z"] -> [(Rock, Y), (Paper, X), (Scissors, Z)]
  . lines -- "A Y\nB X\nC Z" -> ["A Y", "B X", "C Z"]
  where mvs = [('A', Rock), ('B', Paper), ('C', Scissors)]
        insts = [('X', X), ('Y', Y), ('Z', Z)]

-- Part1
data Result = Win | Loss | Draw
  deriving (Eq, Show)

moveOrder :: [Move]
-- A circle that encode wich move beats wich   Rock <-- Paper
--                                             |            ^
--                                             +->Scissors--+
moveOrder = [Rock, Paper, Scissors]

result :: (Move, Move) -> Result
result (mOp, mPl) =
  -- rotate move order to make the player's move `mPl` first.
  -- for example if `mPl = Paper` we have
  -- `m0 = Paper , m1 = Scissors , m2 = Rock`
  -- in every case m0 is the player's move, m1 beats the player and m2 loses to
  -- the player.
  case U.rotate (fromJust $ elemIndex mPl moveOrder) moveOrder of
    [m0, m1, m2] | m2 == mOp -> Win
                 | m1 == mOp -> Loss
                 | m0 == mOp -> Draw

mvPnts :: Move -> Int
mvPnts m = fromJust $ lookup m [(Rock, 1), (Paper, 2), (Scissors, 3)]

resPnts :: Result -> Int
resPnts r = fromJust $ lookup r [(Win, 6), (Loss, 0), (Draw, 3)]

score :: (Move, Move) -> Int
score r@(_, mPl) = mvPnts mPl + resPnts (result r)

instToMove :: Inst -> Move
instToMove = fromJust . flip lookup [(X, Rock), (Y, Paper), (Z, Scissors)]

type Result1 = Int

f1 :: Input -> Result1
f1 = sum -- [8, 1, 6] -> 15
     . map (
         score                 -- (Rock, Paper) -> 8
         . U.second instToMove -- (Rock, Y) -> (Rock, Paper)
       ) -- [(Rock, Y), (Paper, X), (Scissors, Z)] -> [8, 1, 6]

-- Part2
findMove :: Move -> Result -> Move
findMove mOp res =
  -- Same princilple as for `result`. We just pick a move instead of testing
  -- for equality.
  let mvs = U.rotate (fromJust $ elemIndex mOp moveOrder) moveOrder in
  case (res, mvs) of
    (Win, [_, mPl, _])  -> mPl
    (Loss, [_, _, mPl]) -> mPl
    (Draw, [mPl, _, _]) -> mPl

instToResult :: Inst -> Result
instToResult = fromJust . flip lookup [(X, Loss), (Y, Draw), (Z, Win)]

score2 :: (Move, Result) -> Int
score2 (mOp, res) = mvPnts (findMove mOp res) + resPnts res

type Result2 = Int
f2 :: Input -> Result2
f2 = sum -- [4, 1, 7] -> 12
     . map (
         score2                  -- (Rock, Draw) -> 4
         . U.second instToResult -- (Rock, Y) -> (Rock, Draw)
       ) -- [(Rock, Y), (Paper, X), (Scissors, Z)] -> [4, 1, 7]

-- Main
main :: IO()
main = defMain rawToInput f1 f2
