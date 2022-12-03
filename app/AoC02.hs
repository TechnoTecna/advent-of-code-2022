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
  map ( U.second (fromJust . flip lookup insts . head)
        . U.first (fromJust . flip lookup mvs . head)
        . splitAt 2 )
      . lines
  where mvs = [('A', Rock), ('B', Paper), ('C', Scissors)]
        insts = [('X', X), ('Y', Y), ('Z', Z)]

-- Part1
data Result = Win | Loss | Draw
  deriving (Eq, Show)

moveOrder :: [Move]
moveOrder = [Rock, Paper, Scissors]

result :: (Move, Move) -> Result
result (mOp, mPl) =
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
f1 = sum . map (score . U.second instToMove)

-- Part2
findMove :: Move -> Result -> Move
findMove mOp res =
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
f2 = sum . map (score2 . U.second instToResult)

-- Main
main :: IO()
main = defMain rawToInput f1 f2
