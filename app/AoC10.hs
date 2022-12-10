module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Common
import Data.List (intercalate)
import Utils as U


-- Input parsing
data Inst = Noop | AddX Int
  deriving (Eq, Show)

type Inst1C = Inst

type Input = [Inst]

-- Parse one operation
-- "addx 1" -> AddX 1
parseOp :: String -> Inst
parseOp "noop" = Noop
parseOp op =
  AddX                 -- 3 -> AddX 3
  . read               -- "3" -> 3
  . tail               -- " 3" -> "3"
  . dropWhile (/= ' ') -- "addx 3" -> " 3"
  $ op                 -- "addx 3"

rawToInput :: String -> Input
rawToInput =
  map parseOp -- ["addx 1", "addx 2", "noop"] -> [AddX 1, AddX 2, Noop]
  . lines     -- "addx 1\naddx 2\nnoop" -> ["addx 1", "addx 2", "noop"]

-- Part1
type Result1 = Int

-- takes an instruction and turn it into a series of instruction that take one
-- cycle. (Note that we decided use the same type for normal and one-cycle
-- cycle. Let's hope this doesn't come back later to haunt us).
toOneCycle :: Inst -> [Inst1C]
toOneCycle Noop = [Noop]
toOneCycle i = [Noop, i]

-- given X and an instrucion give the new value of X after executing the
-- instruction.
-- exe 10 (AddX 3) -> 13
-- exe 1 Noop -> 1
exe :: Int -> Inst1C -> Int
exe x Noop = x
exe x (AddX i) = x + i

-- given a starting value and a list of one-cycle instructions give us the trace
-- of X through the execution of those instructions.
-- traceX 1 [Noop, AddX 15, Noop, AssX -11] -> [1,1,16,16,5]
traceX :: Int -> [Inst1C] -> [Int]
traceX = scanl exe

f1 :: Input -> Result1
f1 =
  sum                    -- sum everything
                         -- multiply all value by there cycle numbers
  . zipWith (*) [20 + 40*k | k <- [0 ..]]
  . map head             -- [[21,..],[19,..]..] -> [21,19, ...]
  . U.chunksOf 40        -- list in chuncks of length 40 ([[21,..],[19,..]..])
  . drop 19              -- discard the 19 first elements
  . traceX 1             -- [Noop,AddX 15,Noop,AddX -11] -> [1,16,16,5]
  . concatMap toOneCycle -- [AddX 15,AddX -11] -> [Noop,AddX 15,Noop,AddX -11]

-- Part2
type Result2 = String

-- given the postion of the drawn pixel and the position of the middle of the
-- sprite, determine if we should draw the sprite or not.
-- 20 21 -> True
-- 20 22 -> False
touchSprite :: Int -> Int -> Bool
touchSprite crt spt = (<= 1) . abs $ crt - spt

-- For some reason traceX seems to generate one value to many. I don't know why
-- and since it doesn't break the display I don't really care. Maybe since each
-- instruction generate a value for the next cycle the last instruction run on
-- the last cycle should not generate a value.
f2 :: Input -> Result2
f2 =
  intercalate "\n"       -- display lines stacked vertically (draw the screen)
  . chunksOf 40          -- cut the one line into lines of length 40
                         -- draw the image on one line
  . map (\x -> if x then '#' else '.')
                         -- for every cyle tell you if you should draw or not
  . zipWith touchSprite [n `mod` 40 | n <- [0 ..]]
  . traceX 1             -- [Noop,AddX 15,Noop,AddX -11] -> [1,16,16,5]
  . concatMap toOneCycle -- [AddX 15,AddX -11] -> [Noop,AddX 15,Noop,AddX -11]

-- Main
main :: IO()
main = do
  raw <- readFile . head =<< getArgs
  print <$> f1 $ rawToInput raw
  putStrLn <$> f2 $ rawToInput raw -- modified this line to display the screen
