module Main where

import Common
import Data.List (transpose)
import Utils as U

-- Input parsing
type Input = ([[Char]], [(Int, Int, Int)])

-- ["    [D]"
-- ,"[N] [C]"
-- ,"[Z] [M] [P]"]
-- ->
-- [['N', 'Z'], ['D', 'C', 'M'], ['P'], [], [], [], [], [], []]
parseStacks :: [String] -> [[Char]]
parseStacks =
  -- last we pad our list of stack with empty ones to make sure we always have 9
  -- of them.
  (\l -> l ++ replicate (9 - length l) [])
    -- ditch now useless spaces (note that this must be done AFTER transposing)
    . map (filter (/= ' '))
    -- turn our list of lines into a list of collums
    -- [" D"      [" NZ"
    -- ,"NC"   -> ,"DCM"
    -- ,"ZMP"]    ,"P"]
    . transpose
    -- for each line, isolate the letters representing the crates. if there are
    -- no crates it grabs a space ("    [D]" -> " D").
    . map
      ( map (!! 1)
          . U.chunksOf 4
      )

-- ["m 1 f 2 t 1", "m 3 f 1 t 3", "m 2 f 2 t 1"] -> [(1,2,1), (3,1,3), (2,2,1)]
parseInsts :: [String] -> [(Int, Int, Int)]
parseInsts =
  map
    ( ( \dgts ->
          ( read $ reverse $ drop 2 dgts,
            read [dgts !! 1],
            read [dgts !! 0]
          )
      ) -- "122" -> (2, 2, 1)
      -- here we reverse because since there are max 9 stacks, we now they are
      -- represented by one digit. thus we reverse, grabe the stacks numbers
      -- with !!0 and !!1, drop them, re-reverse and read the arbitrarly long
      -- number of crates.
        . reverse -- "221" -> "122"
        . filter (`elem` ['0' .. '9']) -- "m 2 f 2 t 1" -> "221"
    )

rawToInput :: String -> Input
rawToInput =
  -- drop the stacks labels and the empty line at the begining and parse the
  -- instructions. for example:
  -- [" 1   2   3", "", "m 1 f 2 t 1", "m 3 f 1 t 3"] -> [(1,2,1), (3,1,3)]
  U.second (parseInsts . drop 2)
    -- parse the stacks (for the example input it gives us :
    -- ["NZ","DCM","P","","","","","",""])
    . U.first parseStacks
    -- separate our lines at the first containing the digit '1'. Gives us (a, b)
    -- where a is the list of lines containing the visual represention of the
    -- stacks and b contains the label of the stacks and the move instructions
    . break (elem '1')
    -- gives us a list of lines ("first\nsecond" -> ["first", "second"])
    . lines

-- Part1
type Result1 = [Char]

-- execute one instruction on our current stacks
oneMove :: [[Char]] -> (Int, Int, Int) -> [[Char]]
oneMove stk (nb, frm, to) =
  -- We reverse the list of the crates we took to simulate moving them one by
  -- one
  U.setList (to - 1) (reverse took ++) $ -- ["","DCM","P"] -> ["","DCM","ZNDP"]
    U.setList (frm - 1) (drop nb) stk -- ["DNZ", "CM", "P"] -> ["", "DCM", "P"]
  where
    took = take nb (stk !! (frm - 1)) -- crates taken from source stacks

f1 :: Input -> Result1
f1 (stk, mvs) =
  -- get the top crates of each stacks (["C", "M", "ZNDP"] -> ['C', 'M', 'Z'])
  map head
  -- remove empty stacks
  $
    filter (not . null)
    -- succesivly executes all instruction on our stacks
    $
      foldl oneMove stk mvs

-- Part2
type Result2 = [Char]

-- Just like oneMove but we do not reverse taken crates before dropping them
oneMove' :: [[Char]] -> (Int, Int, Int) -> [[Char]]
oneMove' stk (nb, frm, to) =
  U.setList (to - 1) (took ++) $
    U.setList (frm - 1) (drop nb) stk
  where
    took = take nb (stk !! (frm - 1))

-- Just like f1 but with oneMove' instead of oneMove
f2 :: Input -> Result2
f2 (stk, mvs) =
  map head $
    filter (not . null) $
      foldl oneMove' stk mvs

-- Main
main :: IO ()
main = defMain rawToInput f1 f2
