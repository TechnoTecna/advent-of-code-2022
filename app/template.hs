module Main where

import Common (defMain)


type Result1 = ()
type Result2 = ()
type Input = ()

-- Input parsing
rawToInput :: String -> Input
rawToInput = undefined

-- Part1
f1 :: Input -> Result1
f1 = undefined

-- Part2
f2 :: Input -> Result2
f2 = undefined

-- Main
main :: IO()
main = defMain rawToInput f1 f2
