module Common
  ( defMain, testOn )
  where

import System.Environment (getArgs)
import System.IO (readFile)

defMain :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> IO()
defMain rawToInput f1 f2 = do
  args <- getArgs
  raw <- readFile $ head args
  let res1 = f1 $ rawToInput raw
  let res2 = f2 $ rawToInput raw
  print res1
  print res2

testOn :: (Show a, Show b) => String -> (String -> a) -> (a -> b) -> IO()
testOn path rawToInput f = do
  raw <- readFile path
  let input = rawToInput raw
  print input
  putStrLn " ->"
  print $ f input
