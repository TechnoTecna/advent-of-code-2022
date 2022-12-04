module Common
  ( defMain, testOn )
  where

import System.Environment (getArgs)
import System.IO (readFile)

defMain :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> IO()
defMain rawToInput f1 f2 = do
  raw <- readFile . head =<< getArgs
  print <$> f1 $ rawToInput raw
  print <$> f2 $ rawToInput raw

testOn :: (Show a, Show b) => String -> (String -> a) -> (a -> b) -> IO()
testOn path rawToInput f = do
  raw <- readFile path
  let input = rawToInput raw
  print input
  putStrLn " ->"
  print $ f input
