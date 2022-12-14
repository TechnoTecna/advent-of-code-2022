module Main where

import Common
import Data.List (sort)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.IO (readFile)
import Utils as U

-- Input parsing
data Monkey = Monkey
  { items :: [Int],
    operation :: Int -> Int,
    test :: (Int, Int, Int)
  }

type Input = [Monkey]

parseOp :: String -> (Int -> Int)
parseOp str =
  case (a, b) of
    ("old", "old") -> (\n -> op n n)
    ("old", i) -> (\n -> op n (read i))
    (i, "old") -> (\n -> op (read i) n)
  where
    op = fromJust $ lookup sop [("+", (+)), ("-", (-)), ("*", (*))]
    [a, sop, b] = words str

parseMonkey :: [String] -> Monkey
parseMonkey lns = Monkey items operation test
  where
    operation = parseOp . drop 2 . dropWhile (/= '=') $ lns !! 2
    test =
      (\[c, t, f] -> (c, t, f))
        . map (read . drop 2 . dropWhile (/= 'y'))
        $ drop 3 lns
    items =
      map (read . tail)
        . U.splitWhen (== ',')
        . dropWhile (/= ':')
        $ lns !! 1

rawToInput :: String -> Input
rawToInput = map parseMonkey . U.splitWhen null . lines

-- Part1
type Result1 = Int

runMonkey1 :: Monkey -> [(Int, Int)]
runMonkey1 (Monkey itms op (c, t, f)) =
  map (\i -> (if newLvl i `mod` c == 0 then t else f, newLvl i)) itms
  where
    newLvl = (`div` 3) . op

giveItem :: Monkey -> Int -> Monkey
giveItem mnk i = mnk {items = items mnk ++ [i]}

distribute :: [(Int, Int)] -> [(Monkey, Int)] -> [(Monkey, Int)]
distribute throws mnks =
  foldl
    ( \mnks (mnki, i) ->
        U.setList mnki (U.first (`giveItem` i)) mnks
    )
    mnks
    throws

runRound :: [(Monkey, Int)] -> [(Monkey, Int)]
runRound mnks =
  foldl
    ( \mnks i ->
        U.setList i (\(mk, nb) -> (mk {items = []}, nb + length (items mk))) $
          distribute (runMonkey1 (fst $ mnks !! i)) mnks
    )
    mnks
    [0 .. length mnks - 1]

f1 :: Input -> Result1
f1 mnks =
  product
    . take 2
    . reverse
    . sort
    . map snd
    $ foldl
      (\mnks _ -> runRound mnks)
      (map (\m -> (m, 0)) mnks)
      [1 .. 20]

-- Part2
type Result2 = Int

data Monkey2 = Monkey2
  { index2 :: Int,
    items2 :: [[Int]],
    operation2 :: Int -> Int,
    test2 :: (Int, Int, Int)
  }

type Input2 = [Monkey2]

parseMonkey2 :: [String] -> Monkey2
parseMonkey2 lns = Monkey2 index items operation test
  where
    operation = parseOp . drop 2 . dropWhile (/= '=') $ lns !! 2
    test =
      (\[c, t, f] -> (c, t, f))
        . map (read . drop 2 . dropWhile (/= 'y'))
        $ drop 3 lns
    items =
      map ((: []) . read . tail)
        . U.splitWhen (== ',')
        . dropWhile (/= ':')
        $ lns !! 1
    index = read . takeWhile (/= ':') . drop 7 . head $ lns

rawToInput2 :: String -> Input2
rawToInput2 str =
  map
    ( \mnk ->
        mnk {items2 = map (\(i : _) -> map (i `mod`) mods) (items2 mnk)}
    )
    mnks'
  where
    mods = map (U.fst3 . test2) mnks'
    mnks' = map parseMonkey2 . U.splitWhen null . lines $ str

runMonkey2 :: Monkey2 -> [Int] -> [(Int, [Int])]
runMonkey2 (Monkey2 id itms op (c, t, f)) mods =
  map
    ( \is ->
        ( if newLvl (is !! id) `mod` c == 0 then t else f,
          zipWith mod (map newLvl is) mods
        )
    )
    itms
  where
    newLvl = op

giveItem2 :: Monkey2 -> [Int] -> Monkey2
giveItem2 mnk is = mnk {items2 = items2 mnk ++ [is]}

distribute2 :: [(Int, [Int])] -> [(Monkey2, Int)] -> [(Monkey2, Int)]
distribute2 throws mnks =
  foldl
    ( \mnks (mnki, is) ->
        U.setList mnki (U.first (`giveItem2` is)) mnks
    )
    mnks
    throws

runRound2 :: [(Monkey2, Int)] -> [(Monkey2, Int)]
runRound2 mnks =
  foldl
    ( \mnks i ->
        U.setList i (\(mk, nb) -> (mk {items2 = []}, nb + length (items2 mk))) $
          distribute2
            (runMonkey2 (fst $ mnks !! i) $ map (U.fst3 . test2 . fst) mnks)
            mnks
    )
    mnks
    [0 .. length mnks - 1]

f2 :: Input2 -> Int
f2 mnks =
  product
    . take 2
    . reverse
    . sort
    . map snd
    $ foldl
      (\mnks _ -> runRound2 mnks)
      (map (\m -> (m, 0)) mnks)
      [1 .. 10000]

-- Main
main :: IO ()
main = do
  raw <- readFile . head =<< getArgs
  print <$> f1 $ rawToInput raw
  print <$> f2 $ rawToInput2 raw
