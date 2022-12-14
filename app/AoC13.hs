module Main where

import Common
import Data.List (findIndex, findIndices, sortBy)
import Data.Maybe (fromJust)
import Utils as U

-- Input parsing
data ListM = L ListM ListM | E | S Int
  deriving (Show, Eq)

type Input = [(ListM, ListM)]

headM :: ListM -> ListM
headM (L h _) = h

findClose :: Int -> [String] -> Int -> Int
findClose n ("]" : _) 0 = n
findClose n ("]" : t) r = findClose (n + 1) t (r - 1)
findClose n ("[" : t) r = findClose (n + 1) t (r + 1)
findClose n (_ : t) r = findClose (n + 1) t r

shitParse' :: [String] -> ListM
shitParse' ("[" : t) =
  L (shitParse' (take indexC t)) $ shitParse' (drop indexC t)
  where
    indexC = findClose 1 t 0
shitParse' ["]"] = E
shitParse' [] = E
shitParse' (i : t) = L (S $ read i) $ shitParse' t

shitParse :: [String] -> ListM
shitParse = headM . shitParse'

shitLex :: String -> [String]
shitLex =
  filter (/= "")
    . concatMap
      ( \l ->
          takeWhile (/= ']') l : (map (: []) . dropWhile (/= ']') $ l)
      )
    . concatMap
      ( \l ->
          (map (: []) . takeWhile (== '[') $ l) ++ [dropWhile (== '[') l]
      )
    . concatMap (\l -> if head l == '[' then ["[", tail l] else [l])
    . U.splitWhen (== ',')

rawToInput :: String -> Input
rawToInput =
  map (U.both (shitParse . shitLex) . (\[l1, l2] -> (l1, l2)))
    . U.splitWhen (== "")
    . lines

-- Part1
type Result1 = Int

compareM :: ListM -> ListM -> Ordering
compareM lft rgt =
  case (lft, rgt) of
    (S i1, S i2) -> compare i1 i2
    (E, E) -> EQ
    (E, _) -> LT
    (_, E) -> GT
    (lft, S i) -> compareM lft (L (S i) E)
    (S i, rgt) -> compareM (L (S i) E) rgt
    (L lft' lt, L rgt' rt) ->
      case compareM lft' rgt' of
        EQ -> compareM lt rt
        LT -> LT
        GT -> GT

f1 :: Input -> Result1
f1 = sum . map (+ 1) . findIndices (/= GT) . map (uncurry compareM)

-- Part2
type Result2 = Int

f2 :: Input -> Result2
f2 =
  ( \l ->
      (fromJust (findIndex (== L (L (S 2) E) E) l) + 1)
        * (fromJust (findIndex (== L (L (S 6) E) E) l) + 1)
  )
    . sortBy compareM
    . (++) [L (L (S 2) E) E, L (L (S 6) E) E]
    . concatMap (\(a, b) -> [a, b])

-- Main
main :: IO ()
main = defMain rawToInput f1 f2
