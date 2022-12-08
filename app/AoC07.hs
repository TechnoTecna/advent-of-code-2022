module Main where

import Common
import Data.List (findIndex, sort, find)
import Data.Maybe (fromJust)
import Utils as U


-- Input parsing
data Entry = File Int String | Dir String
  deriving (Eq, Show)
-- For Cd the string is the argument. For Ls The list of entries is the result
data Command = Cd String | Ls [Entry]
  deriving (Eq, Show)

type Input = [Command]

parseEntry :: String -> Entry
parseEntry =
  (\(a, b) ->
     if a == "dir"
     then Dir $ tail b
     else File (read a) (tail b)
  )
  . break (' ' ==)

rawToInput :: String -> Input
rawToInput =
  map (
    ( \cmd ->
        if head (head cmd) == 'c'
        then Cd $ drop 3 $ head cmd
        else Ls $ map parseEntry $ tail cmd
    )
    . lines
    . tail
  )
  . U.splitWhen ('$' ==)

-- Part1
type Result1 = Int

data Tree = D String [Tree] | F Int String
  deriving (Eq, Show)

buildTree :: Tree -> Input -> (Tree, Input)
buildTree tree [] = (tree, [])
buildTree tree (Cd ".." : t) = (tree, t)
buildTree (D nd fs) (Cd dir : t) =
  buildTree (D nd $ U.setList indx (const newDir) fs) t'
  where (newDir, t') = buildTree (fs !! indx) t
        indx = fromJust $ findIndex
                            (\fl ->
                               case fl of
                                 D nd fl' -> nd == dir
                                 _        -> False)
                            fs
buildTree (D nd []) (Ls fs : t) =
  buildTree
    ( D nd
      $ map (\ fl ->
          case fl of
            File sz nm -> F sz nm
            Dir nm -> D nm []
        )
          fs
    )
    t

data SizeTree = SD Int [SizeTree] | SF Int

size :: Tree -> SizeTree
size (F s _) = SF s
size (D _ lt) = SD (foldr f 0 slt) slt
  where slt = map size lt
        f (SF s) acc = s + acc
        f (SD s _) acc = s + acc

sizeDirList :: SizeTree -> [Int]
sizeDirList (SD s lt) = s : concatMap sizeDirList lt
sizeDirList _ = []

f1 :: Input -> Result1
f1 =
  sum
  . filter (100000 >=)
  . sizeDirList
  . size
  . fst
  . buildTree (D "/" [])
  . tail

-- Part2
type Result2 = Int

f2 :: Input -> Result2
f2 input = fromJust $ find (minDel <=) $ sort $ sizeDirList sizeT
  where minDel = 30000000 - (70000000 - used)
        used = case sizeT of SD s _ -> s
        sizeT = size $ fst $ buildTree (D "/" []) $ tail input

-- Main
main :: IO()
main = defMain rawToInput f1 f2
