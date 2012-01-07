module Forest
  (leaf
  ,listToForest
  ,draw
  ,forestGetSubTree
  ,forestGetValue
  ,replace
  ,forestRemove
  ,forestInsert
  ,forestEditNode
  ,forestMap
  ,forestFilter
  ,forestSearch) where

import Graphics.UI.Gtk (TreePath)
import Data.Tree
import Data.List

-- type TreePath = [Int]

leaf a = Node a []

listToForest :: [a] -> Forest a
listToForest = map leaf

draw :: Forest String -> FilePath -> IO ()
draw fr str = writeFile (str ++ ".txt") (drawForest fr)

forestGetSubTree :: Forest a -> TreePath -> Tree a
forestGetSubTree fr [x] = fr !! x
forestGetSubTree fr (x:xs) = forestGetSubTree (subForest $ fr !! x) xs

forestGetValue :: Forest a -> TreePath -> a
forestGetValue fr x = rootLabel $ forestGetSubTree fr x

replace :: [a] -> [a] -> Int -> [a]
replace as' as x = (take x as) ++ as' ++ (drop (x + 1) as)

forestRemove :: Forest a -> TreePath -> Forest a
forestRemove fr   [x]  = replace [] fr x
forestRemove fr (x:xs) = let Node a sf = fr !! x
                          in replace [Node a (forestRemove sf xs)] fr x

forestInsert :: Tree a -> Forest a -> TreePath -> Forest a
forestInsert k fr   []   = fr ++ [k]
forestInsert k fr (x:xs) = let Node a sf = fr !! x
                            in replace [Node a (forestInsert k sf xs)] fr x

forestEditNode :: a -> Forest a -> TreePath -> Forest a
forestEditNode rl fr  [x]   = let Node _ sf = fr !! x
                               in replace [Node rl sf] fr x
forestEditNode rl fr (x:xs) = let Node a sf = fr !! x
                               in replace [Node a (forestEditNode rl sf xs)] fr x

forestMap :: (a -> b) -> Forest a -> Forest b
forestMap _ [] = []
forestMap f fr = map (propogate f) fr
  where propogate f (Node a sf) = Node (f a) (forestMap f sf)

forestFilter :: (a -> Bool) -> Forest a -> Forest a
forestFilter _ [] = []
forestFilter f fr = map (propogate f) $ filter (f.rootLabel) fr
  where propogate f (Node a sf) = Node a (forestFilter f sf)

forestSearch :: (a -> Bool) -> Forest a -> [a]
forestSearch _ [] = []
forestSearch f fr = concat $ map (propogate f) fr
  where propogate f (Node a sf) = if f a
                                   then a:(forestSearch f sf)
                                   else forestSearch f sf
 
tf = [Node "a" [Node "b" []
               ,Node "c" [Node "h" [leaf "i"
                                   ,leaf "j"]]
               ,leaf "d"]
     ,Node "e" [leaf "f"
               ,leaf "g"]]