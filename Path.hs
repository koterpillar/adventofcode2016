{-# LANGUAGE TupleSections #-}
module Path where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S


data Tree node edge = Tree
  { treeNode :: node
  , treeBranches :: [(edge, Tree node edge)]
  }


moveTree :: (pos -> [move]) -> (move -> pos -> Maybe pos) -> pos -> Tree pos move
moveTree generate apply start = mtc start
  where
    mtc start = Tree start children
      where
        childNodes =
          catMaybes
            [ (move, ) <$> apply move start
            | move <- generate start ]
        children = map go childNodes
        go (move, childNode) = (move, mtc childNode)

uniqBy :: Ord b => (a -> b) -> [a] -> [a]
uniqBy key = M.elems . M.fromList . map (\a -> (key a, a))

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateWhile continue fn v
  | continue v =
    let v' = fn v
    in v : iterateWhile continue fn v'
  | otherwise = []

levels :: (Ord pos, Ord key) => (pos -> key) -> Tree pos move -> [[pos]]
levels posKey start =
  map (map treeNode . fst) $
  iterateWhile (not . null . fst) (uncurry go) ([start], S.empty)
  where
    go roots seen = (nextRoots, seen')
      where
        nextRoots =
          uniqBy (posKey . treeNode) (concatMap (map snd . treeBranches) roots)
        seen' = S.union seen $ S.fromList $ map treeNode nextRoots

shortestPath :: (pos -> Bool) -> [[pos]] -> [[pos]]
shortestPath success (thisLevel:rest)
  | any success thisLevel = [filter success thisLevel]
  | otherwise = [] : shortestPath success rest
