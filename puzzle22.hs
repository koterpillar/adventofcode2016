import Control.Monad

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M

import Path
import Utils

data Node = Node
  { nSize :: Int
  , nUsed :: Int
  , nGoalData :: Bool
  } deriving (Eq, Ord)

instance Show Node where
  show n | nGoalData n = "G"
         | nUsed n > 100 = "#"
         | nUsed n == 0 = "_"
         | otherwise = "."

nAvail :: Node -> Int
nAvail n = nSize n - nUsed n

parseNode :: String -> (Position2, Node)
parseNode str =
  let [namePart, sizePart, usedPart, _, _] = filter (not . null) $ splitOn " " str
      [_, 'x':xPart, 'y':yPart] = splitOn "-" namePart
      takeDigits = takeWhile isDigit
      sizeStr = takeDigits sizePart
      usedStr = takeDigits usedPart
  in (Position2 (read xPart) (read yPart), Node (read sizeStr) (read usedStr) False)

data Grid = Grid
  { gNodes :: (M.Map Position2 Node)
  , gSize :: Position2
  } deriving (Eq, Ord)

mkGrid :: M.Map Position2 Node -> Grid
mkGrid nodes = Grid (markGoal nodes) (Position2 xmax ymax)
  where
    positions = M.keys nodes
    xmax = maximum $ map pX positions
    ymax = maximum $ map pY positions
    markGoal =
      M.adjust
        (\n ->
            n
            { nGoalData = True
            })
        (Position2 xmax 0)

parse :: [String] -> Grid
parse = mkGrid . M.fromList . map parseNode . dropWhile (not . isInfixOf "/")

viablePair :: (Position2, Node) -> (Position2, Node) -> Bool
viablePair (pa, a) (pb, b) = pa /= pb && nUsed a > 0 && nUsed a <= nAvail b

instance Show Grid where
  show (Grid nodes (Position2 xmax ymax)) = unlines $ map showLine [0 .. ymax]
    where
      showLine y = pad 3 (show y) ++ " " ++ intercalate " " (map (showNodeAt y) [0 .. xmax])
      showNodeAt y x =
        let (Just node) = M.lookup (Position2 x y) nodes
        in show node

demo :: Grid
demo =
  parse
    [ "Filesystem            Size  Used  Avail  Use%"
    , "/dev/grid/node-x0-y0   10T    8T     2T   80%"
    , "/dev/grid/node-x0-y1   11T    6T     5T   54%"
    , "/dev/grid/node-x0-y2   32T   28T     4T   87%"
    , "/dev/grid/node-x1-y0    9T    7T     2T   77%"
    , "/dev/grid/node-x1-y1    8T    0T     8T    0%"
    , "/dev/grid/node-x1-y2   11T    7T     4T   63%"
    , "/dev/grid/node-x2-y0   10T    6T     4T   60%"
    , "/dev/grid/node-x2-y1    9T    8T     1T   88%"
    , "/dev/grid/node-x2-y2    9T    6T     3T   66%"
    ]

data Move =
  Move Position2
       Direction4
  deriving (Show)

moves :: Grid -> [Move]
moves (Grid _ (Position2 sizeX sizeY)) =
  filter destWithin $
  [ Move (Position2 x y) dir
  | dir <- [N, E, W, S]
  , x <- [0 .. sizeX]
  , y <- [0 .. sizeY] ]
  where
    destWithin :: Move -> Bool
    destWithin (Move pos dir) =
      let (Position2 x y) = walk dir pos
      in x >= 0 && y >= 0 && x <= sizeX && y <= sizeY

apply :: Move -> Grid -> Maybe Grid
apply (Move pos dir) (Grid nodes size) = do
  src <- M.lookup pos nodes
  let pos' = walk dir pos
  dest <- M.lookup pos' nodes
  guard (nAvail dest >= nUsed src)
  guard (nUsed src > 0)
  let dest' =
        dest
        { nUsed = nUsed dest + nUsed src
        , nGoalData = nGoalData dest || nGoalData src
        }
  let src' =
        src
        { nUsed = 0
        , nGoalData = False
        }
  let nodes' = M.insert pos src' $ M.insert pos' dest' $ nodes
  pure $ Grid nodes' size

gMoveTree :: Grid -> Tree Grid Move
gMoveTree = moveTree moves apply

gLevels :: Tree Grid Move -> [[Grid]]
gLevels = levels show

gSuccess :: Grid -> Bool
gSuccess g =
  let (Just accessible) = M.lookup (Position2 0 0) (gNodes g)
  in nGoalData accessible

gShortestPath :: [[Grid]] -> [[Grid]]
gShortestPath = shortestPath gSuccess

gFindNode :: (Node -> Bool) -> Grid -> Position2
gFindNode test = fst . head . M.toList . M.filter test . gNodes

gBlank :: Grid -> Position2
gBlank = gFindNode $ \n -> nUsed n == 0

gGoal :: Grid -> Position2
gGoal = gFindNode nGoalData

gBlankGoalDistance :: Grid -> Int
gBlankGoalDistance g = manhattanDistance (gGoal g) (gBlank g)

apply' :: Move -> Grid -> Maybe Grid
apply' m@(Move pos dir) grid = do
  let (Position2 x y) = walk dir pos
  -- Heuristic: don't move the empty cell around in uninteresting rows
  guard $ not $ elem dir [W, E] && elem y [2 .. 25]
  guard $ not $ elem dir [N, S] && y == 26 && x /= 8
  grid' <- apply m grid
  -- Heuristic: if the blank is near the goal, don't move it away
  when (gBlankGoalDistance grid <= 2) $ guard (gBlankGoalDistance grid' <= 2)
  pure grid'

gMoveTree' :: Grid -> Tree Grid Move
gMoveTree' = moveTree moves apply'
