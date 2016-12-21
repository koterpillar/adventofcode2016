import Data.Bits
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Path


data Direction
  = E
  | N
  | W
  | S
  deriving (Eq, Ord, Show)

data Pos = Pos
  { pX :: Int
  , pY :: Int
  } deriving (Eq, Ord, Show)

data Grid = Grid
  { gSeed :: Int
  , gSteps :: [Pos]
  , gTarget :: Pos
  } deriving (Eq, Ord)

gMaxY :: Grid -> Int
gMaxY g = pY (gTarget g) + 2

gMaxX :: Grid -> Int
gMaxX g = pX (gTarget g) + 2

digits :: [Char]
digits = cycle ['0'..'9']

instance Show Grid where
  show g =
    unlines $
    (' ' : ' ' : take (gMaxX g + 1) digits) :
    map
      (\y -> (digits !! y) : ' ' : map (\x -> pixel (Pos x y)) [0 .. gMaxX g])
      [0 .. gMaxY g]
    where
      pixel pos
        | elem pos (gSteps g) = 'O'
        | pos == gTarget g = 'X'
        | passable pos g = '.'
        | otherwise = '#'

gPlayer :: Grid -> Pos
gPlayer = head . gSteps

mkGrid :: Int -> Pos -> Grid
mkGrid seed target = Grid seed [Pos 1 1] target

passable :: Pos -> Grid -> Bool
passable (Pos x y) grid
  | x < 0 || y < 0 = False
  | otherwise =
    let hash = x * x + 3 * x + 2 * x * y + y + y * y
        hash' = hash + gSeed grid
    in even $ popCount hash'

walk :: Direction -> Pos -> Pos
walk E (Pos x y) = Pos (x + 1) y
walk W (Pos x y) = Pos (x - 1) y
walk N (Pos x y) = Pos x (y - 1)
walk S (Pos x y) = Pos x (y + 1)

type Move = Direction

moves :: Grid -> [Move]
moves = const $ [E, S, W, N]  -- lucky order

apply :: Move -> Grid -> Maybe Grid
apply m g =
  if passable newPos g
    then Just $
         g
         { gSteps = newPos : (gSteps g)
         }
    else Nothing
  where
    newPos = walk m (gPlayer g)

gSuccess :: Grid -> Bool
gSuccess grid = gPlayer grid == gTarget grid

gMoveTree :: Grid -> Tree Grid Move
gMoveTree = moveTree moves apply

gLevels :: Tree Grid Move -> [[Grid]]
gLevels = levels gPlayer

demoGrid :: Grid
demoGrid = mkGrid 10 (Pos 7 4)
