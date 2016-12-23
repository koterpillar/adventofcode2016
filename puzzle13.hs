import Data.Bits
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Path
import Utils


data Grid = Grid
  { gSeed :: Int
  , gSteps :: [Position2]
  , gTarget :: Position2
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
      (\y ->
          (digits !! y) : ' ' : map (\x -> pixel (Position2 x y)) [0 .. gMaxX g])
      [0 .. gMaxY g]
    where
      pixel pos
        | elem pos (gSteps g) = 'O'
        | pos == gTarget g = 'X'
        | passable pos g = '.'
        | otherwise = '#'

gPlayer :: Grid -> Position2
gPlayer = head . gSteps

mkGrid :: Int -> Position2 -> Grid
mkGrid seed target = Grid seed [Position2 1 1] target

passable :: Position2 -> Grid -> Bool
passable (Position2 x y) grid
  | x < 0 || y < 0 = False
  | otherwise =
    let hash = x * x + 3 * x + 2 * x * y + y + y * y
        hash' = hash + gSeed grid
    in even $ popCount hash'

type Move = Direction4

moves :: Grid -> [Move]
moves = const $ [E, S, W, N]  -- lucky order

apply :: Move -> Grid -> Maybe Grid
apply m g =
  if passable newPosition2 g
    then Just $
         g
         { gSteps = newPosition2 : (gSteps g)
         }
    else Nothing
  where
    newPosition2 = walk m (gPlayer g)

gSuccess :: Grid -> Bool
gSuccess grid = gPlayer grid == gTarget grid

gMoveTree :: Grid -> Tree Grid Move
gMoveTree = moveTree moves apply

gLevels :: Tree Grid Move -> [[Grid]]
gLevels = levels gPlayer

demoGrid :: Grid
demoGrid = mkGrid 10 (Position2 7 4)
