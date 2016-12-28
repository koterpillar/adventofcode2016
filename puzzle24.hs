import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S

import Path
import Utils

type TargetIndex = Int

data Cell
  = Open
  | Wall
  | Target Int
  deriving (Ord, Eq, Show)

data Map = Map { mCells :: M.Map Position2 Cell
               , mPosition :: Position2
               , mVisited :: S.Set TargetIndex
               , mAllTargets :: S.Set TargetIndex
               , mSize :: Position2 } deriving (Eq, Ord)

mInside :: Map -> Position2 -> Bool
mInside m pos =
  pX pos >= 0 && pY pos >= 0 && pX pos <= pX maxPos && pY pos <= pY maxPos
  where
    maxPos = mSize m

showDigit :: TargetIndex -> Char
showDigit = head . show

instance Show Map where
  show m = unlines $ visitedLine : map showLine [0 .. pY (mSize m)]
    where
      showLine y = map (showCell y) [0 .. pX (mSize m)]
      showCell y x =
        let (Just cell) = M.lookup (Position2 x y) (mCells m)
        in case cell of
             Open -> '.'
             Wall -> '#'
             Target i -> showDigit i
      visitedLine = map showDigit $ S.toList $ mVisited m

demo :: Map
demo =
  parseMap
    ["###########", "#0.1.....2#", "#.#######.#", "#4.......3#", "###########"]

mkMap :: M.Map Position2 Cell -> Map
mkMap cells =
  Map
    cells
    startingPoint
    S.empty
    allTargets
    (Position2 (maximum $ map pX indices) (maximum $ map pY indices))
  where
    indices = M.keys cells
    [startingPoint] = M.keys $ M.filter (== Target 0) cells
    allTargets =
      S.fromList
        [ i
        | Target i <- M.elems cells ]

parseMap :: [String] -> Map
parseMap = mkMap . foldr (uncurry parseMapLine) M.empty . zip [0 ..]
  where
    parseMapLine :: Int -> String -> M.Map Position2 Cell -> M.Map Position2 Cell
    parseMapLine y line cells =
      foldr (uncurry $ parseCell y) cells $ zip [0 ..] line
    parseCell :: Int -> Int -> Char -> M.Map Position2 Cell -> M.Map Position2 Cell
    parseCell y x cell = M.insert (Position2 x y) $ readCell cell
    readCell '.' = Open
    readCell '#' = Wall
    readCell i = Target $ read [i]

type Move = Direction4

moves :: Map -> [Move]
moves = const [N, E, W, S]

apply :: Move -> Map -> Maybe Map
apply move map = do
  let pos' = walk move (mPosition map)
  guard (mInside map pos')
  cell <- M.lookup pos' (mCells map)
  guard (cell /= Wall)
  let visited = mVisited map
  let visited' = case cell of Open -> visited
                              Target i -> S.insert i visited
  pure $ map { mPosition = pos', mVisited = visited' }

mMoveTree :: Map -> Tree Map Move
mMoveTree = moveTree moves apply

mLevels :: Tree Map Move -> [[Map]]
mLevels = levels id

mSuccess :: Map -> Bool
mSuccess map = mAllTargets map == mVisited map

mShortestPath :: [[Map]] -> [[Map]]
mShortestPath = shortestPath mSuccess
