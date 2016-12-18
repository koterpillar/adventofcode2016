{-# LANGUAGE TupleSections #-}
import Control.Applicative

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S


readLines :: IO [String]
readLines =
  getLine >>=
  \s ->
     case s of
       "" -> pure []
       _ -> fmap (s :) readLines

type Element = String

ewidth :: Int
ewidth = 2

data Item = Chip Element | Generator Element deriving (Eq)

instance Ord Item where
  compare = compare `on` itemOrdering
    where itemOrdering (Chip c) = (c, 2)
          itemOrdering (Generator c) = (c, 1)

instance Show Item where
  show (Chip c) = c ++ "M"
  show (Generator c) = c ++ "G"

type Floor = Int

floors :: [Floor]
floors = [0 .. 3]

floorLabels :: [String]
floorLabels = map (\i -> "F" ++ show (i + 1)) floors

data Facility = Facility
  { fElevator :: Floor
  , fFloors :: M.Map Item Floor
  } deriving (Ord, Eq)

instance Show Facility where
  show (Facility e fs) =
    unlines $
    reverse $
    map (intercalate " ") $
    transpose $
    floorLabels :
    map (uncurry floorDots) (("E" ++ blank', e) : M.toList (M.mapKeys show fs))
    where
      floorDots :: String -> Floor -> [String]
      floorDots s f = replicate f blank ++ [s] ++ replicate (3 - f) blank
      blank = "." ++ blank'
      blank' = replicate ewidth ' '

parse :: [String] -> Facility
parse = Facility 0 . foldr (uncurry parseFloor) M.empty . zip floors
  where
    parseFloor :: a -> String -> M.Map Item a -> M.Map Item a
    parseFloor f s old = foldr (parseItem f) old (tails $ splitOn " " $ filter (not . flip elem ".,") s)
    parseItem :: a -> [String] -> M.Map Item a -> M.Map Item a
    parseItem f (elt:"microchip":_) = M.insert (Chip (code elt)) f
    parseItem f (elt:"generator":_) = M.insert (Generator (code elt)) f
    parseItem _ _ = id
    code :: String -> Element
    code = map toUpper . take ewidth

floorItems :: Floor -> Facility -> [Item]
floorItems floor (Facility _ is) = [ item | (item, f) <- M.toList is, f == floor]

dangers :: Facility -> [String]
dangers f = concatMap groupDanger $ map (flip floorItems f) floors

isDangerous :: Facility -> Bool
isDangerous = not . null . dangers

groupDanger :: [Item] -> [String]
groupDanger is =
  let gens =
        [ elt
        | Generator elt <- is ]
      chips =
        [ elt
        | Chip elt <- is ]
      unpowered = filter (not . flip elem gens) chips
  in if (null gens)
       then []
       else map (\elt -> show (Chip elt) ++ " is unpowered") unpowered

data MoveDirection
  = Up
  | Down
  deriving (Ord, Eq, Show)

data Move = Move
  { mTarget :: Floor
  , mItems :: [Item]
  } deriving (Ord, Eq, Show)

moves :: Facility -> [Move]
moves f@(Facility curFloor _) = Move <$> targets <*> taken
  where
    items = floorItems curFloor f
    targets = filter (flip elem floors) [curFloor + 1, curFloor - 1]
    taken = filter (flip elem [1, 2] . length) (subsets items)

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = [id, (x:)] <*> subsets xs

applyUnsafe :: Move -> Facility -> Facility
applyUnsafe (Move target taken) = moveElevator target . flip (foldr (moveItem target)) taken
  where
    moveItem :: Floor -> Item -> Facility -> Facility
    moveItem target item (Facility e m) = Facility e $ M.insert item target m
    moveElevator :: Floor -> Facility -> Facility
    moveElevator target (Facility _ is) = Facility target is

apply :: Move -> Facility -> Maybe Facility
apply m = checkDanger . applyUnsafe m
  where
    checkDanger f
      | isDangerous f = Nothing
      | otherwise = Just f

fSuccess :: Facility -> Bool
fSuccess (Facility _ is) = all (== 3) $ M.elems is

facilityKey :: Facility -> (Floor, [(Floor, Floor)])
facilityKey (Facility e is) = (e, sort $ mapMaybe getTuple $ M.toList is)
  where
    getTuple (Chip elt, f1) = Just (f1, fromJust $ M.lookup (Generator elt) is)
    getTuple (Generator _, _) = Nothing

data Tree node edge = Tree
  { treeNode :: node
  , treeBranches :: [(edge, Tree node edge)]
  }

uniqBy :: Ord b => (a -> b) -> [a] -> [a]
uniqBy key = M.elems . M.fromList . map (\a -> (key a, a))

fromListWithKeyFunc :: Ord k => (a -> k) -> [a] -> M.Map k a
fromListWithKeyFunc f as =
  M.fromList
    [ (f a, a)
    | a <- as ]

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

fMoveTree :: Facility -> Tree Facility Move
fMoveTree = moveTree moves apply

levels :: (Ord pos, Ord key) => (pos -> key) -> Tree pos move -> [[pos]]
levels posKey start =
  map (map treeNode . fst) $ iterate (uncurry go) ([start], S.empty)
  where
    go roots seen = (nextRoots, seen')
      where
        nextRoots =
          uniqBy (posKey . treeNode) (concatMap (map snd . treeBranches) roots)
        seen' = S.union seen $ S.fromList $ map treeNode nextRoots

demoFacility :: Facility
demoFacility =
  parse
    [ "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip."
    , "The second floor contains a hydrogen generator."
    , "The third floor contains a lithium generator."
    , "The fourth floor contains nothing relevant."
    ]

readFacility :: IO Facility
readFacility = fmap parse readLines
