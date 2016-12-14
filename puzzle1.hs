import Control.Monad.Writer

import Data.List.Split


data Direction
  = E
  | S
  | W
  | N
  deriving (Ord, Eq, Enum, Show)

data Turn
  = L
  | R
  deriving (Ord, Eq, Show)

readTurn :: Char -> Turn
readTurn 'R' = R
readTurn 'L' = L

type Instruction = (Turn, Int)

turns :: String -> [Instruction]
turns = map mkTurn . splitOn ", "
  where
    mkTurn (turn:walk) = (readTurn turn, read walk)

data Coordinate = Coo
  { cd :: Direction
  , cx :: Int
  , cy :: Int
  }
  deriving (Eq, Ord)

instance Show Coordinate where
  show (Coo d x y) = "Coo " ++ show d ++ " " ++ show x ++ " " ++ show y

compareCoord :: Coordinate -> Coordinate -> Bool
compareCoord (Coo _ x y) (Coo _ x' y') = (x == x') && (y == y')

follow :: Coordinate -> [Instruction] -> Coordinate
follow = foldl (flip go)

start :: Coordinate
start = Coo N 0 0

distanceTo :: Coordinate -> Int
distanceTo finish = abs (cx finish - cx start) + abs (cy finish - cy start)

puzzle :: String -> Int
puzzle = distanceTo . follow start . turns

go :: Instruction -> Coordinate -> Coordinate
go (turn, walk) = goWalk walk . goTurn turn

goTurn :: Turn -> Coordinate -> Coordinate
goTurn turn (Coo d x y) = Coo (doTurn turn d) x y

doTurn :: Turn -> Direction -> Direction
doTurn R N = E
doTurn R d = succ d
doTurn L E = N
doTurn L d = pred d

goWalk :: Int -> Coordinate -> Coordinate
goWalk d (Coo N x y) = Coo N (x + d) y
goWalk d (Coo S x y) = Coo S (x - d) y
goWalk d (Coo E x y) = Coo E x (y + d)
goWalk d (Coo W x y) = Coo W x (y - d)

puzzle2 :: String -> Int
puzzle2 = distanceTo . firstTwice compareCoord . execWriter . trace start . turns

trace :: Coordinate -> [Instruction] -> Writer [Coordinate] ()
trace loc [] = tell [loc]
trace loc ((turn, walk):is) = do
  let loc' = goTurn turn loc
  forM [0 .. walk - 1] $ \d -> tell [goWalk d loc']
  trace (goWalk walk loc') is

interpolate :: [Coordinate] -> [Coordinate]
interpolate coos = concat $ zipWith interp coos (tail coos)
  where
    interp c c'
      | compareCoord c c' = [c]
      | cx c == cx c' = c:interp (goWalk 1 c) c'

firstTwice :: (a -> a -> Bool) -> [a] -> a
firstTwice _ [] = error "no moves"
firstTwice fn (x:xs) | any (fn x) xs = x
                     | otherwise = firstTwice fn xs

main :: IO ()
main = do
  input <- getLine
  let answer = puzzle2 input
  print answer
