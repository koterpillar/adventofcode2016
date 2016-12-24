module Utils where

readLines :: IO [String]
readLines =
  getLine >>=
  \s ->
     case s of
       "" -> pure []
       _ -> fmap (s :) readLines

data Direction4
  = E
  | N
  | W
  | S
  deriving (Enum, Eq, Ord, Show)

data Position2 = Position2
  { pX :: Int
  , pY :: Int
  } deriving (Eq, Ord, Show)

walk :: Direction4 -> Position2 -> Position2
walk E (Position2 x y) = Position2 (x + 1) y
walk W (Position2 x y) = Position2 (x - 1) y
walk N (Position2 x y) = Position2 x (y - 1)
walk S (Position2 x y) = Position2 x (y + 1)

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateWhile continue fn v
  | continue v =
    let v' = fn v
    in v : iterateWhile continue fn v'
  | otherwise = []
