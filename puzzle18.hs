data Tile = Safe | Trap deriving (Eq)

newtype Line = Line {lineTiles :: [Tile] }

instance Show Line where
  show = map showTile . lineTiles
    where
      showTile Safe = '.'
      showTile Trap = '^'

parse :: String -> Line
parse = Line . map readTile
  where
    readTile '.' = Safe
    readTile '^' = Trap

nextLine :: Line -> Line
nextLine (Line line) =
  Line $ zipWith3 isSafeTile (Safe : line) line (tail line ++ [Safe])
  where
    isSafeTile Trap Trap Safe = Trap
    isSafeTile Safe Trap Trap = Trap
    isSafeTile Trap Safe Safe = Trap
    isSafeTile Safe Safe Trap = Trap
    isSafeTile _ _ _ = Safe

buildLines :: Int -> String -> [Line]
buildLines n seed = take n $ iterate nextLine $ parse seed

countSafe :: [Line] -> Int
countSafe = length . filter (== Safe) . concat . map lineTiles
