import Data.List
import Data.List.Split
import Data.Maybe

import Utils

data Operation
  = SwapPositions Int
                  Int
  | SwapLetters Char
                Char
  | RotateLeft Int
  | RotateRight Int
  | RotateOnLetter Char
  | Reverse Int
            Int
  | Move Int
         Int
  deriving (Show)

parse :: String -> Operation
parse str =
  case splitOn " " str of
    ["swap", "position", x, "with", "position", y] ->
      SwapPositions (read x) (read y)
    ["swap", "letter", [x], "with", "letter", [y]] -> SwapLetters x y
    ["rotate", "left", x, _] -> RotateLeft (read x)
    ["rotate", "right", x, _] -> RotateRight (read x)
    ["rotate", "based", "on", "position", "of", "letter", [x]] -> RotateOnLetter x
    ["reverse", "positions", x, "through", y] -> Reverse (read x) (read y)
    ["move", "position", x, "to", "position", y] -> Move (read x) (read y)

readParse :: IO [Operation]
readParse = fmap (map parse) readLines

demo :: [Operation]
demo =
  map
    parse
    [ "swap position 4 with position 0"
    , "swap letter d with letter b"
    , "reverse positions 0 through 4"
    , "rotate left 1 step"
    , "move position 1 to position 4"
    , "move position 3 to position 0"
    , "rotate based on position of letter b"
    , "rotate based on position of letter d"
    ]

srotate :: Int -> [a] -> [a]
srotate amt lst =
  let amt' = amt `mod` length lst
  in drop amt' lst ++ take amt' lst

execute :: Operation -> String -> String
execute (SwapPositions x y) str = sset x ly $ sset y lx $ str
  where
    lx = str !! x
    ly = str !! y
execute (SwapLetters x y) str = sset ix ly $ sset iy lx $ str
  where
    lx = str !! ix
    ly = str !! iy
    Just ix = findIndex (== x) str
    Just iy = findIndex (== y) str
execute (RotateLeft i) str = srotate i str
execute (RotateRight i) str = srotate (-i) str
execute (RotateOnLetter x) str = srotate (-i) str
  where
    Just li = findIndex (== x) str
    i =
      if li < 4
        then li + 1
        else li + 2
execute (Reverse x y) str =
  take x str ++ reverse (take (y - x + 1) $ drop x str) ++ drop (y + 1) str
execute (Move x y) str =
  let (l, str') = sremove x str
  in sinsert y l str'


executeReverse :: Operation -> String -> String
executeReverse op@(SwapPositions _ _) str = execute op str
executeReverse op@(SwapLetters _ _) str = execute op str
executeReverse (RotateLeft i) str = execute (RotateRight i) str
executeReverse (RotateRight i) str = execute (RotateLeft i) str
executeReverse op@(Reverse _ _) str = execute op str
executeReverse op@(Move x y) str = execute (Move y x) str

-- Cheating!
executeReverse (RotateOnLetter x) str =
  fromJust $
  listToMaybe
    [ candidate
    | candidate <-
       [ srotate i str
       | i <- [0 .. length str - 1] ]
    , execute (RotateOnLetter x) candidate == str ]
