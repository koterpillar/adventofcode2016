import Data.Bool
import Data.List.Split

data Screen = Screen
  { pixels :: [[Bool]]
  } deriving (Eq, Ord)

width :: Int
width = 50

height :: Int
height = 6

type ScreenFn = Int -> Int -> Bool

screenFn :: Screen -> ScreenFn
screenFn (Screen px) x y = px !! y !! x

fnScreen :: ScreenFn -> Screen
fnScreen f =
  Screen
    [ [ f x y
      | x <- [0 .. width - 1] ]
    | y <- [0 .. height - 1] ]

using :: (a -> b) -> (b -> a) -> (b -> b) -> (a -> a)
using f g h = g . h . f

usfn :: (ScreenFn -> ScreenFn) -> Screen -> Screen
usfn = using screenFn fnScreen

empty :: Screen
empty = fnScreen $ const $ const False

instance Show Screen where
  show = unlines . map (map (bool '.' '#')) . pixels

rect :: Int -> Int -> Screen -> Screen
rect w h = usfn $ \scr x y -> scr x y || (x < w && y < h)

wrap :: Int -> Int -> Int -> Int
wrap limit amount i = (i - amount) `mod` limit

wrapY :: Int -> Int -> Int
wrapY = wrap height

wrapX :: Int -> Int -> Int
wrapX = wrap width

rotateCol :: Int -> Int -> Screen -> Screen
rotateCol col amount = usfn $ \scr x y -> scr x (if x == col then wrapY amount y else y)

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow row amount = usfn $ \scr x y -> scr (if y == row then wrapX amount x else x) y

command :: String -> Screen -> Screen
command str =
  case splitOn " " str of
    ["rect", def] ->
      let [x, y] = map read (splitOn "x" def)
      in rect x y
    ["rotate", "column", ('x':'=':colDef), "by", amount] -> rotateCol (read colDef) (read amount)
    ["rotate", "row", ('y':'=':rowDef), "by", amount] -> rotateRow (read rowDef) (read amount)

countPixels :: Screen -> Int
countPixels (Screen px) = sum $ map (sum . map (bool 0 1)) px

go :: [String] -> Screen
go = foldl (flip command) empty

readLines :: IO [String]
readLines =
  getLine >>=
  \s ->
     case s of
       "" -> pure []
       _ -> fmap (s :) readLines
