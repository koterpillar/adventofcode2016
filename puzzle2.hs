import Control.Monad.State

import Data.List
import Data.Maybe


type Key = Char

data Dir = U | D | L | R deriving (Eq, Show)

readDir :: Char -> Dir
readDir 'U' = U
readDir 'D' = D
readDir 'L' = L
readDir 'R' = R

keypad :: [[Maybe Key]]
keypad = map (map spc) ["  1  ", " 234 ", "56789", " ABC ", "  D  "]
  where
    spc ' ' = Nothing
    spc c = Just c

keypadPos :: Key -> Maybe (Int, Int)
keypadPos = kp' keypad 0
  where
    kp' :: [[Maybe Key]] -> Int -> Key -> Maybe (Int, Int)
    kp' [] _ key = Nothing
    kp' (r:rs) y key =
      case elemIndex (Just key) r of
        Just x -> Just (x, y)
        Nothing -> kp' rs (y + 1) key

elemAt :: Int -> [a] -> Maybe a
elemAt _ [] = Nothing
elemAt 0 (x:_) = Just x
elemAt i (_:xs)
  | i > 0 = elemAt (i - 1) xs
  | otherwise = Nothing

posKey :: (Int, Int) -> Maybe Key
posKey (x, y) = do
  row <- elemAt y keypad
  key <- elemAt x row
  key

moveI :: Dir -> (Int, Int) -> (Int, Int)
moveI U (x, y) = (x, y - 1)
moveI D (x, y) = (x, y + 1)
moveI L (x, y) = (x - 1, y)
moveI R (x, y) = (x + 1, y)

move :: Key -> Dir -> Key
move key dir = fromMaybe key $ do
  pos <- keypadPos key
  posKey (moveI dir pos)

start :: Key
start = '5'

ln :: Key -> [Dir] -> Key
ln = foldl move

code :: String -> [Key]
code = code' . lines

code' :: [String] -> [Key]
code' = tail . scanl ln start . map (map readDir)

readCodes :: IO [String]
readCodes =
  getLine >>=
  \s ->
     case s of
       "" -> pure []
       _ -> fmap (s :) readCodes
