import Data.Char
import Data.List.Split

import Utils

data Node = Node
  { nPosition :: Position2
  , nSize :: Int
  , nUsed :: Int
  , nAvail :: Int
  , nUsePercent :: Int
  } deriving (Eq, Ord, Show)

parseNode :: String -> Node
parseNode str =
  let [namePart, sizePart, usedPart, availPart, usePercentPart] =
        filter (not . null) $ splitOn " " str
      [_, 'x':xPart, 'y':yPart] = splitOn "-" namePart
      takeDigits = takeWhile isDigit
      sizeStr = takeDigits sizePart
      usedStr = takeDigits usedPart
      availStr = takeDigits availPart
      usePercentStr = takeDigits usePercentPart
  in Node
       (Position2 (read xPart) (read yPart))
       (read sizeStr)
       (read usedStr)
       (read availStr)
       (read usePercentStr)

parse :: [String] -> [Node]
parse = map parseNode . drop 2

viablePair :: Node -> Node -> Bool
viablePair a b =
  nUsed a > 0 && nPosition a /= nPosition b && nUsed a <= nAvail b
