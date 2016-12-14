import Data.List
import Data.List.Split

readLines :: IO [String]
readLines =
  getLine >>=
  \s ->
     case s of
       "" -> pure []
       _ -> fmap (s :) readLines

isTriangle :: [Int] -> Bool
isTriangle [a, b, c] = a + b > c && a + c > b && b + c > a

transposeTri :: [[a]] -> [[a]]
transposeTri [] = []
transposeTri (x:y:z:rs) = x' : y' : z' : (transposeTri rs)
  where
    [x', y', z'] = transpose [x, y, z]

readInput :: IO [[Int]]
readInput = fmap (map rd) readLines
  where rd = map read . filter (/= "") . splitOn " "
