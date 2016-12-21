import qualified Data.ByteString.UTF8 as U
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import MD5

-- cache all the things
data Candidate = Candidate
  { cIndex :: Int
  , cValue :: String
  , cFivers :: S.Set Char
  } deriving (Show)

findRuns :: Int -> String -> [Char]
findRuns n s
  | length s < n = []
  | otherwise =
    if all (== c) hd
      then c : (findRuns n (drop n s))
      else findRuns n (tail s)
  where
    c = head s
    hd = take n s

stretch :: Int -> (a -> a) -> a -> a
stretch 0 _ = id
stretch n f = f . stretch (n - 1) f

mkCandidate :: Int -> String -> Int -> Candidate
mkCandidate stretches salt idx = Candidate idx val fivers
  where
    val = (stretch stretches md5) $ salt ++ show idx
    fivers = S.fromList $ findRuns 5 val

candSeq :: Int -> String -> [Candidate]
candSeq stretches salt = map (mkCandidate stretches salt) [0..]

keys :: Int -> String -> [Candidate]
keys stretches salt = map head $ filter isKey $ tails $ candSeq stretches salt

isKey :: [Candidate] -> Bool
isKey (candidate:hashes) = any (S.member triplet . cFivers) next1000
  where
    triplet = fromMaybe '*' $ listToMaybe $ findRuns 3 $ cValue candidate
    next1000 = take 1000 hashes
