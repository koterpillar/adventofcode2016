import Data.List
import Data.List.Split
import Data.Maybe

import Utils

abba :: Eq a => [a] -> Bool
abba lst@(a:b:c:d:_)
  | a == d && b == c && a /= b = True
  | otherwise = abba (tail lst)
abba _ = False

aba :: Eq a => [a] -> [[a]]
aba lst = mapMaybe go (tails lst)
  where
    go (a:b:c:_)
      | a == c && a /= b = Just [a, b, c]
    go _ = Nothing

bab :: [a] -> [a]
bab [a, b, _] = [b, a, b]

superhyper :: String -> ([String], [String])
superhyper str =
  let parts = splitOneOf "[]" str
      [outer, inner] = transpose $ chunksOf 2 parts
  in (outer, inner)

tls :: String -> Bool
tls str =
  let (outer, inner) = superhyper str
  in any abba outer && all (not . abba) inner

ssl :: String -> Bool
ssl str =
  let (outer, inner) = superhyper str
      abas = concatMap aba outer
      babs = map bab abas
      isBab candidate = any (flip isInfixOf candidate) babs
  in any isBab inner
