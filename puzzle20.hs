import Control.Applicative

import Data.List.Split

import Utils

type Address = Int

data Range = Range { rStart :: Address, rEnd :: Address } deriving (Eq, Show)

rLength :: Range -> Int
rLength (Range start end) = end - start + 1

parse :: [String] -> [Range]
parse = map parseRange
  where
    parseRange str =
      let [start, end] = splitOn "-" str
      in Range (read start) (read end)

readParse :: IO [Range]
readParse = fmap parse readLines

allAddresses :: [Range]
allAddresses = [Range 0 4294967295]

ban :: Range -> [Range] -> [Range]
ban (Range deniedStart deniedEnd) = concatMap banRange
  where
    banRange :: Range -> [Range]
    banRange range@(Range start end)
      | start > deniedEnd = [range]
      | end < deniedStart = [range]
      | otherwise =
        filter
          validRange
          [Range start (deniedStart - 1), Range (deniedEnd + 1) end]
    validRange (Range start end) = end >= start

allowed :: [Range] -> [Range]
allowed = foldr ban allAddresses

demoBans :: [Range]
demoBans = parse ["5-8", "0-2", "4-7"]

lowestAllowed :: [Range] -> Address
lowestAllowed = minimum . map rStart

allowedCount :: [Range] -> Int
allowedCount = sum . map rLength
