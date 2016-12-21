import Data.List.Split

import Utils

data Disk = Disk
  { dId :: Int
  , dPeriod :: Int
  , dPosition :: Int
  } deriving (Show)

parseDisk :: String -> Disk
parseDisk str = Disk did period position
  where
    [did, period, _, position] =
      map read $ filter (not . null) $ splitOneOf notDigits str
    notDigits = ['a' .. 'z'] ++ "D #;,.="

type Sculpture = [Disk]

parse :: IO Sculpture
parse = fmap (map parseDisk) readLines

demo :: Sculpture
demo =
  map
    parseDisk
    [ "Disc #1 has 5 positions; at time=0, it is at position 4."
    , "Disc #2 has 2 positions; at time=0, it is at position 1."
    ]

stepDisk :: Disk -> Disk
stepDisk (Disk i period position) = Disk i period (succ position `mod` period)

step :: Sculpture -> Sculpture
step = map stepDisk

pass :: Int -> Sculpture -> Bool
pass time = all (uncurry passDisk) . zip [(time + 1) ..]
  where
    passDisk delay (Disk _ period position) =
      (position + delay) `mod` period == 0
