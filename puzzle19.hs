import qualified Data.Map as M
import Data.Maybe

import Debug.Trace

type Elf = Int

winner :: [Elf] -> Elf
winner (elf1:rest@(elf2:elf3:_)) | elf1 == elf3 = elf1
                                 | otherwise = winner rest

looped :: ([a] -> [a]) -> [a] -> [a]
looped fn seed = let result = fn (seed ++ result) in result

steal :: [Elf] -> [Elf]
steal (elf1:elf2:rest) = elf1:steal rest

game :: Int -> Int
game elfCount = winner $ looped steal [1 .. elfCount]

removeAt :: Int -> [a] -> [a]
removeAt 0 (_:rest) = rest
removeAt i (el:rest) = el : removeAt (i - 1) rest

type KillMap = M.Map Int Int

addKill :: Int -> KillMap -> KillMap
addKill idx = M.insertWith (+) idx 1

getKill :: Int -> KillMap -> (Int, KillMap)
getKill idx map = (fromMaybe 0 $ M.lookup idx map, M.delete idx map)

steal2 :: Int -> [Elf] -> [Elf]
steal2 len elves = go len 0 M.empty elves
  where
    go :: Int -> Int -> KillMap -> [Elf] -> [Elf]
    go len idx killed loop
      | len <= 0 = error "why no elves"
      | otherwise = traceShow len $ elf : go (len - 1) (idx + 1) killed'' rest
      where
        killNowCount :: Int
        killed' :: KillMap
        (killNowCount, killed') = getKill idx killed
        (elf:rest) = drop killNowCount loop
        target :: Int
        target = idx + (len `div` 2)
        killed'' :: KillMap
        killed'' = addKill target killed'

game2 :: Int -> Int
game2 elfCount =
  winner $ looped (steal2 elfCount) [1 .. elfCount]
