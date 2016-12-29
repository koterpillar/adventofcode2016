import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Assembunny
import Utils

type CState = (Registers, Address)

cState :: Computer -> CState
cState (Computer regs _ ip) = (regs, ip)

-- which computers were expected to produce which states?
type SeenStates = M.Map Value (S.Set CState)

lookupSeen :: Value -> SeenStates -> S.Set CState
lookupSeen value seen = let (Just cs) = M.lookup value seen in cs

addSeen :: Value -> Computer -> SeenStates -> SeenStates
addSeen value c = M.adjust (S.insert $ cState c) value

emptySeen :: SeenStates
emptySeen = M.fromList [(0, S.empty), (1, S.empty)]

nextValue :: Value -> Value
nextValue 0 = 1
nextValue 1 = 0

makesClock :: Program -> Value -> Either [Value] ()
makesClock program initValue =
  go emptySeen (pokeC 'a' initValue $ boot program) 0 []
  where
    ret :: [Value] -> Either [Value] ()
    ret = Left . reverse
    go :: SeenStates -> Computer -> Value -> [Value] -> Either [Value] ()
    go seen c expected produced
      | S.member (cState c) (lookupSeen expected seen) = Right ()
      | S.member (cState c) (lookupSeen (nextValue expected) seen) = ret produced
      | otherwise =
        let (c', nextOut) = step' c
            seen' = addSeen expected c seen
        in case nextOut of
             Nothing -> go seen' c' expected produced
             Just actual -> if actual == expected then go seen' c' (nextValue expected) (actual:produced) else ret produced

maybePrepend :: Maybe a -> [a] -> [a]
maybePrepend (Just v) vs = v:vs
maybePrepend Nothing vs = vs

hasPeriod :: [Value] -> [Value] -> Bool
hasPeriod values oldValues =
  even (length values - length oldValues) &&
  length values > 0 && isPrefixOf values (cycle [1, 0])

test1 :: Program
test1 = parse $ ["out 0", "out 1", "out a", "out 1", "jnz 1 -2"]

-- Skip uninteresting values
heuristic :: Int -> Bool
heuristic i | i `mod` 2 == 1 = False
            | i `mod` 4 == 2 = False
            | i `mod` 8 == 0 = False
            | i `mod` 16 == 12 = False
            | otherwise = True
