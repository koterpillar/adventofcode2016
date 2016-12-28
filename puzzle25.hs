import Data.List
import qualified Data.Map as M

import Assembunny
import Utils

type SeenStates = M.Map Computer [Value]

makesClock :: Program -> Value -> Bool
makesClock program initValue =
  go M.empty (pokeC 'a' initValue $ boot program) []
  where
    go :: SeenStates -> Computer -> [Value] -> Bool
    go seen c out
      | stopped c = False
      | otherwise =
        case M.lookup c seen of
          Just out' -> hasPeriod out out'
          Nothing -> go (M.insert c out seen) c' out'
            where (c', nextOut) = step' c
                  out' = maybePrepend nextOut out

maybePrepend :: Maybe a -> [a] -> [a]
maybePrepend (Just v) vs = v:vs
maybePrepend Nothing vs = vs

hasPeriod :: [Value] -> [Value] -> Bool
hasPeriod values oldValues =
  even (length values - length oldValues) &&
  length values > 0 && isPrefixOf values (cycle [1, 0])

test1 :: Program
test1 = parse $ ["out 0", "out 1", "jnz 1 -2"]
