import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe


type Bot = Int

type Output = Int

type Value = Int

data Place
  = PBot Bot
  | POut Output
  deriving (Ord, Eq)

instance Show Place where
  show (PBot i) = "bot " ++ show i
  show (POut i) = "output " ++ show i

data Instruction
  = Set { setWhat :: Value
        , setWhom :: Bot}
  | Give { giveFrom :: Bot
         , giveLow :: Place
         , giveHigh :: Place}
  deriving (Eq)

instance Show Instruction where
  show (Set what whom) = "value " ++ show what ++ " goes to bot " ++ show whom
  show (Give from low high) = "bot " ++ show from ++ " gives low to " ++ show low ++ " and high to " ++ show high

parse :: String -> Instruction
parse str =
  case splitOn " " str of
    ["value", v, "goes", "to", "bot", b] -> Set (read v) (read b)
    ["bot", sb, "gives", "low", "to", lowT, lowI, "and", "high", "to", highT, highI] -> Give (read sb) (parseTo lowT lowI) (parseTo highT highI)

parseTo :: String -> String -> Place
parseTo "bot" = PBot . read
parseTo "output" = POut . read

type InstSet = [Instruction]

type Factory = M.Map Place [Value]

setup :: InstSet -> Factory
setup instr = foldr id M.empty [doGiveTo (PBot whom) what | Set what whom <- instr]

findFull :: Factory -> Maybe (Bot, [Value])
findFull st =
  listToMaybe
    [ (bot, values)
    | (PBot bot, values@[_, _]) <- M.toList st ]

step :: InstSet -> Factory -> Maybe Factory
step instr old =
  findFull old >>=
  \(whom, values) -> Just $ doGive instr whom values $ M.delete (PBot whom) old

findInst :: InstSet -> Bot -> Instruction
findInst instr bot =
  head
    [ i
    | i@(Give bot' _ _) <- instr
    , bot == bot' ]

doGive :: InstSet -> Bot -> [Value] -> Factory -> Factory
doGive instr bot values =
  doGiveTo (giveLow botInst) low . doGiveTo (giveHigh botInst) high
  where
    botInst = findInst instr bot
    [low, high] = sort values

doGiveTo :: Place -> Value -> Factory -> Factory
doGiveTo receiver value = M.insertWith (++) receiver [value]

readLines :: IO [String]
readLines =
  getLine >>=
  \s ->
     case s of
       "" -> pure []
       _ -> fmap (s :) readLines

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a =
  a :
  case f a of
    Just a' -> iterateMaybe f a'
    Nothing -> []

trace :: InstSet -> [Factory]
trace instr = iterateMaybe (step instr) (setup instr)

compares :: [Value] -> Factory -> Maybe Bot
compares values =
  fmap (\(PBot i, _) -> i) .
  listToMaybe .
  filter comparingBot . M.toList
  where
    comparingBot (PBot _, botValues) = sort botValues == sort values
    comparingBot _ = False
