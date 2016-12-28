module Assembunny where

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

import Utils


type Register = Char

type Value = Int

data Source
  = SRegister Register
  | SValue Value
  deriving (Eq, Ord, Show)

data Instruction
  = Copy Source
         Register
  | Inc Register
  | Dec Register
  | Jnz Source
        Int
  deriving (Ord, Eq, Show)

type Registers = M.Map Register Value

type Program = [Instruction]

data Computer = Computer
  { cRegisters :: Registers
  , cProgram :: Program
  , cIP :: Int
  } deriving (Show)

registers :: [Register]
registers = "abcd"

boot :: Program -> Computer
boot instructions = Computer (M.fromList $ zip registers (repeat 0)) instructions 0

stopped :: Computer -> Bool
stopped (Computer _ program ip) = ip >= length program

nflow :: (a -> a) -> (a, Int) -> (a, Int)
nflow f (r, i) = (f r, i + 1)

peek :: Register -> Registers -> Value
peek reg = fromJust . M.lookup reg

pokeC :: Register -> Value -> Computer -> Computer
pokeC r v c = c { cRegisters = M.insert r v (cRegisters c) }

eval :: Source -> Registers -> Value
eval src regs =
  case src of
    (SValue val) -> val
    (SRegister reg) -> peek reg regs

step :: Computer -> Computer
step c@(Computer regs program ip) = Computer regs' program ip'
  where
    (regs', ip') = apply (drop ip program) (regs, ip)
    apply :: [Instruction] -> (Registers, Int) -> (Registers, Int)
    apply (Copy src dest:_) = nflow $ \regs -> M.insert dest (eval src regs) regs
    -- optimize a loop of: inc a; dec b; jnz b -2 to adding b to a and zeroing it
    apply ((Inc r1):(Dec r2):(Jnz (SRegister r3) (-2)):_)
      | r2 == r3 =
        \(regs, ip) ->
           ( M.insert r1 (peek r1 regs + peek r2 regs) $ M.insert r2 0 $ regs
           , ip + 3)
      | otherwise = apply [Inc r1]
    apply (Inc reg:_) = nflow $ M.adjust succ reg
    apply (Dec reg:_) = nflow $ M.adjust pred reg
    apply (Jnz src offset:_) =
      \(regs, ip) ->
         if eval src regs == 0
           then (regs, succ ip)
           else (regs, ip + offset)

run :: Computer -> Computer
run = head . dropWhile (not . stopped) . iterate step

parse :: [String] -> Program
parse = map (parseInstr . splitOn " ")
  where
    parseInstr ["cpy", src, dst] = Copy (parseSrc src) (parseReg dst)
    parseInstr ["inc", reg] = Inc $ parseReg reg
    parseInstr ["dec", reg] = Dec $ parseReg reg
    parseInstr ["jnz", src, offset] = Jnz (parseSrc src) (read offset)
    parseReg (r:[]) = r
    parseReg e = error $ e ++ " is not a register"
    parseSrc src = if elem (head src) registers
                    then SRegister $ parseReg src
                    else SValue $ read src

readProgram :: IO Program
readProgram = fmap parse readLines
