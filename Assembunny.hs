module Assembunny where

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

import Utils


type Register = Char

type Value = Int

data Expr
  = SRegister Register
  | SValue Value
  deriving (Eq, Ord, Show)

type Offset = Int

data Instruction
  = Copy Expr
         Expr
  | Inc Expr
  | Dec Expr
  | Jnz Expr
        Expr
  | Toggle Expr
  deriving (Ord, Eq, Show)

type Registers = M.Map Register Value

type Program = [Instruction]

type Address = Int

data Computer = Computer
  { cRegisters :: Registers
  , cProgram :: Program
  , cIP :: Address
  } deriving (Show)

registers :: [Register]
registers = "abcd"

boot :: Program -> Computer
boot instructions = Computer (M.fromList $ zip registers (repeat 0)) instructions 0

stopped :: Computer -> Bool
stopped (Computer _ program ip) = ip >= length program

nflow :: (Registers -> Registers) -> Computer -> Computer
nflow f (Computer r p i) = Computer (f r) p (i + 1)

nonmodifying :: ((Registers, Address) -> (Registers, Address)) -> Computer -> Computer
nonmodifying f (Computer r p i) = let (r', i') = f (r, i) in Computer r' p i'

peek :: Register -> Registers -> Value
peek reg = fromJust . M.lookup reg

pokeC :: Register -> Value -> Computer -> Computer
pokeC r v c = c { cRegisters = M.insert r v (cRegisters c) }

eval :: Expr -> Registers -> Value
eval src regs =
  case src of
    (SValue val) -> val
    (SRegister reg) -> peek reg regs

toggle :: Instruction -> Instruction
toggle (Copy s d) = Jnz s d
toggle (Inc r) = Dec r
toggle (Dec r) = Inc r
toggle (Jnz s d) = Copy s d
toggle (Toggle r) = Inc r

toggleC :: Int -> Computer -> Computer
toggleC index c@(Computer regs program ip)
  | index < 0 = nflow id c
  | index >= length program = nflow id c
  | otherwise =
    let program' = sset index (toggle (program !! index)) program
    in Computer regs program' (ip + 1)

step :: Computer -> Computer
step c = apply (drop (cIP c) (cProgram c)) c
  where
    apply :: [Instruction] -> Computer -> Computer
    -- optimize: c = V; inc a; dec c; jnc c -2; dec d; jnz d -5 to a += V * d; c = 0; d = 0
    apply (instr1@(Copy v (SRegister rC1)):(Inc (SRegister rA1)):(Dec (SRegister rC2)):(Jnz (SRegister rC3) (SValue (-2))):(Dec (SRegister rD1)):(Jnz (SRegister rD2) (SValue (-5))):_)
      | rC1 == rC2 && rC2 == rC3 && rD1 == rD2 =
        nonmodifying $
        \(regs, ip) ->
           ( M.insert rA1 (peek rA1 regs + peek rD1 regs * eval v regs) $
             M.insert rC1 0 $ M.insert rD1 0 $ regs
           , ip + 6)
      | otherwise = apply [instr1]
    apply (Copy src (SRegister dest):_) =
      nflow $ \regs -> M.insert dest (eval src regs) regs
    -- invalid copy instruction as a result of a toggle
    apply (Copy src _:_) = nflow id
    -- optimize a loop of: inc a; dec b; jnz b -2 to adding b to a and zeroing it
    apply (instr1@(Inc (SRegister rA)):(Dec (SRegister rB1)):(Jnz (SRegister rB2) (SValue (-2))):_)
      | rB1 == rB2 =
        nonmodifying $
        \(regs, ip) ->
           ( M.insert rA (peek rA regs + peek rB1 regs) $ M.insert rB1 0 $ regs
           , ip + 3)
      | otherwise = apply [instr1]
    apply (Inc (SRegister reg):_) = nflow $ M.adjust succ reg
    apply (Dec (SRegister reg):_) = nflow $ M.adjust pred reg
    -- invalid inc/dec instruction as a result of a toggle
    apply (Inc _:_) = nflow id
    apply (Dec _:_) = nflow id
    apply (Jnz src dest:_) =
      nonmodifying $
      \(regs, ip) ->
         if eval src regs == 0
           then (regs, succ ip)
           else (regs, ip + eval dest regs)
    apply (Toggle dest:_) =
      \c@(Computer regs _ ip) ->
         let instr = ip + eval dest regs
         in toggleC instr c

run :: Computer -> Computer
run = head . dropWhile (not . stopped) . iterate step

parse :: [String] -> Program
parse = map (parseInstr . splitOn " ")
  where
    parseInstr ["cpy", src, dst] = Copy (parseExpr src) (parseExpr dst)
    parseInstr ["inc", reg] = Inc $ parseExpr reg
    parseInstr ["dec", reg] = Dec $ parseExpr reg
    parseInstr ["jnz", src, dst] = Jnz (parseExpr src) (parseExpr dst)
    parseInstr ["tgl", offset] = Toggle (parseExpr offset)
    parseReg (r:[]) = r
    parseReg e = error $ e ++ " is not a register"
    parseExpr src =
      if elem (head src) registers
        then SRegister $ parseReg src
        else SValue $ read src

readProgram :: IO Program
readProgram = fmap parse readLines
