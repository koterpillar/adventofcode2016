import Data.Function
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
  | Nop
  | AddZero Register Register -- addzero d s -> d := d + s, s := 0
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

optimize :: Program -> Program
optimize p@((Inc r1):(Dec r2):(Jnz (SRegister r3) (-2)):rest) | r2 == r3 = Nop:Nop:(AddZero r1 r2):optimize rest
                                                              | otherwise = head p:optimize (tail p)
optimize (p:ps) = p:optimize ps
optimize p = p

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
    (regs', ip') = apply (program !! ip) (regs, ip)
    apply :: Instruction -> (Registers, Int) -> (Registers, Int)
    apply (Copy src dest) = nflow $ \regs -> M.insert dest (eval src regs) regs
    apply (Inc reg) = nflow $ M.adjust succ reg
    apply (Dec reg) = nflow $ M.adjust pred reg
    apply (Jnz src offset) =
      \(regs, ip) ->
         if eval src regs == 0
           then (regs, succ ip)
           else (regs, ip + offset)
    apply Nop = nflow id
    apply (AddZero d s) = nflow $ \regs -> M.insert d (peek d regs + peek s regs) $ M.insert s 0 regs

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

demo :: [String]
demo = ["cpy 41 a", "inc a", "inc a", "dec a", "jnz a 2", "dec a"]
