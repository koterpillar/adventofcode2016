import Control.Monad

import MD5
import Path
import Utils


data Situation = Situation
  { sPasscode :: String
  , sPosition :: Position2
  , sSteps :: [Direction4]
  } deriving (Ord, Eq)

instance Show Situation where
  show (Situation passcode position steps) = "Situation: code=" ++ show passcode ++ " position=(" ++ show (pX position) ++ ", " ++ show (pY position) ++ ") steps=" ++ show (map directionLetter $ reverse steps)

directionLetter :: Direction4 -> Char
directionLetter N = 'U'
directionLetter S = 'D'
directionLetter W = 'L'
directionLetter E = 'R'

vaultSize :: Int
vaultSize = 4

mkSituation :: String -> Situation
mkSituation passcode = Situation passcode (Position2 1 1) []

sSuccess :: Situation -> Bool
sSuccess s = sPosition s == Position2 vaultSize vaultSize

type Move = Direction4

moves :: Situation -> [Move]
moves = const [S, E, N, W]

apply :: Situation -> Move -> Maybe Situation
apply situation@(Situation _ pos steps) step = do
  let pos' = walk step pos
  guard $ pX pos' > 0
  guard $ pY pos' > 0
  guard $ pX pos' <= vaultSize
  guard $ pY pos' <= vaultSize
  guard $ doorOpen situation step
  pure $ situation { sPosition = pos', sSteps = step : steps }

doorOpen :: Situation -> Direction4 -> Bool
doorOpen (Situation passcode _ steps) direction = goodLetter (hashed !! letterIndex direction)
  where
    letterIndex N = 0
    letterIndex S = 1
    letterIndex W = 2
    letterIndex E = 3
    hashed = md5 (passcode ++ map directionLetter (reverse steps))
    goodLetter letter = elem letter "bcdef"

sMoveTree :: Situation -> Tree Situation Move
sMoveTree = moveTree moves $ flip apply

sLevels :: Tree Situation Move -> [[Situation]]
sLevels = levels id

sPath :: [[Situation]] -> [[Situation]]
sPath = shortestPath sSuccess

demo ::Situation
demo = mkSituation "hijkl"
