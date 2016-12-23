import MD5
import Path
import Utils


data Situation = Situation
  { sPasscode :: String
  , sPosition :: Position2
  , sSteps :: [Direction4]
  } deriving (Ord, Eq, Show)

vaultSize :: Int
vaultSize = 4

init :: String -> Situation
init passcode = Situation passcode (Position2 1 1) []

type Move = Direction4

moves :: Situation -> [Move]
moves = const [S, E, N, W]

apply :: Situation -> Move -> Maybe Situation
apply situation@(Situation _ pos steps) step =
  let pos' = walk step pos
      valid =
        pX pos > 0 &&
        pY pos > 0 &&
        pX pos <= vaultSize && pY pos <= vaultSize && doorOpen situation step
  in if valid
       then Just situation { sPosition = pos', sSteps = step : steps }
       else Nothing

doorOpen :: Situation -> Direction4 -> Bool
doorOpen (Situation passcode _ steps) direction = goodLetter (hashed !! letterIndex direction)
  where
    letterIndex N = 0
    letterIndex S = 1
    letterIndex W = 2
    letterIndex E = 3
    hashed = md5 (passcode ++ map directionLetter steps)
    directionLetter N = 'U'
    directionLetter S = 'D'
    directionLetter W = 'L'
    directionLetter E = 'R'
    goodLetter letter = elem letter "bcdef"
