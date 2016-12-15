import Data.Function
import Data.List
import qualified Data.Map as M


readLines :: IO [String]
readLines =
  getLine >>=
  \s ->
     case s of
       "" -> pure []
       _ -> fmap (s :) readLines

denoise :: [String] -> String
denoise = map denoiseC . transpose

denoiseC :: (Eq a, Ord a) => [a] -> a
denoiseC = fst . head . sortBy (compare `on` snd) . M.toList . M.fromListWith (+) . flip zip (repeat 1)
