import Data.List
import Data.List.Split


takeDrop :: Int -> [a] -> ([a], [a])
takeDrop 0 ls = ([], ls)
takeDrop i (l:ls) =
  let (hd, tl) = takeDrop (i - 1) ls
  in (l : hd, tl)

splitFirst :: Eq a => a -> [a] -> ([a], [a])
splitFirst chr str =
  let (Just i) = elemIndex chr str
      (hd, tl) = takeDrop i str
  in (hd, tail tl)

decompress :: String -> String
decompress "" = ""
decompress ('(':specrest) =
  let (spec, rest) = splitFirst ')' specrest
      [count, multiplier] = map read $ splitOn "x" spec
      (chunk, asis) = takeDrop count rest
      result = concat $ replicate multiplier chunk
  in result ++ decompress asis
decompress (c:cs) = c : decompress cs

decompress2 :: String -> Int
decompress2 "" = 0
decompress2 ('(':specrest) =
  let (spec, rest) = splitFirst ')' specrest
      [count, multiplier] = map read $ splitOn "x" spec
      (chunk, asis) = takeDrop count rest
      result = multiplier * decompress2 chunk
  in result + decompress2 asis
decompress2 (_:cs) = decompress2 cs + 1
