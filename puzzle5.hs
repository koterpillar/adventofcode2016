import Crypto.Hash

import Data.ByteString.UTF8 hiding (take)
import Data.Maybe
import Data.Monoid

md5 :: ByteString -> Digest MD5
md5 = hash

getPCharHash :: Digest MD5 -> Maybe (Char, Char)
getPCharHash hash =
  case show hash of
    '0':'0':'0':'0':'0':c:d:_ -> Just (c, d)
    _ -> Nothing

getPChar :: ByteString -> Int -> Maybe (Char, Char)
getPChar str i =
  let candidate = str <> fromString (show i)
      hash = md5 candidate
  in getPCharHash hash

getPChars :: ByteString -> [(Char, Char)]
getPChars s = mapMaybe (getPChar s) [0..]

password :: String -> String
password = take 8 . map fst . getPChars . fromString

fillIndices :: [(Char, Char)] -> String
fillIndices = go '0'
  where
    go :: Char -> [(Char, Char)] -> String
    go '8' _ = []
    go c pchars = (head $ map snd $ filter (\(pos, _) -> pos == c) pchars):go (succ c) pchars

password2 :: String -> String
password2 = fillIndices . getPChars . fromString
