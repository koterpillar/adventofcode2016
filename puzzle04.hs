import Data.Function
import Data.List
import Data.List.Split

isLL :: Char -> Bool
isLL c = c >= 'a' && c <= 'z'

roomChecksum :: String -> String
roomChecksum code = take 5 $ sortBy (compareCounts code) ['a'..'z']

compareCounts :: String -> Char -> Char -> Ordering
compareCounts code = compare `on` (\c -> (negate $ length $ filter (== c) code, c))

type Room = (String, Int, String)

parseRoom :: String -> Room
parseRoom str =
  let parts = splitOn "-" str
      lastPart:codeParts = reverse parts
      name = intercalate "-" $ reverse codeParts
      (code:checksum':[]) = splitOn "[" lastPart
      checksum = reverse $ tail $ reverse checksum'
  in (name, read code, checksum)

readLines :: IO [String]
readLines =
  getLine >>=
  \s ->
     case s of
       "" -> pure []
       _ -> fmap (s :) readLines

decrypt :: Int -> String -> String
decrypt i = map (shift i)
  where
    shift i c = iterate inc c !! i
    inc 'z' = 'a'
    inc '-' = ' '
    inc ' ' = ' '
    inc c = succ c

isReal :: Room -> Bool
isReal (name, _, checksum) = roomChecksum name == checksum

main = do
  rooms <- fmap (map parseRoom) readLines
  let realRooms = filter isReal rooms
  print $ sum $ map (\(_, code, _) -> code) realRooms
  let decrypted = map (\(name, code, _) -> (decrypt code name, code)) realRooms
  traverse print decrypted
