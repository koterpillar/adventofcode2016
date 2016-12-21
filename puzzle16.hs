dragon :: String -> String
dragon a = a ++ "0" ++ map invert (reverse a)
  where
    invert '0' = '1'
    invert '1' = '0'

fill :: Int -> String -> String
fill size init =
  if length init > size
    then take size init
    else fill size (dragon init)

checksum :: String -> String
checksum s
  | odd (length s) = s
  | otherwise = checksum (mkChecksum s)
  where
    mkChecksum [] = []
    mkChecksum (a:b:rest)
      | a == b = '1' : mkChecksum rest
      | otherwise = '0' : mkChecksum rest
