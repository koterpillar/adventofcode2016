module MD5 where

import Crypto.Hash

import Data.ByteString.UTF8 hiding (take)

md5bs :: ByteString -> String
md5bs = show . (hash :: ByteString -> Digest MD5)

md5 :: String -> String
md5 = md5bs . fromString
