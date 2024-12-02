module Y2015.Day04 (Solution (..)) where

-- https://adventofcode.com/2015/day/4

import Crypto.Hash (Digest, MD5, hash)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . finder 5 . head . lines
  part2 _ = show . finder 6 . head . lines

md5Hash :: ByteString -> ByteString
md5Hash input = convertToBase Base16 (hash input :: Digest MD5)

match :: ByteString -> ByteString -> Bool
match prefix input = C.isPrefixOf prefix (md5Hash input)

matcher :: ByteString -> String -> Int
matcher prefix input = head [i | i <- [1 ..], match prefix (C.pack (input ++ show i))]

finder :: Int -> String -> Int
finder n = matcher (C.pack (replicate n '0'))
