module Y2017.Day01 (Solution (..)) where

-- https://adventofcode.com/2017/day/1

import Data.Char (digitToInt)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . counter . offset 1 . toDigits
  part2 _ c = show . counter . offset (div (length digits) 2) $ digits
    where
      digits = toDigits c

toDigits :: String -> [Int]
toDigits = map digitToInt . head . lines

offset :: Int -> [Int] -> [(Int, Int)]
offset o xs = zip xs (drop o xs ++ take o xs)

counter :: [(Int, Int)] -> Int
counter = sum . map fst . filter (uncurry (==))
