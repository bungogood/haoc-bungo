module Y2022.Day06 (Solution (..)) where

-- https://adventofcode.com/2022/day/6

import Data.List (nub)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . buffer 4
  part2 _ = show . buffer 14

buffer :: Int -> String -> Int
buffer n s
  | nub (take n s) == take n s = n
  | otherwise = 1 + buffer n (tail s)
