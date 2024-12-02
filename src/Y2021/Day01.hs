module Y2021.Day01 (Solution (..)) where

-- https://adventofcode.com/2021/day/1

import Data.List (tails)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . increases . findDepths
  part2 _ = show . increases . map sum . slidingWindow 3 . findDepths

findDepths :: String -> [Int]
findDepths = map read . lines

increases :: [Int] -> Int
increases xs = length . filter (uncurry (<)) $ zip xs (tail xs)

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = filter ((== n) . length) . map (take n) . tails
