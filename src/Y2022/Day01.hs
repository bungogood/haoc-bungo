module Y2022.Day01 (Solution (..)) where

-- https://adventofcode.com/2022/day/1

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Ord (Down (..), comparing)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . bestElf . elfCals
  part2 _ = show . topElves 3 . elfCals

elfCals :: String -> [[Int]]
elfCals = map (map read) . splitOn [""] . lines

bestElf :: [[Int]] -> Int
bestElf = maximum . map sum

topElves :: Int -> [[Int]] -> Int
topElves n = sum . take n . sortBy (comparing Down) . map sum
