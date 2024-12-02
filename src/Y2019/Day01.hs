module Y2019.Day01 (Solution (..)) where

-- https://adventofcode.com/2019/day/1

import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . map toFuel . findMasses
  part2 _ = show . sum . map (calFuel . toFuel) . findMasses

findMasses :: String -> [Int]
findMasses = map read . lines

toFuel :: Int -> Int
toFuel x = x `div` 3 - 2

calFuel :: Int -> Int
calFuel x
  | x <= 0 = 0
  | otherwise = x + calFuel (toFuel x)
