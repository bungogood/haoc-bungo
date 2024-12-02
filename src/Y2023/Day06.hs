module Y2023.Day06 (Solution (..)) where

-- https://adventofcode.com/2023/day/6

import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . product . map numWin . parse . lines
  part2 _ = show . numWin . parse' . lines

parse :: [String] -> [(Int, Int)]
parse [time, distance] = zip (finder time) (finder distance)
  where
    finder = map read . tail . words
parse _ = []

parse' :: [String] -> (Int, Int)
parse' [time, distance] = (finder time, finder distance)
  where
    finder = read . concat . tail . words
parse' _ = (0, 0)

numWin :: (Int, Int) -> Int
numWin (t, d) = max 0 (ceiling upper - floor lower - 1)
  where
    (a, b, c) = (1.0 :: Double, fromIntegral (-t) :: Double, fromIntegral d :: Double)
    discriminant = sqrt (b ** 2 - 4 * a * c) -- Use `**` for floating-point exponentiation
    lower = (-b - discriminant) / (2 * a)
    upper = (-b + discriminant) / (2 * a)
