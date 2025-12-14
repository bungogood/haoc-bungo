module Y2025.Day06 (Solution (..)) where

-- https://adventofcode.com/2025/day/6

import Data.List (transpose)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . uncurry downCol . parse
  part2 _ = show . sum . uncurry flipCol . parse

parse :: String -> ([Char], [[String]])
parse s = (ops, rows)
  where
    ls = lines s
    rows = parseRows (init ls)
    ops = map head (words (last ls))

flush :: [String] -> [[String]] -> [[String]]
flush cur = zipWith (:) (map reverse cur)

empty :: [[a]] -> [[b]]
empty xs = replicate (length xs) []

parseRows :: [String] -> [[String]]
parseRows ls = parseRows' (empty ls) (empty ls) ls

parseRows' :: [String] -> [[String]] -> [String] -> [[String]]
parseRows' cur res rest
  | all null rest =
      map reverse (flush cur res)
  | all (startsWith ' ') rest =
      parseRows' (empty cur) (flush cur res) (map tail rest)
  | otherwise =
      parseRows' (zipWith (:) (map head rest) cur) res (map tail rest)

startsWith :: (Eq a) => a -> [a] -> Bool
startsWith c (x : _) = c == x
startsWith _ _ = False

downCol :: [Char] -> [[String]] -> [Int]
downCol ops = zipWith applyOp ops . (map (map read) . transpose)

flipCol :: [Char] -> [[String]] -> [Int]
flipCol ops = zipWith applyOp ops . (map (map read . transpose) . transpose)

applyOp :: Char -> [Int] -> Int
applyOp '+' xs = sum xs
applyOp '*' xs = product xs
applyOp _ _ = 0
