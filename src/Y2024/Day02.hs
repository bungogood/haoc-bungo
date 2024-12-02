module Y2024.Day02 (Solution (..)) where

import Lib (Solvable (..))

-- https://adventofcode.com/2024/day/2

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . length . filter isSafe . parse
  part2 _ = show . length . filter isSafeMissing . parse

parse :: String -> [[Int]]
parse = map (map read . words) . lines

comp :: (t -> t -> Bool) -> [t] -> Bool
comp cmp (x : y : ys) = cmp y x && comp cmp (y : ys)
comp _ _ = True

allMissing :: [t] -> [[t]]
allMissing (x : xs) = xs : map (x :) (allMissing xs)
allMissing [] = []

isSafe :: [Int] -> Bool
isSafe xs = (comp (<) xs || comp (>) xs) && comp (\x y -> abs (x - y) <= 3) xs

isSafeMissing :: [Int] -> Bool
isSafeMissing = any isSafe . allMissing
