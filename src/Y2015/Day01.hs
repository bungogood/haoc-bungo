module Y2015.Day01 (Solution (..)) where

-- https://adventofcode.com/2015/day/1

import Data.List (find)
import Data.Maybe (fromJust)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . walk
  part2 _ = show . fromJust . basement

updateFloor :: Int -> Char -> Int
updateFloor f '(' = f + 1
updateFloor f ')' = f - 1
updateFloor f _ = f

walk :: String -> Int
walk = foldl updateFloor 0

basement :: String -> Maybe Int
basement = fmap (subtract 1 . fst) . find ((== -1) . snd) . zip [1 ..] . scanl updateFloor 0
