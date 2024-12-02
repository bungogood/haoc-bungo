module Y2018.Day01 (Solution (..)) where

-- https://adventofcode.com/2018/day/1

import qualified Data.Set as Set
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . map toChange . lines
  part2 _ = show . repeated 0 Set.empty [] . map toChange . lines

toChange :: String -> Int
toChange ('+' : xs) = read xs
toChange ('-' : xs) = -read xs
toChange _ = error "Invalid input"

repeated :: Int -> Set.Set Int -> [Int] -> [Int] -> Int
repeated value seen [] ls = repeated value seen ls ls
repeated value seen (c : cs) ls
  | Set.member value seen = value
  | otherwise = repeated (value + c) (Set.insert value seen) cs ls
