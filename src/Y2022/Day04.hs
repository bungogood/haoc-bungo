module Y2022.Day04 (Solution (..)) where

-- https://adventofcode.com/2022/day/4

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . length . filter inside . readRanges
  part2 _ = show . length . filter overlap . readRanges

toTuple :: [a] -> Maybe (a, a)
toTuple [x, y] = Just (x, y)
toTuple _ = Nothing

toRange :: String -> Maybe (Int, Int)
toRange = toTuple . map read . splitOn "-"

readRanges :: String -> [((Int, Int), (Int, Int))]
readRanges = mapMaybe (toTuple . mapMaybe toRange . splitOn ",") . lines

inside :: ((Int, Int), (Int, Int)) -> Bool
inside ((a, b), (c, d)) = (a <= c && d <= b) || (c <= a && b <= d)

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a, b), (c, d)) = (c >= a || d >= a) && (b >= c || b >= d)
