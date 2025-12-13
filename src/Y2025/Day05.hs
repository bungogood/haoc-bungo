module Y2025.Day05 (Solution (..)) where

-- https://adventofcode.com/2025/day/5

import Data.List (foldl', sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . uncurry countInRanges . parse
  part2 _ = show . countInAllRanges . fst . parse

parse :: String -> ([(Int, Int)], [Int])
parse s = (ranges, points)
  where
    ls = lines s
    (rangeLines, pointLines) = break null ls
    ranges = mapMaybe parseRange rangeLines
    points = map read (tail pointLines)

parseRange :: String -> Maybe (Int, Int)
parseRange s = case splitOn "-" s of
  [a, b] -> Just (read a, read b)
  _ -> Nothing

-- Part 1

isInRange :: (Int, Int) -> Int -> Bool
isInRange (a, b) x = x >= a && x <= b

inAnyRange :: [(Int, Int)] -> Int -> Bool
inAnyRange ranges x = any (`isInRange` x) ranges

countInRanges :: [(Int, Int)] -> [Int] -> Int
countInRanges ranges = length . filter (inAnyRange ranges)

-- Part 2

sortRanges :: [(Int, Int)] -> [(Int, Int)]
sortRanges = sortOn fst

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = foldl' go []
  where
    go [] r = [r]
    go ((pstart, pend) : rest) (start, end)
      | start <= pend = (pstart, max pend end) : rest
      | otherwise = (start, end) : (pstart, pend) : rest

rangeLength :: (Int, Int) -> Int
rangeLength (a, b) = b - a + 1

countInAllRanges :: [(Int, Int)] -> Int
countInAllRanges = sum . map rangeLength . mergeRanges . sortRanges
