{-# LANGUAGE TupleSections #-}

module Y2024.Day01 (Solution (..)) where

-- https://adventofcode.com/2024/day/1

import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . uncurry sumSortedDiffs . unzip . parse
  part2 _ = show . uncurry similarityScore . unzip . parse

parse :: String -> [(Int, Int)]
parse = mapMaybe (parseLine . words) . lines

parseLine :: [String] -> Maybe (Int, Int)
parseLine [x, y] = Just (read x, read y)
parseLine _ = Nothing

sumSortedDiffs :: [Int] -> [Int] -> Int
sumSortedDiffs xs ys = sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)

countOccurances :: [Int] -> Map.Map Int Int
countOccurances = Map.fromListWith (+) . map (,1)

similarityScore :: [Int] -> [Int] -> Int
similarityScore xs = sum . map (\y -> y * lookupVal y)
  where
    occurrences = countOccurances xs
    lookupVal y = Map.findWithDefault 0 y occurrences
