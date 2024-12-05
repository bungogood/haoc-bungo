{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Y2024.Day05 (Solution (..)) where

-- https://adventofcode.com/2024/day/5

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . uncurry solve . parse
  part2 _ = show . uncurry solve' . parse

solve :: Map.Map Int [Int] -> [[Int]] -> Int
solve m = sum . map middleElement . filter (`verify` m)

solve' :: Map.Map Int [Int] -> [[Int]] -> Int
solve' m = sum . map (middleElement . flip reorder m) . filter (not . flip verify m)

middleElement :: [a] -> a
middleElement xs = xs !! (length xs `div` 2)

parse :: String -> (Map.Map Int [Int], [[Int]])
parse input = (m, t)
  where
    [a, b] = splitOn "\n\n" input
    m = genMap $ lines a
    t = map (map read . splitOn ",") $ lines b

genMap :: [String] -> Map.Map Int [Int]
genMap = foldl (\m (a, b) -> Map.insert a (b : fromMaybe [] (Map.lookup a m)) m) Map.empty . map (toPair . splitOn "|")
  where
    toPair [a, b] = (read a, read b)

verify :: [Int] -> Map.Map Int [Int] -> Bool
verify (x : xs) m = not (any (elem x . fromMaybe [] . flip Map.lookup m) xs) && verify xs m
verify _ _ = True

-- find x that doesn't have a value in the map
reorder :: [Int] -> Map.Map Int [Int] -> [Int]
reorder [] _ = []
reorder xs m = least : reorder (filter (/= least) xs) m
  where
    least = head $ filter (\x -> not $ any (elem x . fromMaybe [] . flip Map.lookup m) xs) xs
