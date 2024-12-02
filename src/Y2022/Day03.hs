module Y2022.Day03 (Solution (..)) where

-- https://adventofcode.com/2022/day/3

import Data.Char (isUpper, ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . mistakes . toRucksakes
  part2 _ = show . badges . toRucksakes

contains :: (Eq a) => [a] -> a -> Bool
contains = flip elem

duplicate :: (Eq a) => [a] -> [a] -> [a]
duplicate x = filter (contains x)

priority :: Char -> Int
priority c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

toRucksakes :: String -> [String]
toRucksakes = lines

splitBag :: String -> (String, String)
splitBag s = splitAt (div (length s) 2) s

mistakes :: [String] -> Int
mistakes = sum . map (priority . head . uncurry duplicate . splitBag)

common :: (Eq a) => [[a]] -> [a]
common [] = []
common (l : ls) = foldl intersect l ls

badges :: [String] -> Int
badges = sum . map (priority . head . common) . chunksOf 3
