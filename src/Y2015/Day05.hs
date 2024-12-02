module Y2015.Day05 (Solution (..)) where

-- https://adventofcode.com/2015/day/5

import Data.List (isInfixOf)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . length . filter niceFirst . lines
  part2 _ = show . length . filter niceSecond . lines

vowels :: String -> Bool
vowels s = length (filter (`elem` "aeiou") s) >= 3

double :: String -> Bool
double s = any (uncurry (==)) $ zip s (tail s)

bad :: String -> Bool
bad s = any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]

niceFirst :: String -> Bool
niceFirst s = vowels s && double s && not (bad s)

pair :: String -> Bool
pair (x : y : xs) = [x, y] `isInfixOf` xs || pair (y : xs)
pair _ = False

repeated :: String -> Bool
repeated (x : y : z : xs) = x == z || repeated (y : z : xs)
repeated _ = False

niceSecond :: String -> Bool
niceSecond s = pair s && repeated s
