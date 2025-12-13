module Y2025.Day02 (Solution (..)) where

-- https://adventofcode.com/2025/day/2

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . findInvalids invalidHalf . parse
  part2 _ = show . sum . findInvalids invalidAny . parse

parse :: String -> [(Int, Int)]
parse = mapMaybe parseRange . splitOn ","

parseRange :: String -> Maybe (Int, Int)
parseRange s = case splitOn "-" s of
  [a, b] -> Just (read a, read b)
  _ -> Nothing

iterateRanges :: [(Int, Int)] -> [Int]
iterateRanges = concatMap (\(a, b) -> [a .. b])

findInvalids :: (Int -> Bool) -> [(Int, Int)] -> [Int]
findInvalids p = filter p . iterateRanges

invalidHalf :: Int -> Bool
invalidHalf n = even (length str) && repeats str (length str `div` 2)
  where
    str = show n

invalidAny :: Int -> Bool
invalidAny n = any (repeats str) [1 .. length str `div` 2]
  where
    str = show n

repeats :: String -> Int -> Bool
repeats str len = all (== head chunks) chunks
  where
    chunks = takeWhile (not . null) $ map (take len) (iterate (drop len) str)
