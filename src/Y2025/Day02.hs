module Y2025.Day02 (Solution (..)) where

-- https://adventofcode.com/2025/day/2

import Data.ByteString (find)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . findInvalids1 . parse
  part2 _ = show . sum . findInvalids . parse

parse :: String -> [(Int, Int)]
parse = mapMaybe parseRange . splitOn ","

parseRange :: String -> Maybe (Int, Int)
parseRange s = case splitOn "-" s of
  [a, b] -> Just (read a, read b)
  _ -> Nothing

iterateRanges :: [(Int, Int)] -> [Int]
iterateRanges = concatMap (\(a, b) -> [a .. b])

findInvalids1 :: [(Int, Int)] -> [Int]
findInvalids1 = filter isInvalid1 . iterateRanges

isInvalid1 :: Int -> Bool
isInvalid1 n = anyRepeatLen1 (show n)

anyRepeatLen1 :: String -> Bool
anyRepeatLen1 str = even (length str) && repeatLen str (length str `div` 2)

findInvalids :: [(Int, Int)] -> [Int]
findInvalids = filter isInvalid . iterateRanges

isInvalid :: Int -> Bool
isInvalid n = anyRepeatLen (show n)

anyRepeatLen :: String -> Bool
anyRepeatLen str = any (repeatLen str) [1 .. length str `div` 2]

repeatLen :: String -> Int -> Bool
repeatLen str len = all (== head chunks) chunks
  where
    chunks = takeWhile (not . null) $ map (take len) (iterate (drop len) str)
