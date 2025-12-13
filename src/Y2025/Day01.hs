module Y2025.Day01 (Solution (..)) where

-- https://adventofcode.com/2025/day/1

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . countOccurrences 100 50 0 . parse
  part2 _ = show . countPasses 100 50 0 . parse

data Direction = L | R
  deriving (Show, Eq)

stepOf :: Direction -> Int -> Int
stepOf L = negate
stepOf R = id

parse :: String -> [(Direction, Int)]
parse = mapMaybe parseLine . lines

parseLine :: String -> Maybe (Direction, Int)
parseLine [] = Nothing
parseLine (d : xs) = case d of
  'L' -> Just (L, read xs)
  'R' -> Just (R, read xs)
  _ -> Nothing

follow :: Int -> Int -> (Direction, Int) -> Int
follow modulus pos (dir, n) =
  (pos + stepOf dir n) `mod` modulus

countOccurrences :: Int -> Int -> Int -> [(Direction, Int)] -> Int
countOccurrences modulus start target =
  length . filter (== target) . scanl (follow modulus) start

countHits :: Int -> Int -> Int -> (Direction, Int) -> Int
countHits modulus pos target (dir, n)
  | first > n = 0
  | otherwise = 1 + (n - first) `div` modulus
  where
    r = stepOf dir (target - pos) `mod` modulus
    first = if r == 0 then modulus else r

followCountPasses :: Int -> Int -> Int -> (Direction, Int) -> (Int, Int)
followCountPasses modulus pos target (dir, n) =
  ( countHits modulus pos target (dir, n),
    (pos + stepOf dir n) `mod` modulus
  )

countPasses :: Int -> Int -> Int -> [(Direction, Int)] -> Int
countPasses modulus start target =
  fst . foldl' step (0, start)
  where
    step (total, pos) move =
      let (hits, pos') = followCountPasses modulus pos target move
       in (total + hits, pos')
