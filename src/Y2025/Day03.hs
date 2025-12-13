module Y2025.Day03 (Solution (..)) where

-- https://adventofcode.com/2025/day/3

import Data.Ord (comparing)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . map (joltInt . findJoltN 2) . lines
  part2 _ = show . sum . map (joltInt . findJoltN 12) . lines

joltInt :: [Char] -> Int
joltInt = read

findJoltN :: Int -> [Char] -> [Char]
findJoltN 0 _ = []
findJoltN n line = c : findJoltN (n - 1) rest
  where
    (idx, c) = maxWithIndex $ dropEnd (n - 1) line
    rest = drop (idx + 1) line

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs

maxWithIndex :: (Ord a) => [a] -> (Int, a)
maxWithIndex = firstMaxBy snd . zip [0 ..]

firstMaxBy :: (Ord b, Foldable t) => (a -> b) -> t a -> a
firstMaxBy f = foldl1 step
  where
    step x y = case comparing f x y of
      GT -> x
      EQ -> x
      LT -> y
