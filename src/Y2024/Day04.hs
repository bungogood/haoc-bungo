module Y2024.Day04 (Solution (..)) where

-- https://adventofcode.com/2024/day/4

import Data.List (isPrefixOf, transpose)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . wordsearch "XMAS" . lines
  part2 _ = show . masAll 'A' ('M', 'S') . lines

countOcc :: [Char] -> [Char] -> Int
countOcc _ [] = 0
countOcc target input
  | target `isPrefixOf` input = 1 + countOcc target (drop (length target) input)
  | otherwise = countOcc target (tail input)

countAll :: [Char] -> [[Char]] -> Int
countAll target = sum . map (countOcc target)

wordsearch :: [Char] -> [[Char]] -> Int
wordsearch target input = countAll target input + countAll target (map reverse input) + countAll target (transpose input) + countAll target (map reverse (transpose input)) + countAll target (toDiag input) + countAll target (toDiag (reverse input)) + countAll target (map reverse (toDiag input)) + countAll target (map reverse (toDiag (reverse input)))

toDiag :: [[Char]] -> [[Char]]
toDiag input = reverse (strip $ transpose $ tail input) ++ strip input
  where
    strip = transpose . zipWith drop [0 ..]

masAll :: Char -> (Char, Char) -> [[Char]] -> Int
masAll c p input =
  sum $
    [ mas c p input (x, y)
      | x <- [1 .. length (head input) - 2],
        y <- [1 .. length input - 2]
    ]

cross :: [[Char]] -> (Int, Int) -> (Char, Char, Char, Char) -> Bool
cross input (x, y) (a, b, c, d) = input !! (y - 1) !! (x - 1) == a && input !! (y + 1) !! (x + 1) == b && input !! (y - 1) !! (x + 1) == c && input !! (y + 1) !! (x - 1) == d

mas :: Char -> (Char, Char) -> [[Char]] -> (Int, Int) -> Int
mas c (a, b) input (x, y)
  | input !! y !! x == c && (cross input (x, y) (a, b, a, b) || cross input (x, y) (b, a, b, a) || cross input (x, y) (a, b, b, a) || cross input (x, y) (b, a, a, b)) = 1
  | otherwise = 0
