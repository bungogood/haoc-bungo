module Y2025.Day04 (Solution (..)) where

-- https://adventofcode.com/2025/day/4

import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . countAllAdj '@' . lines
  part2 _ = show . removeAllRounds '@' . lines

allPositions :: [[Char]] -> [(Int, Int)]
allPositions grid = [(x, y) | y <- [0 .. length grid - 1], x <- [0 .. length (grid !! y) - 1]]

findAdj :: [[Char]] -> (Int, Int) -> [Char]
findAdj grid (x, y) = mapMaybe getValue positions
  where
    positions =
      [ (x - 1, y - 1),
        (x, y - 1),
        (x + 1, y - 1),
        (x - 1, y),
        (x + 1, y),
        (x - 1, y + 1),
        (x, y + 1),
        (x + 1, y + 1)
      ]
    getValue (i, j)
      | i < 0 || j < 0 = Nothing
      | j >= length grid = Nothing
      | i >= length (grid !! j) = Nothing
      | otherwise = Just ((grid !! j) !! i)

countAdj :: Char -> [[Char]] -> (Int, Int) -> Int
countAdj target grid = length . filter (== target) . findAdj grid

isValid :: [[Char]] -> Char -> (Int, Int) -> Bool
isValid grid target (x, y) = (cell == target) && countAdj target grid (x, y) < 4
  where
    cell = (grid !! y) !! x

countAllAdj :: Char -> [[Char]] -> Int
countAllAdj target grid = length $ filter (isValid grid target) (allPositions grid)

setGridValues :: [[Char]] -> [(Int, Int)] -> Char -> [[Char]]
setGridValues grid positions val = foldl setValue grid positions
  where
    setValue g (x, y) =
      take y g
        ++ [take x (g !! y) ++ [val] ++ drop (x + 1) (g !! y)]
        ++ drop (y + 1) g

countUpdate :: Char -> [[Char]] -> ([[Char]], Int)
countUpdate target grid = (grid', length positions)
  where
    grid' = setGridValues grid positions '.'
    positions = filter (isValid grid target) (allPositions grid)

removeAllRounds :: Char -> [[Char]] -> Int
removeAllRounds target = sum . takeWhile (> 0) . map (snd . countUpdate target) . iterate (fst . countUpdate target)
