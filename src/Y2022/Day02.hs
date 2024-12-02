module Y2022.Day02 (Solution (..)) where

-- https://adventofcode.com/2022/day/2

import Data.List.Split (splitOn)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . points . map symbolMap . toGame
  part2 _ = show . points . map outcomeMap . toGame

data Symbol = Rock | Paper | Scissors deriving (Eq, Show)

beats :: Symbol -> Symbol
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

losses :: Symbol -> Symbol
losses = beats . beats

shapePoints :: Symbol -> Int
shapePoints Rock = 1
shapePoints Paper = 2
shapePoints Scissors = 3

outcomePoints :: Symbol -> Symbol -> Int
outcomePoints x y
  | beats x == y = 6 -- win
  | x == beats y = 0 -- loss
  | otherwise = 3 -- draw

game :: Symbol -> Symbol -> Int
game x y = outcomePoints x y + shapePoints y

points :: [(Symbol, Symbol)] -> Int
points = sum . map (uncurry game)

mapSymbol :: Char -> Symbol
mapSymbol 'X' = Rock
mapSymbol 'Y' = Paper
mapSymbol 'Z' = Scissors
mapSymbol 'A' = Rock
mapSymbol 'B' = Paper
mapSymbol 'C' = Scissors
mapSymbol _ = error "Invalid symbol"

symbolMap :: (Symbol, Char) -> (Symbol, Symbol)
symbolMap (x, y) = (x, mapSymbol y)

outcomeMap :: (Symbol, Char) -> (Symbol, Symbol)
outcomeMap (x, 'X') = (x, losses x)
outcomeMap (x, 'Y') = (x, x)
outcomeMap (x, 'Z') = (x, beats x)
outcomeMap _ = error "Invalid outcome"

toGame :: String -> [(Symbol, Char)]
toGame = map (tuplify . map head . splitOn " ") . lines
  where
    tuplify :: [Char] -> (Symbol, Char)
    tuplify [x, y] = (mapSymbol x, y)
    tuplify _ = error "Invalid tuple"
