module Y2015.Day06 (Solution (..)) where

-- https://adventofcode.com/2015/day/6

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . runner (1000, 1000) . map toInstr . lines
  part2 _ = show . runner' (1000, 1000) . map toInstr . lines

data Op = Turn Bool | Toggle deriving (Show, Eq)

type Instr = (Op, Coord, Coord)

type Coord = (Int, Int)

toInstr :: String -> Instr
toInstr s = (op, (read x1, read y1), (read x2, read y2))
  where
    wordsList = words s
    op
      | "turn on" `isInfixOf` s = Turn True
      | "turn off" `isInfixOf` s = Turn False
      | otherwise = Toggle
    [x1, y1] = splitOn "," (wordsList !! (length wordsList - 3))
    [x2, y2] = splitOn "," (last wordsList)

inside :: Coord -> Coord -> Coord -> Bool
inside (tx, ty) (bx, by) (x, y) = x >= tx && x <= bx && y >= ty && y <= by

checker :: Coord -> [Instr] -> Bool
checker _ [] = False
checker coord ((op, start, end) : instrs)
  | inside start end coord = case op of
      Turn v -> v
      Toggle -> not (checker coord instrs)
  | otherwise = checker coord instrs

runner :: Coord -> [Instr] -> Int
runner (x, y) instrs = foldl (\acc coord -> if checker coord reved then acc + 1 else acc) 0 coords
  where
    coords = [(x', y') | x' <- [0 .. x], y' <- [0 .. y]]
    reved = reverse instrs

checker' :: Coord -> [Instr] -> Int
checker' _ [] = 0
checker' coord ((op, start, end) : instrs)
  | inside start end coord = case op of
      Turn True -> checker' coord instrs + 1
      Turn False -> max (checker' coord instrs - 1) 0
      Toggle -> checker' coord instrs + 2
  | otherwise = checker' coord instrs

runner' :: Coord -> [Instr] -> Int
runner' (x, y) instrs = foldl (\acc coord -> acc + checker' coord reved) 0 coords
  where
    coords = [(x', y') | x' <- [0 .. x], y' <- [0 .. y]]
    reved = reverse instrs
