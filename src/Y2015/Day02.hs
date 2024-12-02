module Y2015.Day02 (Solution (..)) where

-- https://adventofcode.com/2015/day/2

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . map wrapping . toBoxes
  part2 _ = show . sum . map ribbon . toBoxes

toBoxes :: String -> [(Int, Int, Int)]
toBoxes = mapMaybe (toBox . splitOn "x") . lines

toBox :: [String] -> Maybe (Int, Int, Int)
toBox [l, w, h] = Just (read l, read w, read h)
toBox _ = Nothing

wrapping :: (Int, Int, Int) -> Int
wrapping (l, w, h) = 2 * l * w + 2 * w * h + 2 * h * l + minimum [l * w, w * h, h * l]

ribbon :: (Int, Int, Int) -> Int
ribbon (l, w, h) = minimum [2 * l + 2 * w, 2 * w + 2 * h, 2 * h + 2 * l] + l * w * h
