module Y2015.Day03 (Solution (..)) where

-- https://adventofcode.com/2015/day/3

import qualified Data.Set as Set
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . Set.size . walk (0, 0)
  part2 _ c = show . Set.size $ walk (0, 0) santa `Set.union` walk (0, 0) robo
    where
      (santa, robo) = splitAlt c

step :: (Int, Int) -> Char -> (Int, Int)
step (x, y) '>' = (x + 1, y)
step (x, y) '<' = (x - 1, y)
step (x, y) '^' = (x, y + 1)
step (x, y) 'v' = (x, y - 1)
step p _ = p

walk :: (Int, Int) -> String -> Set.Set (Int, Int)
walk p = walkRec p (Set.singleton p)

walkRec :: (Int, Int) -> Set.Set (Int, Int) -> String -> Set.Set (Int, Int)
walkRec p v (d : ds) = walkRec (step p d) (Set.insert (step p d) v) ds
walkRec _ v [] = v

splitAlt :: String -> (String, String)
splitAlt s = (first, second)
  where
    first = [c | (i, c) <- zip [0 ..] s, even i]
    second = [c | (i, c) <- zip [0 ..] s, odd i]
