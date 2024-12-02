module Y2023.Day02 (Solution (..)) where

-- https://adventofcode.com/2023/day/2

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . allUnderSumID (Map.fromList [("red", 12), ("green", 13), ("blue", 14)]) . parse
  part2 _ = show . sumProduct . parse

type Game = (Int, [Round])

type Round = [(String, Int)]

parse :: String -> [Game]
parse = map toGame . lines

toGame :: String -> Game
toGame vs =
  let [game, rest] = splitOn ":" vs
      gid = read . last . words
      rounds = map (map toPair . splitOn ",") . splitOn ";"
      toPair s = let [count, cube] = words s in (cube, read count)
   in (gid game, rounds rest)

under :: Map.Map String Int -> Map.Map String Int -> Bool
under limits = all (\(cube, count) -> count <= Map.findWithDefault 0 cube limits) . Map.toList

most :: [Round] -> Map.Map String Int
most = foldl' (foldl' (\acc (cube, count) -> Map.insertWith max cube count acc)) Map.empty

allUnderSumID :: Map.Map String Int -> [Game] -> Int
allUnderSumID limits = sum . map (\(gid, rounds) -> if under limits (most rounds) then gid else 0)

sumProduct :: [Game] -> Int
sumProduct = sum . map (product . Map.elems . most . snd)
