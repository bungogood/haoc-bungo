{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Y2024.Day07 (Solution (..)) where

-- https://adventofcode.com/2024/day/7

import Data.Maybe (mapMaybe)
import Lib
import Text.Read (readMaybe)

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . mapMaybe (applier [Mul, Add]) . parse
  part2 _ = show . sum . mapMaybe (applier [Mul, Add, Con]) . parse

data Op = Mul | Add | Con deriving (Show, Eq)

parse :: String -> [(Int, [Int])]
parse = mapMaybe parseLine . lines

parseLine :: String -> Maybe (Int, [Int])
parseLine s = case mapMaybe (readMaybe . takeWhile (`elem` ['0' .. '9'])) (words s) of
  (x : xs) -> Just (x, xs)
  _ -> Nothing

combine :: Int -> Int -> Int
combine x y = read (show x ++ show y)

applyOp :: Op -> Int -> Int -> Int
applyOp Mul = (*)
applyOp Add = (+)
applyOp Con = combine

applier :: [Op] -> (Int, [Int]) -> Maybe Int
applier ops (t, xs) = applyLeft ops t xs

applyLeft :: [Op] -> Int -> [Int] -> Maybe Int
applyLeft _ t [x]
  | x == t = Just x
  | otherwise = Nothing
applyLeft ops t (x : y : xs) = go ops ops t x y xs

go :: [Op] -> [Op] -> Int -> Int -> Int -> [Int] -> Maybe Int
go _ [] _ _ _ _ = Nothing
go ops (o : os) t x y xs
  | next <= t = case applyLeft ops t (next : xs) of
      Just result -> Just result
      Nothing -> go ops os t x y xs
  | otherwise = go ops os t x y xs
  where
    next = applyOp o x y
