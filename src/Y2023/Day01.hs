module Y2023.Day01 (Solution (..)) where

-- https://adventofcode.com/2023/day/1

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (find, isPrefixOf)
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . mapMaybe (firstLast digit) . lines
  part2 _ = show . sum . mapMaybe (firstLast both) . lines

pairs :: [(String, Char)]
pairs =
  [ ("zero", '0'),
    ("one", '1'),
    ("two", '2'),
    ("three", '3'),
    ("four", '4'),
    ("five", '5'),
    ("six", '6'),
    ("seven", '7'),
    ("eight", '8'),
    ("nine", '9')
  ]

numPrefix :: String -> Maybe Char
numPrefix s = snd <$> find (flip isPrefixOf s . fst) pairs

findFirst :: ([a] -> Maybe b) -> [a] -> Maybe b
findFirst _ [] = Nothing
findFirst f (x : xs) = f (x : xs) <|> findFirst f xs

findLast :: ([a] -> Maybe b) -> [a] -> Maybe b
findLast f xs = walk f [] (reverse xs)

walk :: ([a] -> Maybe b) -> [a] -> [a] -> Maybe b
walk _ _ [] = Nothing
walk f acc (x : xs) = f (x : acc) <|> walk f (x : acc) xs

firstLast :: (String -> Maybe Char) -> String -> Maybe Int
firstLast f s = do
  firstDigit <- findFirst f s
  lastDigit <- findLast f s
  Just (read [firstDigit, lastDigit])

digit :: String -> Maybe Char
digit (x : _) | isDigit x = Just x
digit _ = Nothing

both :: String -> Maybe Char
both s = digit s <|> numPrefix s
