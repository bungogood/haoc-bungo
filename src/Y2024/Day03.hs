module Y2024.Day03 (Solution (..)) where

-- https://adventofcode.com/2024/day/3

import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . parse 0
  part2 _ = show . parse' True 0

parse :: Int -> String -> Int
parse t ('m' : 'u' : 'l' : '(' : s) = parseFirstNum t s
parse t (_ : s) = parse t s
parse t [] = t

parseFirstNum :: Int -> String -> Int
parseFirstNum t s
  | length digits <= 3 && not (null digits) = parseComma t (read digits) rest
  | otherwise = parse t rest
  where
    (digits, rest) = span (`elem` ['0' .. '9']) s

parseComma :: Int -> Int -> String -> Int
parseComma t n (',' : s) = parseSecondNum t n s
parseComma t _ (_ : s) = parse t s
parseComma t _ [] = t

parseSecondNum :: Int -> Int -> String -> Int
parseSecondNum t n s
  | length digits <= 3 && not (null digits) = parseBracket t n (read digits) rest
  | otherwise = parse t rest
  where
    (digits, rest) = span (`elem` ['0' .. '9']) s

parseBracket :: Int -> Int -> Int -> String -> Int
parseBracket t n p (')' : s) = parse (t + n * p) s
parseBracket t _ _ (_ : s) = parse t s
parseBracket t _ _ [] = t

parse' :: Bool -> Int -> String -> Int
parse' _ t ('d' : 'o' : '(' : ')' : s) = parse' True t s
parse' _ t ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : s) = parse' False t s
parse' b t ('m' : 'u' : 'l' : '(' : s) = parseFirstNum' b t s
parse' b t (_ : s) = parse' b t s
parse' _ t [] = t

parseFirstNum' :: Bool -> Int -> String -> Int
parseFirstNum' b t s
  | length digits <= 3 && not (null digits) = parseComma' b t (read digits) rest
  | otherwise = parse' b t rest
  where
    (digits, rest) = span (`elem` ['0' .. '9']) s

parseComma' :: Bool -> Int -> Int -> String -> Int
parseComma' b t n (',' : s) = parseSecondNum' b t n s
parseComma' b t _ (_ : s) = parse' b t s
parseComma' _ t _ [] = t

parseSecondNum' :: Bool -> Int -> Int -> String -> Int
parseSecondNum' b t n s
  | length digits <= 3 && not (null digits) = parseBracket' b t n (read digits) rest
  | otherwise = parse' b t rest
  where
    (digits, rest) = span (`elem` ['0' .. '9']) s

parseBracket' :: Bool -> Int -> Int -> Int -> String -> Int
parseBracket' True t n p (')' : s) = parse' True (t + n * p) s
parseBracket' b t _ _ (_ : s) = parse' b t s
parseBracket' _ t _ _ [] = t
