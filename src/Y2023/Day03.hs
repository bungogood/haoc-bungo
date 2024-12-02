module Y2023.Day03 (Solution (..)) where

-- https://adventofcode.com/2023/day/3
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ contents =
    let (symbols, numbers) = parseInput contents
     in show $ validNumberSum symbols numbers

  part2 _ contents =
    let (symbols, numbers) = parseInput contents
     in show $ productNearSymbol numbers '*' symbols

type Coord = (Int, Int)

data Item = Symbol Char | Number String deriving (Show, Eq)

parseInput :: String -> (Map Coord Char, [([Coord], Int)])
parseInput = splitter . toItems . lines

toNumber :: Coord -> String -> [(Coord, Item)]
toNumber (x, y) rev = [((x - length rev, y), Number (reverse rev))]

toItems :: [String] -> [(Coord, Item)]
toItems (r : rs) = toItemsRec (0, 0) [] r rs
toItems [] = []

toItemsRec :: Coord -> String -> String -> [String] -> [(Coord, Item)]
toItemsRec (x, y) acc [] [] = toNumber (x, y) acc
toItemsRec (x, y) acc [] (r : rs) = toNumber (x, y) acc ++ toItemsRec (0, y + 1) [] r rs
toItemsRec (x, y) acc (c : cs) rs
  | c == '.' = remaining
  | isDigit c = toItemsRec (x + 1, y) (c : acc) cs rs
  | otherwise = ((x, y), Symbol c) : remaining
  where
    remaining = toNumber (x, y) acc ++ toItemsRec (x + 1, y) [] cs rs

splitter :: [(Coord, Item)] -> (Map Coord Char, [([Coord], Int)])
splitter =
  foldl
    ( \(sym, nums) item -> case item of
        ((x, y), Symbol c) -> (Map.insert (x, y) c sym, nums)
        ((x, y), Number s) ->
          let coords = [(x', y) | x' <- [x .. x + length s - 1]]
           in (sym, (coords, read s) : nums)
    )
    (Map.empty, [])

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]

isHitSym :: Map Coord Char -> ([Coord], Int) -> Bool
isHitSym sym = any symAdj . fst
  where
    symAdj = any (`Map.member` sym) . neighbours

validNumberSum :: Map Coord Char -> [([Coord], Int)] -> Int
validNumberSum sym = sum . map snd . filter (isHitSym sym)

charCoords :: Char -> Map Coord Char -> [Coord]
charCoords c = Map.keys . Map.filter (== c)

numNearProd :: [([Coord], Int)] -> Coord -> Int
numNearProd nums tar = case map snd $ filter (isAdj . fst) nums of
  [a, b] -> a * b
  _ -> 0
  where
    isAdj coords = any (`elem` coords) (neighbours tar)

productNearSymbol :: [([Coord], Int)] -> Char -> Map Coord Char -> Int
productNearSymbol cs c = sum . map (numNearProd cs) . charCoords c
