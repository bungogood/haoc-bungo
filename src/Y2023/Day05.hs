module Y2023.Day05 (Solution (..)) where

-- https://adventofcode.com/2023/day/5

import Data.Bifunctor (first)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ contents =
    let (seeds, order, mapping) = parseInput contents
     in show . best mapping order "seed" $ toSingles seeds

  part2 _ contents =
    let (seeds, order, mapping) = parseInput contents
     in show . best mapping order "seed" $ toPairs seeds

parseInput :: String -> ([Int], Map String String, Map String [Inter])
parseInput s = (seeds, order, mapping)
  where
    (seeds, input) = parse s
    order = toOrder input
    mapping = createMap input

type Inter = (Int, Int, Int)

toInter :: [String] -> [Inter]
toInter = mapMaybe (interval . map read . words)
  where
    interval [d, s, l] = Just (d, s, s + l - 1)
    interval _ = Nothing

splitTop :: String -> Maybe (String, String)
splitTop s = case words s of
  (word : _) -> case splitOn "-to-" word of
    [src, dest] -> Just (src, dest)
    _ -> Nothing
  _ -> Nothing

parse :: String -> ([Int], [((String, String), [Inter])])
parse s = case splitOn "\n\n" s of
  (header : sections) ->
    let seeds = case splitOn ":" header of
          [_, sss] -> map read $ words sss
          _ -> []
        ls = mapMaybe parseSection sections
     in (seeds, ls)
  _ -> ([], []) -- Return empty values for invalid input
  where
    parseSection :: String -> Maybe ((String, String), [Inter])
    parseSection section = case lines section of
      (top : rest) -> do
        srcDest <- splitTop top
        let inters = toInter rest
        Just (srcDest, inters)
      _ -> Nothing

createMap :: [((String, String), [Inter])] -> Map String [Inter]
createMap = Map.fromList . map (Data.Bifunctor.first snd)

toOrder :: [((String, String), [Inter])] -> Map String String
toOrder = Map.fromList . map fst

breakup :: Inter -> (Int, Int) -> [(Int, Int)]
breakup (_, s, e) (a, b)
  | a < s && b < s || a > e && b > e = [(a, b)]
  | otherwise = start ++ mid ++ end
  where
    start = [(a, s - 1) | a < s]
    mid = [(max a s, min b e)]
    end = [(e + 1, b) | b > e]

mover :: [Inter] -> [(Int, Int)] -> [(Int, Int)]
mover is = foldl (\rs' r -> move is r : rs') []

move :: [Inter] -> (Int, Int) -> (Int, Int)
move [] (a, b) = (a, b)
move (i : is) (a, b)
  | a < s && b < s || a > e && b > e = move is (a, b)
  | otherwise = (a - s + d, b - s + d)
  where
    (d, s, e) = i

nextRanges :: [Inter] -> [(Int, Int)] -> [(Int, Int)]
nextRanges is rs = mover is $ foldl (\a i -> concatMap (breakup i) a) rs is

findResults :: Map String [Inter] -> Map String String -> String -> [(Int, Int)] -> [(Int, Int)]
findResults mi ms k vs = case Map.lookup k ms of
  Nothing -> vs
  Just next -> findResults mi ms next $ nextRanges (mi Map.! next) vs

toPairs :: [Int] -> [(Int, Int)]
toPairs (s : l : rest) = (s, s + l - 1) : toPairs rest
toPairs _ = []

toSingles :: [Int] -> [(Int, Int)]
toSingles = map (\x -> (x, x))

best :: Map String [Inter] -> Map String String -> String -> [(Int, Int)] -> Int
best mi ms k = minimum . map fst . findResults mi ms k
