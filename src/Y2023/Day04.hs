module Y2023.Day04 (Solution (..)) where

-- https://adventofcode.com/2023/day/4

import Data.List (intersect)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib

data Solution = Solution

instance Solvable Solution where
  part1 _ = show . sum . map (dupsPow 2) . toCards
  part2 _ = show . countCards . toCards

type Card = (Int, [Int], [Int])

toCards :: String -> [Card]
toCards = mapMaybe toCard . lines

toCard :: String -> Maybe Card
toCard s = case splitOn ":" s of
  [game, rest] -> case splitOn "|" rest of
    [wn, bn] -> Just (gid, map read $ words wn, map read $ words bn)
      where
        gid = read . last . words $ game
    _ -> Nothing
  _ -> Nothing

dups :: Card -> [Int]
dups (_, wn, bn) = wn `intersect` bn

dupsPow :: Int -> Card -> Int
dupsPow base card = if count == 0 then 0 else base ^ (count - 1)
  where
    count = length $ dups card

decrement :: [(Int, Int)] -> [(Int, Int)]
decrement pairs = [(r - 1, o) | (r, o) <- pairs, r > 1]

countCards :: [Card] -> Int
countCards cards = fst . foldl cont (0, []) $ map (length . dups) cards
  where
    instances = (+ 1) . sum . map snd
    cont (acc, ls) n = (acc + instances ls, occs n ls)
    occs n os = if n > 0 then (n, instances os) : decrement os else decrement os
