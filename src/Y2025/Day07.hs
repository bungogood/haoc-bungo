module Y2025.Day07 (Solution (..)) where

-- https://adventofcode.com/2025/day/7

-- See nice visualization at https://www.reddit.com/r/adventofcode/comments/1pgnmou/2025_day_7_lets_visualize/

import Control.Monad (foldM, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Debug.Trace (traceShowM)
import Lib

data Solution = Solution

debug :: Bool
debug = False

instance Solvable Solution where
  part1 _ = show . fst . uncurry processRows . parse
  part2 _ = show . sum . snd . uncurry processRows . parse

parse :: String -> ((Int, V.Vector Int), [String])
parse s = ((0, state), tail ls)
  where
    ls = lines s
    state = V.fromList $ map toStart (head ls)

toStart :: Char -> Int
toStart 'S' = 1
toStart _ = 0

processRows :: (Int, V.Vector Int) -> [String] -> (Int, V.Vector Int)
processRows (icnt, ivec) rows =
  runST $ do
    mv <- V.thaw ivec
    cnt <- foldM (processRow mv) icnt rows
    vec' <- V.freeze mv
    pure (cnt, vec')

processRow :: MV.MVector s Int -> Int -> String -> ST s Int
processRow mv icnt row = do
  cnt <- applyRow mv row
  when debug $ do
    vec <- V.freeze mv
    traceShowM vec
  pure (icnt + cnt)

applyRow :: MV.MVector s Int -> String -> ST s Int
applyRow mv row =
  foldM step 0 (zip [0 ..] row)
  where
    step acc (i, '^') = do
      v <- MV.read mv i
      when (v > 0) (updateNeighbors mv i)
      pure (acc + fromEnum (v > 0))
    step acc _ = pure acc

updateNeighbors :: MV.MVector s Int -> Int -> ST s ()
updateNeighbors mv i = do
  v <- MV.read mv i
  MV.write mv i 0
  when (i > 0) $ MV.modify mv (+ v) (i - 1)
  when (i < MV.length mv - 1) $ MV.modify mv (+ v) (i + 1)
