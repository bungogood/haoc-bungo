{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

-- {-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Time
import Lib
import Options (Options (..), parseOptions)
import qualified Y2024.Day01
import qualified Y2024.Day02

-- Wrapper for any Solvable instance
data Solver = forall a. (Solvable a) => Solver a

-- Run solution using the wrapper
runSolution :: Solver -> String -> (String, String)
runSolution (Solver solution) input = (part1 solution input, part2 solution input)

-- Get the solution for a given year and day
getSolution :: Int -> Int -> Solver
getSolution year day =
  case (year, day) of
    (2024, 1) -> Solver Y2024.Day01.Solution
    (2024, 2) -> Solver Y2024.Day02.Solution
    _ -> error "Invalid year or day"

main :: IO ()
main = do
  -- Parse CLI options
  Options {year, day, input} <- parseOptions
  -- Get the solution for the given year and day
  let solution = getSolution year day
  let (output1, output2) = runSolution solution input
  putStrLn output1
  putStrLn output2
  currentDate <- getCurrentTime
  let (year, month, day) = toGregorian $ utctDay currentDate
  -- Print the current day and year
  putStrLn $ "Year: " ++ show year
  putStrLn $ "Day: " ++ show day
