{-# LANGUAGE ExistentialQuantification #-}

module Solver (Solver (..), getSolver, runSolver) where

import Lib
-- REPLACE_IMPORTS_START
import qualified Y2024.Day01
import qualified Y2024.Day02
-- REPLACE_IMPORTS_END

data Solver = forall a. (Solvable a) => Solver a

-- Run solution using the wrapper
runSolver :: Solver -> String -> (String, String)
runSolver (Solver solver) input = (part1 solver input, part2 solver input)

getSolver :: Int -> Int -> Solver
getSolver year day =
  case (year, day) of
-- REPLACE_CASES_START
    (2024, 1) -> Solver Y2024.Day01.Solution
    (2024, 2) -> Solver Y2024.Day02.Solution
-- REPLACE_CASES_END
    _ -> error "Invalid year or day"
