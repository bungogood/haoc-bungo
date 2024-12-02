{-# LANGUAGE ExistentialQuantification #-}

module Solver (Solver (..), getSolver, runSolver) where

import Lib
-- REPLACE_IMPORTS_START
-- REPLACE_IMPORTS_END

data Solver = forall a. (Solvable a) => Solver a

-- Run solution using the wrapper
runSolver :: Solver -> String -> (String, String)
runSolver (Solver solver) input = (part1 solver input, part2 solver input)

getSolver :: Int -> Int -> Solver
getSolver year day =
  case (year, day) of
-- REPLACE_CASES_START
-- REPLACE_CASES_END
    _ -> error "Invalid year or day"
