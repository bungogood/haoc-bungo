{-# LANGUAGE ExistentialQuantification #-}

module Solver (Solver (..), getSolver, runSolver) where

import Lib
-- REPLACE_IMPORTS_START
import qualified Y2015.Day01
import qualified Y2015.Day02
import qualified Y2015.Day03
import qualified Y2015.Day04
import qualified Y2015.Day05
import qualified Y2015.Day06
import qualified Y2017.Day01
import qualified Y2018.Day01
import qualified Y2019.Day01
import qualified Y2021.Day01
import qualified Y2022.Day01
import qualified Y2022.Day02
import qualified Y2022.Day03
import qualified Y2022.Day04
import qualified Y2022.Day06
import qualified Y2023.Day01
import qualified Y2023.Day02
import qualified Y2023.Day03
import qualified Y2023.Day04
import qualified Y2023.Day05
import qualified Y2023.Day06
import qualified Y2024.Day01
import qualified Y2024.Day02
import qualified Y2024.Day03
import qualified Y2024.Day04
import qualified Y2024.Day05
import qualified Y2024.Day07
import qualified Y2025.Day01
import qualified Y2025.Day02
import qualified Y2025.Day03
import qualified Y2025.Day04
-- REPLACE_IMPORTS_END

data Solver = forall a. (Solvable a) => Solver a

-- Run solution using the wrapper
runSolver :: Solver -> String -> (String, String)
runSolver (Solver solver) input = (part1 solver input, part2 solver input)

getSolver :: Int -> Int -> Solver
getSolver year day =
  case (year, day) of
-- REPLACE_CASES_START
    (2015, 1) -> Solver Y2015.Day01.Solution
    (2015, 2) -> Solver Y2015.Day02.Solution
    (2015, 3) -> Solver Y2015.Day03.Solution
    (2015, 4) -> Solver Y2015.Day04.Solution
    (2015, 5) -> Solver Y2015.Day05.Solution
    (2015, 6) -> Solver Y2015.Day06.Solution
    (2017, 1) -> Solver Y2017.Day01.Solution
    (2018, 1) -> Solver Y2018.Day01.Solution
    (2019, 1) -> Solver Y2019.Day01.Solution
    (2021, 1) -> Solver Y2021.Day01.Solution
    (2022, 1) -> Solver Y2022.Day01.Solution
    (2022, 2) -> Solver Y2022.Day02.Solution
    (2022, 3) -> Solver Y2022.Day03.Solution
    (2022, 4) -> Solver Y2022.Day04.Solution
    (2022, 6) -> Solver Y2022.Day06.Solution
    (2023, 1) -> Solver Y2023.Day01.Solution
    (2023, 2) -> Solver Y2023.Day02.Solution
    (2023, 3) -> Solver Y2023.Day03.Solution
    (2023, 4) -> Solver Y2023.Day04.Solution
    (2023, 5) -> Solver Y2023.Day05.Solution
    (2023, 6) -> Solver Y2023.Day06.Solution
    (2024, 1) -> Solver Y2024.Day01.Solution
    (2024, 2) -> Solver Y2024.Day02.Solution
    (2024, 3) -> Solver Y2024.Day03.Solution
    (2024, 4) -> Solver Y2024.Day04.Solution
    (2024, 5) -> Solver Y2024.Day05.Solution
    (2024, 7) -> Solver Y2024.Day07.Solution
    (2025, 1) -> Solver Y2025.Day01.Solution
    (2025, 2) -> Solver Y2025.Day02.Solution
    (2025, 3) -> Solver Y2025.Day03.Solution
    (2025, 4) -> Solver Y2025.Day04.Solution
-- REPLACE_CASES_END
    _ -> error "Invalid year or day"
