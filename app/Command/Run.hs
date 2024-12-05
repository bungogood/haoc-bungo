module Command.Run (run) where

import Command.Parse (PuzzleDay (..))
import Solver (Solver, getSolver, runSolver)
import Util (currentYearDay, getExamplePath, getInputPath)

run :: PuzzleDay -> Bool -> Maybe FilePath -> IO ()
run (Specific year day) example input = do
  let solver = getSolver year day
  inputFile <- selectInput year day example input
  runSolverWithInput solver inputFile
run Today example input = do
  (year, day) <- currentYearDay
  run (Specific year day) example input

-- Function to determine the input file path
selectInput :: Int -> Int -> Bool -> Maybe FilePath -> IO FilePath
selectInput _ _ _ (Just path) = return path -- Use the provided file path
selectInput year day True _ = getExamplePath year day -- Use the example file path
selectInput year day _ Nothing = getInputPath year day -- Generate the default path

-- Function to run the solution with a solver and input file
runSolverWithInput :: Solver -> FilePath -> IO ()
runSolverWithInput solver inputFile = do
  fileContents <- readFile inputFile
  let (output1, output2) = runSolver solver fileContents
  putStr "Part 1:"
  putStrLn output1
  putStr "Part 2:"
  putStrLn output2
