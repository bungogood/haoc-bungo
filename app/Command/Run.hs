module Command.Run (run) where

import Command.Parse (RunType (RunDay, RunToday))
import Solver (getSolver, runSolver)
import Util (currentYearDay, getInputPath)

run :: RunType -> Maybe FilePath -> IO ()
run (RunDay year day) input = runDay year day input
run RunToday input = do
  (year, day) <- currentYearDay
  run (RunDay year day) input

-- Function to determine the input file path
selectInput :: Int -> Int -> Maybe FilePath -> IO FilePath
selectInput _ _ (Just path) = return path -- Use the provided file path
selectInput year day Nothing = getInputPath year day -- Generate the path

-- Function to run the solution for a specific year and day
runDay :: Int -> Int -> Maybe FilePath -> IO ()
runDay year day input = do
  inputFile <- selectInput year day input
  fileContents <- readFile inputFile

  let solver = getSolver year day
  let (output1, output2) = runSolver solver fileContents

  putStr "Part 1: "
  putStrLn output1
  putStr "Part 2: "
  putStrLn output2