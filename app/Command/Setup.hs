module Command.Setup (setup) where

import Command.Parse (PuzzleDay (..))
import Data.List (sort)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath ((</>))
import Text.Printf (printf)
import Util (currentYearDay)

setup :: PuzzleDay -> IO ()
setup (Specific year day) = setupDay year day
setup Today = do
  (year, day) <- currentYearDay
  setup (Specific year day)

qualifiedName :: Int -> Int -> String
qualifiedName = printf "Y%04d.Day%02d"

-- Create `src/YXXXX/DayXX.hs` and insert module declaration
setupDay :: Int -> Int -> IO ()
setupDay year day = do
  let yearDir = printf "src/Y%04d" year
      dayFile = printf "Day%02d.hs" day
      filePath = yearDir </> dayFile

  -- Ensure the directory exists
  createDirectoryIfMissing True yearDir

  -- Check if the file already exists
  fileExists <- doesFileExist filePath
  if fileExists
    then putStrLn $ "File already exists: " ++ filePath
    else do
      -- Do wierd codegen in `app/Solver.hs` to generate the solver
      addSolver year day
      -- Write the template to the file
      writeFile filePath (generateModule (qualifiedName year day))
      putStrLn $ "Created: " ++ filePath

-- Generate the module content
generateModule :: String -> String
generateModule name =
  unlines
    [ "module " ++ name ++ " (Solution (..)) where",
      "",
      "import Lib",
      "",
      "data Solution = Solution",
      "",
      "instance Solvable Solution where",
      "  part1 _ = undefined",
      "  part2 _ = undefined",
      ""
    ]

-- Read and update the `app/Solver.hs` file
-- adding the new module to the import list and getSolver function
-- This is a bit hacky, but it works for now
addSolver :: Int -> Int -> IO ()
addSolver year day = do
  let originalFile = "app/Solver.hs"
      tempFile = "app/Solver.tmp"
  contents <- readFile originalFile
  let updatedContents = unlines $ processFile year day (lines contents)
  writeFile tempFile updatedContents
  renameFile tempFile originalFile -- Atomically replace the original file

-- Process the file to update sections
processFile :: Int -> Int -> [String] -> [String]
processFile year day linesOfFile =
  let (beforeImports, rest1) = break (== "-- REPLACE_IMPORTS_START") linesOfFile
      (importSection, afterImportsEnd) = span (/= "-- REPLACE_IMPORTS_END") (drop 1 rest1)
      updatedImports = updateImports year day importSection

      (beforeCases, rest2) = break (== "-- REPLACE_CASES_START") (drop 1 afterImportsEnd)
      (caseSection, afterCasesEnd) = span (/= "-- REPLACE_CASES_END") (drop 1 rest2)
      updatedCases = updateCases year day caseSection
   in beforeImports
        ++ ["-- REPLACE_IMPORTS_START"]
        ++ updatedImports
        ++ ["-- REPLACE_IMPORTS_END"]
        ++ beforeCases
        ++ ["-- REPLACE_CASES_START"]
        ++ updatedCases
        ++ ["-- REPLACE_CASES_END"]
        ++ drop 1 afterCasesEnd -- Skip duplicate markers

-- Update the import section
updateImports :: Int -> Int -> [String] -> [String]
updateImports year day imports =
  let newImport = "import qualified " ++ qualifiedName year day
      allImports = sort (newImport : imports)
   in allImports

-- Update the case section
updateCases :: Int -> Int -> [String] -> [String]
updateCases year day cases =
  let newCase =
        "    ("
          ++ show year
          ++ ", "
          ++ show day
          ++ ") -> Solver "
          ++ qualifiedName year day
          ++ ".Solution"
      allCases = sort (newCase : cases)
   in allCases
