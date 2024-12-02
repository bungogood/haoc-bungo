module Command.Setup (setup) where

import Command.Parse (PuzzleDay (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)
import Util (currentYearDay)

setup :: PuzzleDay -> IO ()
setup (Specific year day) = setupDay year day
setup Today = do
  (year, day) <- currentYearDay
  setup (Specific year day)

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
      -- Write the template to the file
      writeFile filePath (generateModule year day)
      -- Do wierd codegen in `app/Solver.hs` to generate the solver
      putStrLn $ "Created: " ++ filePath

-- Generate the module content
generateModule :: Int -> Int -> String
generateModule year day =
  let yearModule = printf "Y%04d" year
      dayModule = printf "Day%02d" day
   in unlines
        [ "module " ++ yearModule ++ "." ++ dayModule ++ " (Solution (..)) where",
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
