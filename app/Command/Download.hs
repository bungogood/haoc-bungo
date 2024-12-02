module Command.Download (download) where

-- Import ExitCode

import Command.Parse (DownloadType (..), PuzzleDay (..))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Process (createProcess, proc, waitForProcess)
import Util (currentYearDay, getInputPath, getPuzzlePath)

download :: DownloadType -> IO ()
download (DownloadYear year) = downloadYear year
download (DownloadDay (Specific year day)) = downloadDay year day
download (DownloadDay Today) = do
  (year, day) <- currentYearDay
  download (DownloadDay (Specific year day))

downloadYear :: Int -> IO ()
downloadYear year = do
  mapM_ (downloadDay year) [1 .. 25]

downloadDay :: Int -> Int -> IO ()
downloadDay year day = do
  -- Get paths for input and puzzle files
  inputPath <- getInputPath year day
  puzzlePath <- getPuzzlePath year day

  -- Ensure directories exist
  createDirectoryIfMissing True (takeDirectory inputPath)
  createDirectoryIfMissing True (takeDirectory puzzlePath)

  -- Construct the arguments for the `aoc` command
  let aocCommand = "aoc"
      aocArgs =
        [ "download",
          "--quiet",
          "--overwrite",
          "--input-file",
          inputPath,
          "--puzzle-file",
          puzzlePath,
          "--day",
          show day,
          "--year",
          show year
        ]

  -- Run the process
  (_, _, _, processHandle) <- createProcess (proc aocCommand aocArgs)
  exitCode <- waitForProcess processHandle
  case exitCode of
    ExitSuccess -> putStrLn "Download complete!"
    ExitFailure n -> putStrLn $ "Error: aoc command failed with exit code " ++ show n
