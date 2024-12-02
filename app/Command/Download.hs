module Command.Download (download) where

-- Import ExitCode

import Command.Parse (DownloadType (DownloadDay, DownloadToday, DownloadYear))
import System.Exit (ExitCode (..))
import System.Process (createProcess, proc, waitForProcess)
import Util (currentYearDay, getInputPath, getPuzzlePath)

download :: DownloadType -> IO ()
download (DownloadYear year) = downloadYear year
download (DownloadDay year day) = downloadDay year day
download DownloadToday = do
  (year, day) <- currentYearDay
  download (DownloadDay year day)

downloadYear :: Int -> IO ()
downloadYear year = do
  mapM_ (downloadDay year) [1 .. 25]

downloadDay :: Int -> Int -> IO ()
downloadDay year day = do
  -- Get paths for input and puzzle files
  inputPath <- getInputPath year day
  puzzlePath <- getPuzzlePath year day

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
