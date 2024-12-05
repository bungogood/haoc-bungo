{-# LANGUAGE ExistentialQuantification #-}

module Util (currentYearDay, getInputPath, getPuzzlePath, getExamplePath) where

import Data.Time (getCurrentTime, toGregorian, utctDay)
import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))
import Text.Printf (printf)

-- Function to get the current year and day
currentYearDay :: IO (Int, Int)
currentYearDay = do
  (year, _, day) <- toGregorian . utctDay <$> getCurrentTime
  pure (fromIntegral year, day)

-- Function to generate the input file path
getInputPath :: Int -> Int -> IO FilePath
getInputPath year day = getDayYearPath year day "input"

-- Function to generate the input file path
getExamplePath :: Int -> Int -> IO FilePath
getExamplePath year day = getDayYearPath year day "example"

-- Function to grenerate the puzzle file path
getPuzzlePath :: Int -> Int -> IO FilePath
getPuzzlePath year day = getDayYearPath year day "puzzle"

getDayYearPath :: Int -> Int -> String -> IO FilePath
getDayYearPath year day dir = do
  currentDir <- getCurrentDirectory
  let dayStr = printf "%02d" day -- Format day as two digits
  return $ currentDir </> dir </> show year </> ("day" ++ dayStr) <.> "txt"
