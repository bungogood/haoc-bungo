{-# LANGUAGE ExistentialQuantification #-}

module Util (currentYearDay, getInputPath, getPuzzlePath) where

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
getInputPath year day = do
  currentDir <- getCurrentDirectory
  let dayStr = printf "%02d" day -- Format day as two digits
  return $ currentDir </> "input" </> show year </> ("day" ++ dayStr) <.> "txt"

-- Function to generate the puzzle file path
getPuzzlePath :: Int -> Int -> IO FilePath
getPuzzlePath year day = do
  currentDir <- getCurrentDirectory
  let dayStr = printf "%02d" day -- Format day as two digits
  return $ currentDir </> "puzzle" </> show year </> ("day" ++ dayStr) <.> "md"
