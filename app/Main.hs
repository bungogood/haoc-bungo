{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Command.Cookie (writeCookie)
import Command.Download (download)
import Command.Parse (Command (..), parseCommand)
import Command.Run (run)

main :: IO ()
main = do
  command <- parseCommand
  case command of
    Setup {..} -> putStrLn $ "Setup for year: " ++ show year ++ ", day: " ++ show day
    Download {..} -> download downloadType
    Run {..} -> run runType input
    Cookie {..} -> writeCookie cookie
