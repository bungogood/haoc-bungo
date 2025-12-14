{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Command.Cookie (writeCookie)
import Command.Download (download)
import Command.Parse (Command (..), parseCommand)
import Command.Run (run)
import Command.Setup (setup)

main :: IO ()
main = do
  command <- parseCommand
  case command of
    Setup {..} -> setup puzzleDay
    Download {..} -> download downloadType
    Run {..} -> run puzzleDay example input debug
    Cookie {..} -> writeCookie cookie
