module Command.Parse (Command (..), parseCommand, DownloadType (..), PuzzleDay (..)) where

import Options.Applicative

-- Define the Command data type
data Command
  = Setup {puzzleDay :: PuzzleDay}
  | Download {downloadType :: DownloadType}
  | Run {puzzleDay :: PuzzleDay, example :: Bool, input :: Maybe FilePath, debug :: Bool}
  | Cookie {cookie :: String}
  deriving (Show)

data PuzzleDay = Today | Specific Int Int deriving (Show)

puzzleDayParser :: Parser PuzzleDay
puzzleDayParser =
  pure Today
    <|> ( Specific
            <$> argument
              auto
              ( metavar "YEAR"
                  <> help "Specify the year (e.g., 2024)"
              )
            <*> argument
              auto
              ( metavar "DAY"
                  <> help "Specify the day (e.g., 04)"
              )
        )

-- Parser for "setup" subcommand
setupParser :: Parser Command
setupParser = Setup <$> puzzleDayParser

data DownloadType
  = DownloadDay PuzzleDay -- No arguments, download today's puzzle
  | DownloadYear Int -- Explicit --full flag with a year
  deriving (Show)

-- Parser for "download" subcommand
downloadParser :: Parser Command
downloadParser =
  Download
    <$> ( ( DownloadYear
              <$> option
                auto
                ( long "full"
                    <> help "Download all puzzles for the specified year"
                    <> metavar "YEAR"
                )
          )
            <|> (DownloadDay <$> puzzleDayParser)
        )

-- Parser for "run" subcommand
runParser :: Parser Command
runParser =
  Run
    <$> puzzleDayParser
    <*> switch
      ( long "example"
          <> short 'e'
          <> help "Run the example input"
      )
    <*> optional
      ( strOption
          ( long "input"
              <> short 'i'
              <> metavar "FILE"
              <> help "Specify the input file (optional)"
          )
      )
    <*> switch
      ( long "debug"
          <> short 'd'
          <> help "Enable debug output for the solution"
      )

cookieParser :: Parser Command
cookieParser =
  Cookie
    <$> strArgument
      ( metavar "COOKIE"
          <> help "Specify the session cookie"
      )

-- Top-level parser for all commands
commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "setup" (info setupParser (progDesc "Setup environment for a specific year and day"))
        <> command "download" (info downloadParser (progDesc "Download input for a specific year and day"))
        <> command "run" (info runParser (progDesc "Run a solution for a specific year and day"))
        <> command "cookie" (info cookieParser (progDesc "Set the session cookie"))
    )

-- Entry point for parsing commands
parseCommand :: IO Command
parseCommand = execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Advent of Code CLI tool"
            <> header "haoc - a tool for solving Advent of Code challenges"
        )
