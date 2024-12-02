module Command.Parse (Command (..), parseCommand, DownloadType (..), RunType (..)) where

import Options.Applicative

-- Define the Command data type
data Command
  = Setup {year :: Maybe Int, day :: Maybe Int}
  | Download {downloadType :: DownloadType}
  | Run {runType :: RunType, input :: Maybe FilePath}
  | Cookie {cookie :: String}
  deriving (Show)

-- Parser for "setup" subcommand
setupParser :: Parser Command
setupParser =
  Setup
    <$> optional
      ( option
          auto
          ( long "year"
              <> short 'y'
              <> metavar "YEAR"
              <> help "Specify the year (default: current year)"
          )
      )
    <*> optional
      ( option
          auto
          ( long "day"
              <> short 'd'
              <> metavar "DAY"
              <> help "Specify the day (default: current day)"
          )
      )

data DownloadType
  = DownloadToday -- No arguments, download today's puzzle
  | DownloadYear Int -- Explicit --full flag with a year
  | DownloadDay Int Int -- Specific year and day
  deriving (Show)

-- Parser for "download" subcommand
downloadParser :: Parser Command
downloadParser =
  Download
    <$> ( DownloadYear
            <$> option
              auto
              ( long "full"
                  <> help "Download all puzzles for the specified year"
                  <> metavar "YEAR"
              )
              <|> ( DownloadDay
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
              <|> pure DownloadToday
        )

data RunType
  = RunToday -- No arguments, run today's puzzle
  | RunDay Int Int -- Specific year and day
  deriving (Show)

-- Parser for "run" subcommand
runParser :: Parser Command
runParser =
  Run
    <$> (runTodayParser <|> runDayParser)
    <*> optional
      ( strOption
          ( long "input"
              <> short 'i'
              <> metavar "FILE"
              <> help "Specify the input file (optional)"
          )
      )

-- Parser for running today's puzzle
runTodayParser :: Parser RunType
runTodayParser = pure RunToday

-- Parser for running a specific year and day
runDayParser :: Parser RunType
runDayParser =
  RunDay
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
