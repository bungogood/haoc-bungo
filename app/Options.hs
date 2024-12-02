module Options (Options(..), parseOptions) where

import Options.Applicative

-- Define a data type to hold the options
data Options = Options
  { year  :: Int
  , day   :: Int
  , input :: String
  }

-- Define the parser
optionsParser :: Parser Options
optionsParser = Options
  <$> option auto
        ( long "year"
       <> short 'y'
       <> metavar "YEAR"
       <> help "Specify the year"
       <> value 2024 -- Default value
       <> showDefault )
  <*> option auto
        ( long "day"
       <> short 'd'
       <> metavar "DAY"
       <> help "Specify the day (required)"
       <> showDefault )
  <*> strOption
        ( long "input"
       <> short 'i'
       <> metavar "INPUT"
       <> help "Specify the input file"
       <> value "input.txt" -- Default value
       <> showDefault )

-- Helper to parse CLI arguments
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Run a solution for a specific Advent of Code day"
     <> header "Advent of Code Runner - a solution execution tool" )