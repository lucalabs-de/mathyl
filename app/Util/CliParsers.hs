module Util.CliParsers where

import Data.Version (showVersion)
import Options.Applicative (
  Parser,
  command,
  help,
  hsubparser,
  info,
  infoOption,
  metavar,
  optional,
  progDesc,
  strArgument,
 )
import Paths_Mathyl (version)

newtype Options = Options {optCommand :: Maybe Command}

data Command = Build BuildOptions | Stop
data BuildOptions = BuildOptions
  { bInDir :: String
  , bOutDir :: String
  }

commandParser :: Parser (Maybe Command)
commandParser =
  optional
    . hsubparser
    $ command "build" (info buildCommand (progDesc "Start the build process"))

buildCommand :: Parser Command
buildCommand = Build <$> buildOptionsParser

buildOptionsParser :: Parser BuildOptions
buildOptionsParser =
  BuildOptions
    <$> strArgument
      (metavar "IN-DIR" <> help "Directory of your template files")
    <*> strArgument
      (metavar "OUT-DIR" <> help "Output directory for your blog files")

versionOption :: Parser (a -> a)
versionOption = infoOption ("end " ++ showVersion version) (long "version" <> help "Show version")

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

getCliOptions :: IO Options
getCliOptions =
  execParser
    $ info (helper <*> versionOption <*> optionsParser)
    $ header ("mathyl" ++ showVersion version)
