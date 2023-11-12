module Util.CliParsers where

import Data.Version (showVersion)
import Options.Applicative (
  Parser,
  command,
  execParser,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  metavar,
  optional,
  progDesc,
  strArgument,
  subparser,
 )
import Paths_Mathyl (version)

newtype Options = Options {optCommand :: Command}
  deriving Show 

data Command = Build BuildOptions | Preview PreviewOptions
  deriving Show

data BuildOptions = BuildOptions
  { bInDir :: String
  , bOutDir :: String
  }
  deriving Show
data PreviewOptions = PreviewOptions
  { pInDir :: String
  , pOutDir :: Maybe String
  }
  deriving Show

commandParser :: Parser Command
commandParser =
  subparser
    $ command "build" (info buildCommand (progDesc "Start the build process"))
    <> command "preview" (info previewCommand (progDesc "Build and run blog at 127.0.0.1:8080"))

buildCommand :: Parser Command
buildCommand = Build <$> buildOptionsParser

buildOptionsParser :: Parser BuildOptions
buildOptionsParser =
  BuildOptions
    <$> strArgument
      (metavar "IN-DIR" <> help "Directory of your template files")
    <*> strArgument
      (metavar "OUT-DIR" <> help "Output directory for your blog files")

previewCommand :: Parser Command
previewCommand = Preview <$> previewOptionsParser

previewOptionsParser :: Parser PreviewOptions
previewOptionsParser =
  PreviewOptions
    <$> strArgument
      (metavar "IN-DIR" <> help "Directory of your template files")
    <*> (optional . strArgument)
      (metavar "OUT-DIR" <> help "Optional output directory for your blog files")

versionOption :: Parser (a -> a)
versionOption = infoOption ("mathyl " ++ showVersion version) (long "version" <> help "Show version")

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

getCliOptions :: IO Options
getCliOptions =
  execParser
    $ info (helper <*> versionOption <*> optionsParser)
    $ header ("mathyl" ++ showVersion version)
