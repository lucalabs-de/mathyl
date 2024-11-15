module Parsers.CliParser where

import Data.Version (showVersion)
import Options.Applicative (
  Parser,
  command,
  execParser,
  header,
  help,
  helper,
  hsubparser,
  info,
  infoOption,
  long,
  metavar,
  optional,
  progDesc,
  short,
  strArgument,
  switch,
 )
import Paths_Mathyl (version)
import Settings.Options

newtype CliOptions = Options {optCommand :: Command}
  deriving (Show)

data Command = Build BuildOptions | Preview PreviewOptions
  deriving (Show)

data BuildOptions = BuildOptions
  { bInDir :: String
  , bOutDir :: String
  , bSettings :: UserDefinedSettings
  }
  deriving (Show)

data PreviewOptions = PreviewOptions
  { pInDir :: String
  , pOutDir :: Maybe String
  , pSettings :: UserDefinedSettings
  }
  deriving (Show)

commandParser :: Parser Command
commandParser =
  hsubparser $
    command "build" (info buildCommand (progDesc "Build blog"))
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
    <*> settingsParser

settingsParser :: Parser UserDefinedSettings
settingsParser =
  UserDefinedSettings
    <$> switch
      ( help "Compile tikZ to SVGs instead of PNGs. Requires pdf2svg or dvisvgm to be installed"
          <> long "use-svgs"
          <> short 's'
      )
    <*> switch
      ( help "Use server-side rendering for LaTeX formulas"
          <> long "server-render"
          <> short 'r'
      )
    <*> switch
      ( help "Continue compiling on errors"
          <> long "continue-on-errors"
          <> short 'e'
      )
    <*> switch
      ( help "Generate nicer looking urls"
          <> long "nice-urls"
          <> short 'e'
      )
    <*> switch
      ( help "Only log errors"
          <> long "quiet"
          <> short 'q'
      )

previewCommand :: Parser Command
previewCommand = Preview <$> previewOptionsParser

previewOptionsParser :: Parser PreviewOptions
previewOptionsParser =
  PreviewOptions
    <$> strArgument
      (metavar "IN-DIR" <> help "Directory of your template files")
    <*> (optional . strArgument)
      (metavar "OUT-DIR" <> help "Optional output directory for your blog files")
    <*> settingsParser

versionOption :: Parser (a -> a)
versionOption = infoOption ("mathyl " ++ showVersion version) (long "version" <> help "Show version")

optionsParser :: Parser CliOptions
optionsParser = Options <$> commandParser

getCliOptions :: IO CliOptions
getCliOptions =
  execParser $
    info (helper <*> versionOption <*> optionsParser) $
      header ("mathyl " ++ showVersion version ++ " - Plug and Play Static Site Generator")
