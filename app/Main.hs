module Main where

import Compilers.BlogCompiler
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import Logging.Logger
import Parsers.CliParser
import Settings.Options (Settings (oQuiet), fromUserDefinedSettings)
import System.IO.Temp (withSystemTempDirectory)

display :: (MonadReader Settings m, MonadIO m) => FilePath -> m ()
display = undefined

run ::
  ( MonadReader Settings m
  , MonadLogger m
  , MonadMask m
  , MonadIO m
  ) =>
  Command ->
  m ()
run (Build opts) = compile (bInDir opts) (bOutDir opts)
run (Preview opts) = case pOutDir opts of
  Just outDir -> runMathyl (pInDir opts) outDir
  Nothing -> withSystemTempDirectory "mathyl" (runMathyl $ pInDir opts)

runMathyl ::
  ( MonadReader Settings m
  , MonadLogger m
  , MonadMask m
  , MonadIO m
  ) =>
  FilePath ->
  FilePath ->
  m ()
runMathyl inDir outDir = compile inDir outDir >> display outDir

buildSettings :: Command -> Settings
buildSettings (Build opts) = fromUserDefinedSettings $ bSettings opts
buildSettings (Preview opts) = fromUserDefinedSettings $ pSettings opts

main :: IO ()
main = do
  opts <- getCliOptions

  let command = optCommand opts
  let settings = buildSettings command
  let logLevel = if oQuiet settings then Error else Message

  runLoggerT (runReaderT (run command) settings) logLevel
