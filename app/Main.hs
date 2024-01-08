module Main where

import Compilers.BlogCompiler
import Logging.Logger
import Parsers.CliParser
import Settings.Options (Settings, fromUserDefinedSettings)
import System.IO.Temp (withSystemTempDirectory)

display :: FilePath -> IO ()
display = undefined

run :: Logger -> Settings -> Command -> IO ()
run l s (Build opts) = compile l s (bInDir opts) (bOutDir opts)
run l s (Preview opts) = case pOutDir opts of
  Just outDir -> runMathyl l s (pInDir opts) outDir
  Nothing -> withSystemTempDirectory "mathyl" (runMathyl l s $ pInDir opts)

runMathyl :: Logger -> Settings -> FilePath -> FilePath -> IO ()
runMathyl l s inDir outDir = compile l s inDir outDir >> display outDir

buildSettings :: Command -> Settings
buildSettings (Build opts) = fromUserDefinedSettings $ bSettings opts
buildSettings (Preview opts) = fromUserDefinedSettings $ pSettings opts

main :: IO ()
main = do
  opts <- getCliOptions

  let command = optCommand opts
  let settings = buildSettings command

  run (mkLogger Message) settings command
