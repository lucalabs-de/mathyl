module Main where

import BlogCompiler
import System.IO.Temp (withSystemTempDirectory)
import Util.CliParsers
import Util.Logger

display :: FilePath -> IO ()
display = undefined

run :: Logger -> Command -> IO ()
run l (Build opts) = compile l (bInDir opts) (bOutDir opts)
run l (Preview opts) = case pOutDir opts of
  Just outDir -> runMathyl l (pInDir opts) outDir
  Nothing -> withSystemTempDirectory "mathyl" (runMathyl l $ pInDir opts)

runMathyl :: Logger -> FilePath -> FilePath -> IO ()
runMathyl l inDir outDir = compile l inDir outDir >> display outDir

main :: IO ()
main = do
  opts <- getCliOptions
  run (mkLogger Message) (optCommand opts)
