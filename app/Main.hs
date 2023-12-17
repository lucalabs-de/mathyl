-- TODO: refactor this into multiple files
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BlogCompiler (compile)
import System.IO.Temp (withSystemTempDirectory)
import Util.CliParsers

display :: FilePath -> IO ()
display = undefined

run :: Command -> IO ()
run (Build opts) = compile (bInDir opts) (bOutDir opts)
run (Preview opts) = case pOutDir opts of
  Just outDir -> runMathyl (pInDir opts) outDir
  Nothing -> withSystemTempDirectory "mathyl" (runMathyl $ pInDir opts)

runMathyl :: FilePath -> FilePath -> IO ()
runMathyl inDir outDir = compile inDir outDir >> display outDir

main :: IO ()
main = do
  opts <- getCliOptions
  run (optCommand opts)
