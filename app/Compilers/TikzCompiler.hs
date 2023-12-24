{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Compilers.TikzCompiler where

import Control.Monad (when)
import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import System.Directory (listDirectory, removeFile)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, takeFileName, (-<.>), (</>))
import System.IO (hPrint, hPutStrLn)
import qualified System.IO as FD (stderr)
import System.Process (callCommand, readCreateProcessWithExitCode, shell)
import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import Util.FileHelpers
import Util.Files (tikzTemplate)
import Util.Helpers

data TikzImage = TikzImage
  { t_source :: T.Text
  , t_libraries :: [T.Text]
  , t_packages :: [T.Text]
  , t_file :: FilePath
  }
  deriving (Show)

compileTikzImage :: TikzImage -> IO ()
compileTikzImage img =
  do
    let templateDir = takeDirectory (t_file img)
    let templateSrc = t_file img -<.> ".tex"
    let template = $(TH.compileMustacheText "tikz" tikzTemplate)

    let filledTemplate =
          renderMustache template
            $ object
              [ "source" .= t_source img
              , "libraries" .= t_libraries img
              , "packages" .= t_packages img
              ]
    TIO.writeFile templateSrc filledTemplate

    let compileProcess = shell $ "pdflatex -halt-on-error -output-directory=" ++ templateDir ++ " " ++ templateSrc
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode compileProcess ""

    -- remove pdflatex compilation artifacts
    deleteAllExceptFileExtensions templateDir [".pdf"] (takeBaseName $ t_file img)

    when (isErrorCode exitCode) $ do
      hPutStrLn FD.stderr "   Failed!"
      hPutStrLn FD.stderr stdout
