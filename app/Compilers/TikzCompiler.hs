{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Compilers.TikzCompiler where

import Conversion.PdfConverter (convertPdfToPng, convertPdfToSvg)
import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import Logging.Logger
import Settings.Options (Settings (..))
import System.FilePath (takeBaseName, takeDirectory, (-<.>))
import System.Process (readCreateProcessWithExitCode, shell)
import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit (exitFailure)
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

compileTikzImage :: (MonadLogger m, MonadIO m) => Settings -> TikzImage -> m ()
compileTikzImage settings img =
  do
    let templateDir = takeDirectory (t_file img)
    let templateSrc = t_file img -<.> ".tex"
    let template = $(TH.compileMustacheText "tikz" tikzTemplate)

    let filledTemplate =
          renderMustache template $
            object
              [ "source" .= t_source img
              , "libraries" .= t_libraries img
              , "packages" .= t_packages img
              ]
    liftIO $ TIO.writeFile templateSrc filledTemplate

    let compileProcess = shell $ "pdflatex -halt-on-error -output-directory=" ++ templateDir ++ " " ++ templateSrc
    (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode compileProcess ""

    if (not . isErrorCode) exitCode
      then do
        let imagePath = t_file img -<.> ".pdf"

        if oUseSvgs settings
          then do
            liftIO $ convertPdfToSvg imagePath
            liftIO $ deleteAllExceptFileExtensions templateDir [".svg"] (takeBaseName $ t_file img)
          else do
            liftIO $ convertPdfToPng imagePath (oDefaultPngHeightInPx settings)
            liftIO $ deleteAllExceptFileExtensions templateDir [".png"] (takeBaseName $ t_file img)
      else do
        logError stdout
        logError stderr
        logError "Failed!"
        liftIO exitFailure
