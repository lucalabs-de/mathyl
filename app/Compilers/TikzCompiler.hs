{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Compilers.TikzCompiler where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Conversion.PdfConverter (convertPdfToPng, convertPdfToSvg)
import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import Data.Bifunctor (bimap)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import Logging.Logger
import Settings.Options (Settings (..))
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, takeDirectory, (-<.>), (<.>), (</>))
import System.Process (readCreateProcessWithExitCode, shell)
import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import Text.Pandoc (Block (..), Inline (..), Pandoc (..))

import Control.Monad.Reader (MonadReader, asks)
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

-- Compiles a TikzImage and writes it to the specified path
compileTikzImage :: (MonadReader Settings m, MonadLogger m, MonadIO m) => TikzImage -> m ()
compileTikzImage img =
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

        useSvgs <- asks oUseSvgs
        defaultPngHeight <- asks oDefaultPngHeightInPx

        if useSvgs
          then do
            liftIO $ convertPdfToSvg imagePath
            liftIO $ deleteAllExceptFileExtensions templateDir [".svg"] (takeBaseName $ t_file img)
          else do
            liftIO $ convertPdfToPng imagePath defaultPngHeight
            liftIO $ deleteAllExceptFileExtensions templateDir [".png"] (takeBaseName $ t_file img)
      else do
        logError stdout
        logError stderr
        logError "Failed!"
        liftIO exitFailure

-- Finds tikZ blocks that should be rendered as images in the Pandoc AST and replaces them
-- by Image blocks. Returns both the updated AST and the list of tikZ images to be compiled.
processTikzBlocks :: FilePath -> String -> [T.Text] -> Pandoc -> (Pandoc, [TikzImage])
processTikzBlocks assetPath fileExtension texPkgs (Pandoc meta items) =
  bimap (Pandoc meta . reverse) reverse $ -- we construct the lists in reverse to avoid quadratic complexity
    foldl' store ([], []) (zip [1 ..] items)
 where
  store (pandocItems, tikzItems) (idx, CodeBlock (_, "tikz" : libs, _) src) =
    ( tikzPlaceholder assetPath fileExtension idx : pandocItems
    , tikzImage src libs texPkgs idx : tikzItems
    )
  store (pandocItems, tikzItems) (_, item) =
    (item : pandocItems, tikzItems)
  tikzImage src lib pkg idx =
    TikzImage src lib pkg (tikzFilePath assetPath fileExtension idx)

-- Constructs a Pandoc Image block as used by @ref processTikzBlocks. The Image block
-- points to a path generated by @ref tikzFilePath based on @p dir and @p idx.
tikzPlaceholder :: FilePath -> String -> Int -> Block
tikzPlaceholder dir ext idx =
  Para [Image ("tikz", [], []) [] (T.pack $ tikzFilePath (takeBaseName dir) ext idx, "")]

-- | Given an index @p idx and a file path @p dir, constructs the file path "dir/idx.pdf"
tikzFilePath :: FilePath -> String -> Int -> FilePath
tikzFilePath dir ext idx = dir </> show idx <.> ext
