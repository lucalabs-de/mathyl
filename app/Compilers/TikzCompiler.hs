{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Compilers.TikzCompiler where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Conversion.PdfConverter (convertPdfToPng, convertPdfToSvg)
import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import Data.Bifunctor (bimap)
import Data.List (foldl')
import Data.Map (Map, fromList, (!?))
import Data.Text (Text)
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
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Read (readMaybe)
import Util.FileHelpers
import Util.Files (tikzTemplate)
import Util.Helpers

data ImageProperty = Height | Width | AltText
  deriving (Eq, Ord)

data PropertyValue = IntValue Int | TextValue Text

class ExtractValue a where
  value :: PropertyValue -> Maybe a

instance ExtractValue Int where
  value (IntValue v) = Just v
  value _ = Nothing

instance ExtractValue Text where
  value (TextValue v) = Just v
  value _ = Nothing

data TikzImage = TikzImage
  { t_source :: Text
  , t_libraries :: [Text]
  , t_packages :: [Text]
  , t_height :: Maybe Int
  , t_width :: Maybe Int
  , t_altText :: Maybe Text
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

        if useSvgs
          then do
            liftIO $ convertPdfToSvg imagePath
            liftIO $ deleteAllExceptFileExtensions templateDir [".svg"] (takeBaseName $ t_file img)
          else do
            liftIO $ convertPdfToPng imagePath
            liftIO $ deleteAllExceptFileExtensions templateDir [".png"] (takeBaseName $ t_file img)
      else do
        logError stdout
        logError stderr
        logError "Failed!"
        liftIO exitFailure

-- Finds tikZ blocks that should be rendered as images in the Pandoc AST and replaces them
-- by Image blocks. Returns both the updated AST and the list of tikZ images to be compiled.
processTikzBlocks :: FilePath -> String -> [Text] -> Pandoc -> (Pandoc, [TikzImage])
processTikzBlocks assetPath fileExtension texPkgs (Pandoc meta items) =
  -- we construct the lists in reverse to avoid quadratic complexity
  bimap (Pandoc meta . reverse) reverse $ foldl' store ([], []) (zip [1 ..] items)
 where
  store (pandocItems, tikzItems) (idx, CodeBlock (_, "tikz" : libs, params) src) =
    ( tikzPlaceholder assetPath fileExtension idx (parseImageParams params) : pandocItems
    , tikzImage src libs texPkgs idx (parseImageParams params) : tikzItems
    )
  store (pandocItems, tikzItems) (_, item) = (item : pandocItems, tikzItems)
  tikzImage src lib pkg idx params =
    TikzImage
      { t_source = src
      , t_libraries = lib
      , t_packages = pkg
      , t_file = tikzFilePath assetPath fileExtension idx
      , t_width = value =<< params !? Width
      , t_height = value =<< params !? Height
      , t_altText = value =<< params !? AltText
      }

parseImageParams :: [(Text, Text)] -> Map ImageProperty PropertyValue
parseImageParams = fromList . mapMaybe parseProperty
 where
  parseProperty :: (Text, Text) -> Maybe (ImageProperty, PropertyValue)
  parseProperty ("height", h) = sequence (Height, IntValue <$> readMaybe (T.unpack h))
  parseProperty ("width", w) = sequence (Width, IntValue <$> readMaybe (T.unpack w))
  parseProperty ("altText", w) = Just (AltText, TextValue w)
  parseProperty _ = Nothing

-- Constructs a Pandoc Image block as used by @ref processTikzBlocks. The Image block
-- points to a path generated by @ref tikzFilePath based on @p dir and @p idx.
tikzPlaceholder :: FilePath -> String -> Int -> Map ImageProperty PropertyValue -> Block
tikzPlaceholder dir ext idx props =
  Para
    [ Image
        ( "tikz"
        , []
        , mapMaybe
            sequence
            [ ("alt", value =<< props !? AltText)
            , ("width", value =<< props !? Width)
            , ("height", value =<< props !? Height)
            ]
        )
        []
        (T.pack $ tikzFilePath (takeBaseName dir) ext idx, "")
    ]

-- | Given an index @p idx and a file path @p dir, constructs the file path "dir/idx.pdf"
tikzFilePath :: FilePath -> String -> Int -> FilePath
tikzFilePath dir ext idx = dir </> show idx <.> ext
