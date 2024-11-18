{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Compilers.BlogCompiler (compile) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Either (fromRight)
import Data.Map.Strict (Map, fromList, insert, toList, (!?))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (forM)
import Settings.Options (Settings (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, takeDirectory, (-<.>), (</>))
import Text.Pandoc (
  Block (Plain),
  Extension (
    Ext_fenced_code_attributes,
    Ext_fenced_code_blocks,
    Ext_tex_math_dollars,
    Ext_yaml_metadata_block
  ),
  Extensions,
  Meta (unMeta),
  MetaValue (MetaInlines),
  Pandoc (..),
  ReaderOptions (readerExtensions),
  def,
  extensionsFromList,
  nullMeta,
  readMarkdown,
  renderError,
  runIO,
  writeHtml5String,
  writeMarkdown,
 )

import Compilers.MathCompiler (processMathBlocks)
import Compilers.Post (PostInfo (..))
import Compilers.Templates
import Compilers.TikzCompiler
import Control.Monad.Catch (MonadMask, MonadThrow)
import Util.FileHelpers
import Util.Helpers

import Logging.Logger

markdownExtensions :: Extensions
markdownExtensions =
  extensionsFromList
    [ Ext_fenced_code_blocks
    , Ext_fenced_code_attributes
    , Ext_yaml_metadata_block
    , Ext_tex_math_dollars
    ]

compile ::
  ( MonadReader Settings m
  , MonadLogger m
  , MonadThrow m
  , MonadMask m
  , MonadIO m
  ) =>
  FilePath ->
  FilePath ->
  m ()
compile inDir outDir = do
  logMsg "Generating blog..."

  liftIO $ removeDirectoryIfExists outDir -- clean workdir
  posts <- liftIO $ getMarkdownFiles inDir
  otherFiles <- liftIO $ getNonMarkdownFiles inDir

  logSubroutine $ forM_ posts (compileFile inDir outDir)

  forM_ otherFiles \f -> liftIO $ copyAndCreateParents f (replaceTopDirectory inDir outDir f)

  logMsg "Done!"

compileFile ::
  ( MonadReader Settings m
  , MonadLogger m
  , MonadThrow m
  , MonadMask m
  , MonadIO m
  ) =>
  FilePath ->
  FilePath ->
  FilePath ->
  m ()
compileFile inDir outDir post = do
  let fileName = takeBaseName post
  let outputFile = replace inDir outDir post -<.> "html"
  let outputDir = takeDirectory outputFile
  let assetDirName = fileName ++ "-assets"
  let assetDir = outputDir </> assetDirName

  let postInfo =
        PostInfo
          { pFileName = fileName
          , pInputFile = post
          , pOutputFile = outputFile
          , pInputDir = takeDirectory post
          , pOutputDir = outputDir
          , pAssetDir = assetDir
          , pAssetDirName = assetDirName
          }

  logMsg $ "Compiling " ++ show fileName

  liftIO $ createDirectoryIfMissing True assetDir

  md <- liftIO $ TIO.readFile post
  result <- liftIO . runIO $ readMarkdown def{readerExtensions = markdownExtensions} md

  case result of
    Left bundle -> do
      logError "Failed!"
      logErrorP (renderError bundle)
    Right ast -> logSubroutine $ renderAst postInfo ast

renderAst ::
  ( MonadReader Settings m
  , MonadLogger m
  , MonadThrow m
  , MonadMask m
  , MonadIO m
  ) =>
  PostInfo ->
  Pandoc ->
  m ()
renderAst post ast = do
  metadata <- liftIO $ parseMetadata ast
  useSvgs <- asks oUseSvgs

  let texPkgs = commaSeparatedToList $ fromMaybe "" $ metadata !? "packages"
  let fileExt = if useSvgs then ".svg" else ".png"

  logMsg "Processing LaTeX"

  (updatedAst, tikzImages) <-
    processTikzBlocks (pAssetDir post) fileExt texPkgs
      <$> processMathBlocks ast

  let numImages = length tikzImages

  forM_ (zip [1 :: Int ..] tikzImages) \(idx, source) -> do
    logMsg $ "Compiling Image " ++ show idx ++ "/" ++ show numImages
    logSubroutine $ compileTikzImage source

  html <-
    fromRight ""
      <$> (liftIO . runIO) (writeHtml5String def updatedAst)

  logMsg "Filling template"
  let templateMetadata = metadata !? "template"

  case templateMetadata of
    Nothing -> logError "Missing template key in metadata"
    Just templateFile ->
      logSubroutine $
        fillTemplate
          post
          (insert "content" html metadata)
          (pInputDir post </> T.unpack templateFile)

-- | Parses the Markdown metadata block and returns it as a map
parseMetadata :: Pandoc -> IO (Map T.Text T.Text)
parseMetadata (Pandoc meta _) = fromList <$> parseMetadata_ (unMeta meta)
 where
  parseMetadata_ metaMap = do
    let kvList = toList metaMap
    forM kvList $ \(k, v) -> case v of
      MetaInlines i -> do
        -- since we read from markdown, this should give us exactly what the user wrote
        parsedValue <- runIO $ writeMarkdown def (Pandoc nullMeta [Plain i])
        return (k, fromRight "" parsedValue)
      _ -> return (k, "")

-- | Returns a list of all markdown files stored in @p dir and its subdirectories.
getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles dir = filter (endsIn [".md", ".markdown"]) <$> listDirectoryRecursive dir

getNonMarkdownFiles :: FilePath -> IO [FilePath]
getNonMarkdownFiles dir = filter (not . endsIn [".md", ".markdown"]) <$> listDirectoryRecursive dir
