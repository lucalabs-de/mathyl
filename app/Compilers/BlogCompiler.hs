{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Compilers.BlogCompiler (compile) where

import Control.Monad (forM, forM_, unless)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Aeson (ToJSON (toJSON), Value, object, (.=))
import Data.Either (fromRight)
import Data.Map.Strict (Map, insert, (!?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Settings.Options (Settings (..))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath (makeRelative, takeBaseName, takeDirectory, (-<.>), (</>))
import Text.Pandoc (
  Extension (..),
  Extensions,
  Pandoc (..),
  ReaderOptions (readerExtensions),
  def,
  extensionsFromList,
  readMarkdown,
  renderError,
  runPure,
  writeHtml5String,
 )

import Compilers.LinkCompiler (processHyperlinks)
import Compilers.MathCompiler (processMathBlocks)
import Compilers.Post (PostInfo (..))
import Compilers.Templates
import Compilers.TikzCompiler
import Parsers.MetadataParser (parseMetadata)

import Util.FileHelpers
import Util.Helpers

import Logging.Logger
import qualified Logging.Messages as Msg
import Debug.Trace

data AstInfo = AstInfo
  { aTitle :: Text
  , aMetadata :: Map Text Text
  , aTikzImages :: [TikzImage]
  }

data CompiledFileInfo = FileInfo
  { cfTitle :: Text
  , cfMetadata :: Map Text Text
  , cfOutLocation :: FilePath
  }

markdownExtensions :: Extensions
markdownExtensions =
  extensionsFromList
    [ Ext_fenced_code_blocks
    , Ext_fenced_code_attributes
    , Ext_yaml_metadata_block
    , Ext_tex_math_dollars
    , Ext_link_attributes
    , Ext_raw_html
    , Ext_markdown_in_html_blocks
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
  logMsg "Generating site..."

  liftIO $ removeDirectoryIfExists outDir -- clean workdir
  posts <- liftIO $ getMarkdownFiles inDir
  htmlFiles <- liftIO $ getHtmlFiles inDir
  otherFiles <- liftIO $ getOtherFiles inDir

  compiledFileInfo <- logSubroutine $ forM posts (compileFile inDir outDir)

  logMsg "Filling placeholders in HTML files"
  logSubroutine $ forM_ htmlFiles (fillHtmlPlaceholders inDir outDir compiledFileInfo)

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
  m CompiledFileInfo
compileFile inDir outDir post = do
  useNiceUrls <- asks oNiceUrls

  let fileName = takeBaseName post

  let outputFile =
        if useNiceUrls
          then replace inDir outDir (takeDirectory post) </> fileName </> "index.html"
          else replace inDir outDir post -<.> "html"

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

  md <- liftIO $ TIO.readFile post
  let pandocResult = runPure $ readMarkdown def{readerExtensions = markdownExtensions} md

  case pandocResult of
    Left bundle -> do
      logError "Failed!"
      logErrorP (renderError bundle)
      liftIO $ exitWith (ExitFailure 1)
    Right ast -> do
      (updatedAst, astInfo) <- processAst postInfo ast

      logSubroutine $ renderImages postInfo (aTikzImages astInfo)
      logSubroutine $ renderAst postInfo astInfo updatedAst

      return
        FileInfo
          { cfTitle = aTitle astInfo
          , cfMetadata = aMetadata astInfo
          , cfOutLocation = outputFile
          }

processAst ::
  ( MonadReader Settings m
  , MonadLogger m
  , MonadThrow m
  , MonadMask m
  , MonadIO m
  ) =>
  PostInfo ->
  Pandoc ->
  m (Pandoc, AstInfo)
processAst post ast = do
  let metadata = parseMetadata ast
  useSvgs <- asks oUseSvgs

  let texPkgs = commaSeparatedToList $ fromMaybe "" $ metadata !? "packages"
  let fileExt = if useSvgs then ".svg" else ".png"

  astH <- processHyperlinks ast
  astHM <- processMathBlocks astH
  let (astHMT, tikzImages) = processTikzBlocks (pAssetDir post) fileExt texPkgs astHM

  case metadata !? "title" of
    Nothing -> do
      logError $ Msg.missingMetadataKey "title"
      liftIO $ exitWith (ExitFailure 1)
    Just title -> return (astHMT, AstInfo title metadata tikzImages)

renderAst ::
  ( MonadReader Settings m
  , MonadLogger m
  , MonadIO m
  ) =>
  PostInfo ->
  AstInfo ->
  Pandoc ->
  m ()
renderAst post astInfo ast = do
  let metadata = aMetadata astInfo

  let html = fromRight "" . runPure $ writeHtml5String def ast

  logMsg "Filling template"
  let templateMetadata = metadata !? "template"

  case templateMetadata of
    Nothing -> logError $ Msg.missingMetadataKey "template"
    Just templateFile ->
      logSubroutine $
        fillTemplate
          post
          (insert "content" html metadata)
          (pInputDir post </> T.unpack templateFile)

renderImages ::
  ( MonadReader Settings m
  , MonadLogger m
  , MonadIO m
  ) =>
  PostInfo ->
  [TikzImage] ->
  m ()
renderImages post imgs = unless (null imgs) $ do
  liftIO $ createDirectoryIfMissing True (pAssetDir post)

  let numImages = length imgs

  forM_ (zip [1 :: Int ..] imgs) \(idx, source) -> do
    logMsg $ "Compiling Image " ++ show idx ++ "/" ++ show numImages
    logSubroutine $ compileTikzImage source

fillHtmlPlaceholders ::
  ( MonadReader Settings m
  , MonadLogger m
  , MonadIO m
  ) =>
  FilePath ->
  FilePath ->
  [CompiledFileInfo] ->
  FilePath ->
  m ()
fillHtmlPlaceholders inDir outDir info file = do
  logMsg file

  let outFile = replaceTopDirectory inDir outDir file
  let vals = generatePlaceholderValuesForFile outFile info

  fillStandaloneTemplate inDir (takeDirectory outFile) vals file

generatePlaceholderValuesForFile :: FilePath -> [CompiledFileInfo] -> Value
generatePlaceholderValuesForFile file info =
  object
    [ "posts" .= (toJsonObject <$> info)
    ]
 where
  toJsonObject :: CompiledFileInfo -> Value
  toJsonObject fileInfo =
    object
      [ "title" .= cfTitle fileInfo
      , "href" .= makeRelative file (cfOutLocation fileInfo)
      , "meta" .= toJSON (cfMetadata fileInfo)
      ]

-- | Returns a list of all markdown files stored in @p dir and its subdirectories.
getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles dir = filter (endsIn markdownFileExts) <$> listDirectoryRecursive dir

getHtmlFiles :: FilePath -> IO [FilePath]
getHtmlFiles dir = filter (endsIn htmlFileExts) <$> listDirectoryRecursive dir

getOtherFiles :: FilePath -> IO [FilePath]
getOtherFiles dir = filter (not . endsIn (markdownFileExts ++ htmlFileExts)) <$> listDirectoryRecursive dir
