{-# LANGUAGE OverloadedStrings #-}

module BlogCompiler (compile) where

import Control.Monad (filterM, forM_)
import Data.Bifunctor
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Map.Strict (Map, fromList, insert, toList, (!?))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (forM)
import System.FilePath (takeDirectory, takeFileName, (<.>), (</>))
import Templates
import Text.Pandoc (
  Block (CodeBlock, Para, Plain),
  Extension (
    Ext_fenced_code_attributes,
    Ext_fenced_code_blocks,
    Ext_tex_math_dollars,
    Ext_yaml_metadata_block
  ),
  Extensions,
  HTMLMathMethod (KaTeX),
  Inline (Image),
  Meta (unMeta),
  MetaValue (MetaInlines),
  Pandoc (Pandoc),
  ReaderOptions (readerExtensions),
  WriterOptions (writerHTMLMathMethod),
  def,
  extensionsFromList,
  nullMeta,
  readMarkdown,
  renderError,
  runIO,
  writeHtml5String,
  writeMarkdown,
 )
import TikzCompiler
import Util.FileHelpers
import Util.Helpers
import Util.Logger
import System.Directory (createDirectoryIfMissing, listDirectory, doesDirectoryExist)

markdownExtensions :: Extensions
markdownExtensions =
  extensionsFromList
    [ Ext_fenced_code_blocks
    , Ext_fenced_code_attributes
    , Ext_yaml_metadata_block
    , Ext_tex_math_dollars
    ]

katexWriter :: HTMLMathMethod
katexWriter = KaTeX "vendor/katex"

compile :: Logger -> FilePath -> FilePath -> IO ()
compile logger inDir outDir = do
  logMsg logger "Generating blog..."

  removeDirectoryIfExists outDir -- clean workdir
  posts <- getMarkdownFiles inDir

  forM_ posts $ compileFile (mkChild logger) inDir outDir

  logMsg logger "Done!"

compileFile :: Logger -> FilePath -> FilePath -> FilePath -> IO ()
compileFile logger inDir outDir post = do
  let fileName = takeFileName post -- the file name of the input post
  let postDir = takeDirectory post -- the directory of the input post
  let outDirName = replace inDir outDir postDir -- the same relative path as the post, but starting in outDir
  let assetDir = outDirName </> fileName ++ "-assets" -- directory for storing assets such as images
  createDirectoryIfMissing True assetDir

  logMsg logger $ "Compiling " ++ show fileName

  md <- TIO.readFile post
  result <- runIO $ readMarkdown def{readerExtensions = markdownExtensions} md

  case result of
    Left error -> do
      logError logger "Failed!"
      logErrorP logger (renderError error)
    Right ast -> renderAst (mkChild logger) postDir assetDir outDirName ast

renderAst :: Logger -> FilePath -> FilePath -> FilePath -> Pandoc -> IO ()
renderAst logger postDir assetDir outDir ast = do
  metadata <- parseMetadata ast

  let texPkgs = commaSeparatedToList $ fromMaybe "" $ metadata !? "packages"
  let (parsedAst, tikzImages) = processTikzBlocks ast assetDir texPkgs
  let numImages = length tikzImages

  forM_ (zip [1 ..] tikzImages) $ \(idx, source) -> do
    logMsg logger $ "Compiling Image " ++ show idx ++ "/" ++ show numImages
    compileTikzImage source

  html <-
    fromRight ""
      <$> runIO (writeHtml5String def{writerHTMLMathMethod = katexWriter} parsedAst)

  logMsg logger "Filling template"
  let templateMetadata = metadata !? "template"

  case templateMetadata of
    Nothing -> logError logger "Missing template key in metadata"
    Just templateFile ->
      fillTemplate (insert "content" html metadata) (postDir </> T.unpack templateFile) outDir

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

processTikzBlocks :: Pandoc -> FilePath -> [T.Text] -> (Pandoc, [TikzImage])
processTikzBlocks (Pandoc meta items) path texPkgs =
  first (Pandoc meta) $
    foldl' store ([], []) (zip [1 ..] items)
 where
  store (pandocItems, tikzItems) (idx, CodeBlock (_, "tikz" : libs, _) src) =
    (tikzPlaceholder path idx : pandocItems, tikzImage src libs texPkgs idx : tikzItems)
  store (pandocItems, tikzItems) (_, item) =
    (item : pandocItems, tikzItems)
  tikzImage src lib pkg idx = TikzImage src lib pkg (tikzFilePath path idx)

tikzPlaceholder :: FilePath -> Int -> Block
tikzPlaceholder dir idx = Para [Image ("tikz", [], []) [] (T.pack $ tikzFilePath dir idx, "")]

tikzFilePath :: FilePath -> Int -> FilePath
tikzFilePath dir idx = dir </> show idx <.> "pdf"

getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles dir = do
  contents <- map (dir </>) <$> listDirectory dir
  dirs <- filterM doesDirectoryExist contents
  let mdFiles = filter (endsIn [".md", ".markdown"]) contents
  (mdFiles ++) . concat <$> mapM getMarkdownFiles dirs
