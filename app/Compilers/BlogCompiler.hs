{-# LANGUAGE OverloadedStrings #-}

module Compilers.BlogCompiler (compile) where

import Compilers.Post (PostInfo (..))
import Compilers.Templates
import Compilers.TikzCompiler
import Control.Monad (filterM, forM_)
import Data.Bifunctor
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Map.Strict (Map, fromList, insert, toList, (!?))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (forM)
import Logging.Logger
import Settings.Options (Settings (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeDirectory, (-<.>), (<.>), (</>))
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
import Util.FileHelpers
import Util.Helpers

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

compile :: Logger -> Settings -> FilePath -> FilePath -> IO ()
compile logger settings inDir outDir = do
  logMsg logger "Generating blog..."

  removeDirectoryIfExists outDir -- clean workdir
  posts <- getMarkdownFiles inDir

  forM_ posts $ compileFile (mkChild logger) settings inDir outDir

  logMsg logger "Done!"

compileFile :: Logger -> Settings -> FilePath -> FilePath -> FilePath -> IO ()
compileFile logger settings inDir outDir post = do
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

  logMsg logger $ "Compiling " ++ show fileName

  createDirectoryIfMissing True assetDir

  md <- TIO.readFile post
  result <- runIO $ readMarkdown def{readerExtensions = markdownExtensions} md

  case result of
    Left bundle -> do
      logError logger "Failed!"
      logErrorP logger (renderError bundle)
    Right ast -> renderAst (mkChild logger) settings postInfo ast

renderAst :: Logger -> Settings -> PostInfo -> Pandoc -> IO ()
renderAst logger settings post ast = do
  metadata <- parseMetadata ast

  let texPkgs = commaSeparatedToList $ fromMaybe "" $ metadata !? "packages"
  let fileExt = if oUseSvgs settings then ".svg" else ".png"

  let (parsedAst, tikzImages) = processTikzBlocks ast (pAssetDir post) fileExt texPkgs
  let numImages = length tikzImages

  forM_ (zip [1 :: Int ..] tikzImages) $ \(idx, source) -> do
    logMsg logger $ "Compiling Image " ++ show idx ++ "/" ++ show numImages
    compileTikzImage (mkChild logger) settings source

  html <-
    fromRight ""
      <$> runIO (writeHtml5String def{writerHTMLMathMethod = katexWriter} parsedAst)

  logMsg logger "Filling template"
  let templateMetadata = metadata !? "template"

  case templateMetadata of
    Nothing -> logError logger "Missing template key in metadata"
    Just templateFile ->
      fillTemplate
        (mkChild logger)
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

{- |
  Finds tikZ blocks that should be rendered as images in the Pandoc AST and replaces them
  by Image blocks. Returns both the updated AST and the list of tikZ images to be
  compiled.
-}
processTikzBlocks :: Pandoc -> FilePath -> String -> [T.Text] -> (Pandoc, [TikzImage])
processTikzBlocks (Pandoc meta items) assetPath fileExtension texPkgs =
  first (Pandoc meta) $
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

{- | Constructs a Pandoc Image block as used by @ref processTikzBlocks. The Image block
points to a path generated by @ref tikzFilePath based on @p dir and @p idx.
-}
tikzPlaceholder :: FilePath -> String -> Int -> Block
tikzPlaceholder dir ext idx =
  Para [Image ("tikz", [], []) [] (T.pack $ tikzFilePath (takeBaseName dir) ext idx, "")]

-- | Given an index @p idx and a file path @p dir, constructs the file path "dir/idx.pdf"
tikzFilePath :: FilePath -> String -> Int -> FilePath
tikzFilePath dir ext idx = dir </> show idx <.> ext

-- | Returns a list of all markdown files stored in @p dir and its subdirectories.
getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles dir = do
  contents <- map (dir </>) <$> listDirectory dir
  dirs <- filterM doesDirectoryExist contents
  let mdFiles = filter (endsIn [".md", ".markdown"]) contents
  (mdFiles ++) . concat <$> mapM getMarkdownFiles dirs
