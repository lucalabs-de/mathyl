{-# LANGUAGE OverloadedStrings #-}

module BlogCompiler (compile) where

import Control.Monad (filterM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor
import Data.Either (fromRight)
import Data.Functor
import Data.List (foldl', isSuffixOf)
import Data.Map.Strict (Map, fromList, insert, toList, (!?))
import Data.Maybe (fromMaybe)
import Data.Text (splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (forM)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath (joinPath, takeDirectory, takeFileName, (<.>), (</>))
import System.IO (hPrint, hPutStrLn)
import qualified System.IO as FD
import System.IO.Temp (withSystemTempDirectory)
import Text.Pandoc (
  Block (CodeBlock, Para, Plain),
  Extension (Ext_fenced_code_attributes, Ext_fenced_code_blocks, Ext_tex_math_dollars, Ext_yaml_metadata_block),
  Extensions,
  HTMLMathMethod (KaTeX),
  Inline (Image),
  Meta (unMeta),
  MetaValue (MetaInlines),
  Pandoc (Pandoc),
  PandocIO (PandocIO),
  ReaderOptions (readerExtensions),
  WriterOptions (writerHTMLMathMethod),
  def,
  extensionsFromList,
  handleError,
  nullAttr,
  nullMeta,
  readMarkdown,
  runIO,
  writeHtml5String,
  writeMarkdown,
  writeNative,
  writeRST,
 )
import Text.Pandoc.Writers (writeAsciiDoc)
import TikzCompiler
import Util.CliParsers (
  BuildOptions (..),
  Command (..),
  Options (optCommand),
  PreviewOptions (..),
  getCliOptions,
 )
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

-- TODO: replace print statements by proper logging
compile :: FilePath -> FilePath -> IO ()
compile inDir outDir = do
  putStrLn "Generating blog..."
  posts <- getMarkdownFiles inDir
  forM_ posts
    $ \post -> do
      let fileName = takeFileName post
      let dirName = takeDirectory post
      let outDirName = replace inDir outDir dirName
      let assetDir = outDirName </> fileName ++ "-assets"

      putStrLn (" Compiling " ++ show fileName)

      createDirectoryIfMissing True assetDir

      md <- TIO.readFile post
      result <- runIO $ readMarkdown def{readerExtensions = markdownExtensions} md

      print result

      case result of
        Left error -> do
          hPutStrLn FD.stderr "Failed!"
          hPrint FD.stderr error
        Right ast -> do
          metadata <- parseMetadata ast

          print metadata

          let texPkgs = commaSeparatedToList $ fromMaybe "" $ metadata !? "packages"
          let (parsedAst, tikzImages) = processTikzBlocks ast assetDir texPkgs
          let numImages = length tikzImages

          forM_ (zip [1 ..] tikzImages) $ \(idx, source) -> do
            putStrLn ("  Compiling Image " ++ show idx ++ "/" ++ show numImages)
            compileTikzImage source

          html <-
            fromRight ""
              <$> runIO (writeHtml5String def{writerHTMLMathMethod = katexWriter} parsedAst)

          putStrLn "  Filling template"
          let templateMetadata = metadata !? "template"

          case templateMetadata of
            Nothing -> hPutStrLn FD.stderr "  Missing template key in metadata"
            Just templateFile -> do
              fillTemplate (insert "content" html metadata) (dirName </> T.unpack templateFile)

      putStrLn "Done!"

fillTemplate :: Map T.Text T.Text -> FilePath -> IO ()
fillTemplate = undefined

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
  first (Pandoc meta)
    $ foldl' store ([], []) (zip [1 ..] items)
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
