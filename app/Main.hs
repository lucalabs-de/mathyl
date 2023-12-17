-- TODO: refactor this into multiple files
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor
import Data.Either (fromRight)
import Data.Functor
import Data.List (foldl', isSuffixOf)
import Data.Map.Strict (Map, fromList, insert, toList, (!?))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (forM)
import System.Directory (doesDirectoryExist, listDirectory)
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
  writeNative,
  writeRST,
 )
import Util.CliParsers (
  BuildOptions (..),
  Command (..),
  Options (optCommand),
  PreviewOptions (..),
  getCliOptions,
 )
import Util.Helpers
import TikzCompiler

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
      let assetDir = dirName </> joinPath [dirName, "-assets"]

      putStrLn (" Compiling " ++ show fileName)

      md <- TIO.readFile post
      result <- runIO $ readMarkdown def{readerExtensions = markdownExtensions} md

      case result of
        Left error -> do
          hPutStrLn FD.stderr "Failed!"
          hPrint FD.stderr error
        Right ast -> do
          let (parsedAst, tikzImages) = parseTikzBlocks ast assetDir
          let numImages = length tikzImages

          forM_ (zip [1 ..] tikzImages) $ \(idx, source) -> do
            putStrLn ("  Compiling Image " ++ show idx ++ "/" ++ show numImages)
            compileTikzImage source

          html <- fromRight "" <$> runIO (writeHtml5String def{writerHTMLMathMethod = katexWriter} parsedAst)
          putStrLn "  Filling template"

          metadata <- parseMetadata parsedAst

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

parseMetadata_ :: Map T.Text MetaValue -> IO [(T.Text, T.Text)]
parseMetadata_ metaMap = do
  let kvList = toList metaMap
  forM kvList $ \(k, v) -> case v of
    MetaInlines i -> do
      parsedValue <- runIO $ writeNative def (Pandoc nullMeta [Plain i])
      return (k, fromRight "" parsedValue)
    _ -> return (k, "")

tikzPlaceholder :: FilePath -> Int -> Block
tikzPlaceholder path idx = Para [Image ("tikz", [], []) [] (T.pack $ path </> show idx <.> "pdf", "")]

parseTikzBlocks :: Pandoc -> FilePath -> (Pandoc, [TikzSource])
parseTikzBlocks (Pandoc meta items) path =
  first (Pandoc meta)
    $ foldl' insert ([], []) (zip [1 ..] items)
 where
  insert (pandocItems, tikzItems) (idx, CodeBlock (_, ["tikz"], _) source) =
    (tikzPlaceholder path idx : pandocItems, TikzSource source : tikzItems)
  insert (pandocItems, tikzItems) (_, item) =
    (item : pandocItems, tikzItems)

getMarkdownFiles :: FilePath -> IO [FilePath]
getMarkdownFiles dir = do
  contents <- map (dir </>) <$> listDirectory dir
  dirs <- filterM doesDirectoryExist contents
  let mdFiles = filter (endsIn [".md", ".markdown"]) contents
  (mdFiles ++) . concat <$> mapM getMarkdownFiles dirs

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
