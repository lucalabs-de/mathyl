-- TODO: refactor this into multiple files
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor
import Data.Functor
import Data.List (foldl', isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeFileName, (<.>), (</>), joinPath)
import System.IO.Temp (withSystemTempDirectory)
import System.IO (hPutStrLn, hPrint)
import qualified System.IO as FD
import Text.Pandoc (
  Block (CodeBlock, Para),
  Extension (Ext_fenced_code_attributes, Ext_fenced_code_blocks, Ext_tex_math_dollars, Ext_yaml_metadata_block),
  Extensions,
  HTMLMathMethod (KaTeX),
  Inline (Image),
  Pandoc (Pandoc),
  PandocIO (PandocIO),
  ReaderOptions (readerExtensions),
  WriterOptions (writerHTMLMathMethod),
  def,
  extensionsFromList,
  handleError,
  readMarkdown,
  runIO,
  writeHtml5String,
  writeRST, Meta (unMeta),
 )
import Util.CliParsers (
  BuildOptions (..),
  Command (..),
  Options (optCommand),
  PreviewOptions (..),
  getCliOptions,
 )
import Data.Map.Strict (Map, fromList, toList)

newtype TikzSource = TikzSource T.Text deriving (Show)

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

          forM_ (zip [1..] tikzImages) $ \(idx, source) -> do 
            putStrLn ("  Compiling Image " ++ show idx ++ "/" ++ show numImages)
            compileTikzImage source 

          html <- runIO $ writeHtml5String def{writerHTMLMathMethod = katexWriter} parsedAst
          putStrLn "  Filling template"

          metadata <- parseMetadata parsedAst

          fillTemplate (insert "content" html metadata) 
          liftIO $ print html


      putStrLn "Done!"

parseMetadata :: Pandoc -> IO (Map String String)
parseMetadata (Pandoc meta _) = fromList parseMetadata_ (unMeta meta)
  where parseMetadata_ metaMap = do let kvList = toList metaMap
                                    forM kvList $ \(k, v) -> do 
                                      

compileTikzImage :: TikzSource -> IO ()
compileTikzImage = undefined

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

endsIn :: [String] -> String -> Bool
endsIn sfxs w = any (`isSuffixOf` w) sfxs

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
