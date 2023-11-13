-- TODO: refactor this into multiple files
module Main where

import Control.Monad (filterM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Functor
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Text.Pandoc (
  Extension (Ext_fenced_code_attributes, Ext_fenced_code_blocks, Ext_yaml_metadata_block),
  Extensions,
  HTMLMathMethod,
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
  writeRST,
 )
import Util.CliParsers (
  BuildOptions (..),
  Command (..),
  Options (optCommand),
  PreviewOptions (..),
  getCliOptions,
 )

markdownExtensions :: Extensions
markdownExtensions =
  extensionsFromList
    [ Ext_fenced_code_blocks
    , Ext_fenced_code_attributes
    , Ext_yaml_metadata_block
    ]

katexWriter :: HTMLMathMethod
katexWriter = undefined

-- TODO: replace print statements by proper logging
compile :: FilePath -> FilePath -> IO ()
compile inDir outDir = do
  putStrLn "Generating blog..."
  posts <- getMarkdownFiles inDir
  forM_ posts
    $ \post -> do
      let fileName = takeFileName post
      putStrLn (" Compiling " ++ show fileName)

      md <- TIO.readFile post
      result <- runIO $ do
        doc <- readMarkdown def{readerExtensions = markdownExtensions} md
        liftIO $ print doc
        html <- writeHtml5String def{writerHTMLMathMethod = katexWriter} doc
        -- liftIO $ print html
        return html

      putStrLn "Done!"

-- let doc = readMarkdown def md :: PandocIO Pandoc
-- doc2 <- doc
-- print doc2

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
