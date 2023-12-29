module Compilers.Templates (fillTemplate) where

import Compilers.Post (PostInfo (pOutputFile))
import Control.Monad (unless)
import Data.Aeson (toJSON)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Data.Void
import Logging.Logger
import Parsers.MustachePartialParser (pPartials)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)
import Text.Mustache (compileMustacheText, renderMustache)
import qualified Text.Mustache.Type as MT
import Util.FileHelpers (normalizeFilePath)
import Util.Helpers (memoize, toPName, trim)

fillTemplate :: Logger -> PostInfo -> Map T.Text T.Text -> FilePath -> IO ()
fillTemplate logger post templateMap templateFile = do
  print templateMap
  print $ toJSON templateMap
  compiledTemplate <- getFullTemplate logger templateFile
  let filledTemplate = renderMustache compiledTemplate (toJSON templateMap)
  LTIO.writeFile (pOutputFile post) filledTemplate

getFullTemplate :: Logger -> FilePath -> IO MT.Template
getFullTemplate logger path =
  do
    -- remove any surrounding whitespace and unify file paths for memoization
    let cleanPath = normalizeFilePath $ trim path

    fileExists <- doesFileExist cleanPath
    unless fileExists $ do
      logError logger $ "Could not find template " ++ cleanPath
      error "Failed!"

    baseTemplate <- compileTemplateFile logger cleanPath
    dependencies <- getTemplateDependencies cleanPath
    case dependencies of
      Left bundle -> logError logger (errorBundlePretty bundle) >> error "Failed!"
      Right deps -> do
        compiledDependencies <- mapM (compileTemplateFile logger) deps
        return $ foldr (<>) baseTemplate compiledDependencies

getTemplateDependencies :: FilePath -> IO (Either (ParseErrorBundle T.Text Void) [FilePath])
getTemplateDependencies path = do
  templateSrc <- TIO.readFile (trim path)
  let srcFileName = takeFileName path
  return $ runParser pPartials srcFileName templateSrc

compileTemplateFile :: Logger -> FilePath -> IO MT.Template
compileTemplateFile logger path = do
  templateRes <- getCompiledTemplateByFile path
  case templateRes of
    Left bundle -> do
      logError logger "Could not compile template!"
      logError logger (errorBundlePretty bundle)
      error "Failed!"
    Right template -> return template

getCompiledTemplateByFile :: FilePath -> IO (Either (ParseErrorBundle T.Text Void) MT.Template)
getCompiledTemplateByFile =
  memoize
    ( \path -> do
        templateSrc <- TIO.readFile path
        let identifier = toPName path
        return $ compileMustacheText identifier templateSrc
    )
