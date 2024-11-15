{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}

module Compilers.Templates (fillTemplate) where

import Compilers.Post (PostInfo (pOutputFile))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
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

fillTemplate :: (MonadLogger m, MonadIO m) => PostInfo -> Map T.Text T.Text -> FilePath -> m ()
fillTemplate post templateMap templateFile = do
  compiledTemplate <- getFullTemplate templateFile
  let filledTemplate = renderMustache compiledTemplate (toJSON templateMap)
  liftIO $ LTIO.writeFile (pOutputFile post) filledTemplate

getFullTemplate :: (MonadLogger m, MonadIO m) => FilePath -> m MT.Template
getFullTemplate path =
  do
    -- remove any surrounding whitespace and unify file paths for memoization
    let cleanPath = normalizeFilePath $ trim path

    fileExists <- liftIO $ doesFileExist cleanPath
    unless fileExists do
      logError $ "Could not find template " ++ cleanPath
      error "Failed!"

    baseTemplate <- compileTemplateFile cleanPath
    dependencies <- liftIO $ getTemplateDependencies cleanPath
    case dependencies of
      Left bundle -> logError (errorBundlePretty bundle) >> error "Failed!"
      Right deps -> do
        compiledDependencies <- mapM compileTemplateFile deps
        return $ foldr (<>) baseTemplate compiledDependencies

getTemplateDependencies :: FilePath -> IO (Either (ParseErrorBundle T.Text Void) [FilePath])
getTemplateDependencies path = do
  templateSrc <- TIO.readFile (trim path)
  let srcFileName = takeFileName path
  return $ runParser pPartials srcFileName templateSrc

compileTemplateFile :: (MonadLogger m, MonadIO m) => FilePath -> m MT.Template
compileTemplateFile path = do
  templateRes <- liftIO $ getCompiledTemplateByFile path
  case templateRes of
    Left bundle -> do
      logError "Could not compile template!"
      logError (errorBundlePretty bundle)
      error "Failed!"
    Right template -> return template

getCompiledTemplateByFile :: FilePath -> IO (Either (ParseErrorBundle T.Text Void) MT.Template)
getCompiledTemplateByFile =
  memoize \path -> do
    templateSrc <- TIO.readFile path
    let identifier = toPName path
    return $ compileMustacheText identifier templateSrc
