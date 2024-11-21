{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}

module Compilers.Templates (fillTemplate) where

import Compilers.Post (PostInfo (pOutputFile))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (toJSON)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Data.Void
import Logging.Logger
import Parsers.MustachePartialParser (
  TemplateInfo (containsKatexInfo, partials),
  pTemplateInfo,
 )
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)
import Text.Mustache (compileMustacheText, renderMustache)
import qualified Text.Mustache.Type as MT

import qualified Logging.Messages as Msg
import Util.FileHelpers (normalizeFilePath)
import Util.Helpers (
  flattenEithers,
  memoize,
  toPName,
  trim,
 )

fillTemplate :: (MonadLogger m, MonadIO m) => PostInfo -> Map T.Text T.Text -> FilePath -> m ()
fillTemplate post templateMap templateFile = do
  (compiledTemplate, templateInfo) <- getFullTemplate templateFile

  -- TODO maybe only log this when there's actually math in the template
  unless (containsKatexInfo templateInfo) $ logMsg Msg.noKatexWarning

  let filledTemplate = renderMustache compiledTemplate (toJSON templateMap)
  liftIO $ LTIO.writeFile (pOutputFile post) filledTemplate

getFullTemplate :: (MonadLogger m, MonadIO m) => FilePath -> m (MT.Template, TemplateInfo)
getFullTemplate path =
  do
    -- remove any surrounding whitespace and unify file paths for memoization
    let cleanPath = normalizeFilePath $ trim path

    fileExists <- liftIO $ doesFileExist cleanPath
    unless fileExists do
      logError $ Msg.templateNotFound cleanPath 
      error "Failed!"

    baseTemplate <- compileTemplateFile cleanPath
    templateInfo <- liftIO $ getTemplateInfo cleanPath
    case templateInfo of
      Left bundle -> logError (errorBundlePretty bundle) >> error "Failed!"
      Right info -> do
        compiledDependencies <- mapM compileTemplateFile (partials info)
        return (foldr (<>) baseTemplate compiledDependencies, info)

getTemplateInfo :: FilePath -> IO (Either (ParseErrorBundle T.Text Void) TemplateInfo)
getTemplateInfo path = do
  templateSrc <- TIO.readFile (trim path)
  let srcFileName = takeFileName path

  let templateInfo = runParser pTemplateInfo srcFileName templateSrc

  -- get template info for all partials
  partialsInfo <- flattenEithers <$> mapM (mapM getTemplateInfo . partials) templateInfo

  -- combine all partial info with original template info
  return $ templateInfo >>= (<$> partialsInfo) . foldr (<>)

compileTemplateFile :: (MonadLogger m, MonadIO m) => FilePath -> m MT.Template
compileTemplateFile path = do
  templateRes <- liftIO $ getCompiledTemplateByFile path
  case templateRes of
    Left bundle -> do
      logError Msg.templateCompilationFailed
      logError (errorBundlePretty bundle)
      error "Failed!"
    Right template -> return template

getCompiledTemplateByFile :: FilePath -> IO (Either (ParseErrorBundle T.Text Void) MT.Template)
getCompiledTemplateByFile =
  memoize \path -> do
    templateSrc <- TIO.readFile path
    let identifier = toPName path
    return $ compileMustacheText identifier templateSrc
