module Compilers.Templates (fillTemplate) where

import Compilers.Post (PostInfo (pOutputFile))
import Data.Aeson (toJSON)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Data.Void
import Logging.Logger
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Text.Mustache (compileMustacheText, renderMustache)
import qualified Text.Mustache.Type as MT
import Util.FileHelpers (normalizeFilePath)
import Util.Helpers (memoize, toPName)

fillTemplate :: Logger -> PostInfo -> Map T.Text T.Text -> FilePath -> IO ()
fillTemplate logger post templateMap templateFile = do
  compiledTemplate <- getFullTemplate logger templateFile
  let filledTemplate = renderMustache compiledTemplate (toJSON templateMap)
  LTIO.writeFile (pOutputFile post) filledTemplate

getFullTemplate :: Logger -> FilePath -> IO MT.Template
getFullTemplate logger path =
  foldr1 (<>)
    <$> (mapM (compileTemplateFile logger) =<< getTemplateDependencies path)

getTemplateDependencies :: FilePath -> IO [FilePath]
getTemplateDependencies = undefined

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
        let identifier = toPName (normalizeFilePath path)
        return $ compileMustacheText identifier templateSrc
    )
