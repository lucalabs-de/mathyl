{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}

module Compilers.Templates (fillTemplate, fillStandaloneTemplate) where

import Compilers.Post (PostInfo (pOutputDir, pOutputFile))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (toJSON, Value)
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
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)
import Text.Mustache (compileMustacheFile, compileMustacheText, renderMustache)
import qualified Text.Mustache.Type as MT

import qualified Logging.Messages as Msg
import Parsers.FilePathParser (pImagePaths)
import Util.FileHelpers (copyAndCreateParents, normalizeFilePath, replaceTopDirectory)
import Util.Helpers (
  extractF,
  flattenEithers,
  memoize,
  toPName,
  trim,
 )

data TemplateImage = TemplateImage
  { tiTemplatePath :: FilePath
  , tiImagePath :: FilePath
  }
  deriving (Show)

data FullTemplate = FullTemplate
  { eTemplate :: MT.Template
  , eTemplateInfo :: TemplateInfo
  , eImages :: [TemplateImage]
  }
  deriving (Show)

fillStandaloneTemplate ::
  (MonadLogger m, MonadIO m) =>
  FilePath ->
  FilePath ->
  Value ->
  FilePath ->
  m ()
fillStandaloneTemplate inDir outDir mappings file = do
  compiledTemplate <- compileMustacheFile file

  let filledTemplate = renderMustache compiledTemplate mappings
  let outputFile = replaceTopDirectory inDir outDir file 

  liftIO $ createDirectoryIfMissing True (takeDirectory outputFile)
  liftIO $ LTIO.writeFile outputFile filledTemplate

fillTemplate :: (MonadLogger m, MonadIO m) => PostInfo -> Map T.Text T.Text -> FilePath -> m ()
fillTemplate post templateMap templateFile = do
  fullTemplate <- getFullTemplate templateFile

  let compiledTemplate = eTemplate fullTemplate
  let templateInfo = eTemplateInfo fullTemplate

  -- TODO maybe only log this when there's actually math in the template
  unless (containsKatexInfo templateInfo) $ logMsg Msg.noKatexWarning

  let filledTemplate = renderMustache compiledTemplate (toJSON templateMap)

  liftIO $ createDirectoryIfMissing True (pOutputDir post)
  liftIO $ LTIO.writeFile (pOutputFile post) filledTemplate
  liftIO $
    mapM_
      ( \f ->
          copyAndCreateParents
            (tiTemplatePath f </> tiImagePath f)
            (pOutputDir post </> tiImagePath f)
      )
      (eImages fullTemplate)

getFullTemplate :: (MonadLogger m, MonadIO m) => FilePath -> m FullTemplate
getFullTemplate path =
  do
    -- remove any surrounding whitespace and unify file paths for memoization
    let cleanPath = normalizeFilePath $ trim path
    let cleanPathDir = takeDirectory cleanPath

    fileExists <- liftIO $ doesFileExist cleanPath
    unless fileExists do
      logError $ Msg.templateNotFound cleanPath
      error "Failed!"

    (baseTemplate, containedImgPaths) <- compileTemplateFile cleanPath
    let prefixedContainedImgPaths = TemplateImage cleanPathDir <$> containedImgPaths

    templateInfo <- liftIO $ getTemplateInfo cleanPath
    case templateInfo of
      Left bundle -> logError (errorBundlePretty bundle) >> error "Failed!"
      Right info -> do
        -- compile all partial templates and extract the image paths contained in them, prefixed by
        -- the partial's path
        compiledDependenciesWithImgs <- mapM (\p -> extractF (compileTemplateFile p, p)) (partials info)

        let compiledDependencies = fst . fst <$> compiledDependenciesWithImgs
        let depContainedImgPaths =
              concatMap
                (\((_, paths), pPath) -> TemplateImage (cleanPathDir </> pPath) <$> paths)
                compiledDependenciesWithImgs

        return
          FullTemplate
            { eTemplate = foldr (<>) baseTemplate compiledDependencies
            , eTemplateInfo = info
            , eImages = prefixedContainedImgPaths ++ depContainedImgPaths
            }

getTemplateInfo :: FilePath -> IO (Either (ParseErrorBundle T.Text Void) TemplateInfo)
getTemplateInfo path = do
  templateSrc <- TIO.readFile (trim path)
  let srcFileName = takeFileName path

  let templateInfo = runParser pTemplateInfo srcFileName templateSrc

  -- get template info for all partials
  partialsInfo <- flattenEithers <$> mapM (mapM getTemplateInfo . partials) templateInfo

  -- combine all partial info with original template info
  return $ templateInfo >>= (<$> partialsInfo) . foldr (<>)

compileTemplateFile :: (MonadLogger m, MonadIO m) => FilePath -> m (MT.Template, [FilePath])
compileTemplateFile path = do
  templateRes <- liftIO $ getCompiledTemplateByFile path
  case templateRes of
    Left bundle -> do
      logError Msg.templateCompilationFailed
      logError (errorBundlePretty bundle)
      error "Failed!"
    Right template -> return template

getCompiledTemplateByFile :: FilePath -> IO (Either (ParseErrorBundle T.Text Void) (MT.Template, [FilePath]))
getCompiledTemplateByFile =
  memoize \path -> do
    templateSrc <- TIO.readFile path

    let identifier = toPName path
    let containedImgPaths = runParser pImagePaths path templateSrc
    let compiledTemplate = compileMustacheText identifier templateSrc

    return $ liftA2 (,) compiledTemplate containedImgPaths
