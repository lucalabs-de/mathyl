module Templates (fillTemplate) where

import Data.Map (Map, foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as FD (stderr)
import Data.Void
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Text.Mustache (compileMustacheText)
import qualified Text.Mustache.Type as MT
import Util.FileHelpers (normalizeFilePath)
import Util.Helpers (memoize, memoizeIO, toPName)
import System.IO (hPutStrLn, hPrint)

fillTemplate :: Map T.Text T.Text -> FilePath -> FilePath -> IO ()
fillTemplate templateMap templateFile outDir = do
  compiledTemplate <- getFullTemplate templateFile
  undefined

getFullTemplate :: FilePath -> IO MT.Template
getFullTemplate path = foldr1 (<>) <$> (mapM compileTemplateFile =<< getTemplateDependencies path)

getTemplateDependencies :: FilePath -> IO [FilePath]
getTemplateDependencies = undefined  

compileTemplateFile :: FilePath -> IO MT.Template
compileTemplateFile path = do templateRes <- getCompiledTemplateByFile path
                              case templateRes of 
                                Left bundle -> do hPutStrLn FD.stderr "  Could not compile template!"
                                                  hPutStrLn FD.stderr (errorBundlePretty bundle)
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
