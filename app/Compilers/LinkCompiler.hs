module Compilers.LinkCompiler where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isRelative, takeBaseName, takeDirectory, takeExtension, (-<.>), (</>))
import Text.Pandoc (Block, Inline (Link), Pandoc (..))
import Text.Pandoc.Walk (Walkable (..))

import Settings.Options (Settings (oNiceUrls))
import Util.FileHelpers (isProbablyLocalPath)

processHyperlinks :: (MonadReader Settings m) => Pandoc -> m Pandoc
processHyperlinks (Pandoc meta ast) = do
  niceUrls <- asks oNiceUrls

  let updatedAst = replaceLinks niceUrls ast
  return $ Pandoc meta updatedAst

replaceLinks :: Bool -> [Block] -> [Block]
replaceLinks niceUrls = walk replaceLinks'
 where
  replaceLinks' :: Inline -> Inline
  replaceLinks' (Link attr text (url, title)) = Link attr text (replaceDirIfNeeded url, title)
  replaceLinks' i = i

  replaceDirIfNeeded :: Text -> Text
  replaceDirIfNeeded = T.pack . replaceDirIfNeeded' . T.unpack

  replaceDirIfNeeded' :: String -> FilePath
  replaceDirIfNeeded' uri
    | needsReplacing uri && niceUrls = ".." </> takeDirectory uri </> takeBaseName uri </> "index.html"
    | needsReplacing uri = uri -<.> "html"
    | otherwise = uri

  needsReplacing :: String -> Bool
  needsReplacing uri =
    isProbablyLocalPath uri
      && isRelative uri
      && takeExtension uri `elem` [".md", ".markdown"]
