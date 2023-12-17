{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TikzCompiler where

import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import Data.ByteString.Char8 (hPutStrLn)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import qualified System.IO as FD (stderr)
import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import Util.Files (tikzTemplate)
import System.FilePath ((-<.>))

data TikzImage = TikzImage
  { t_source :: T.Text
  , t_libraries :: [T.Text]
  , t_packages :: [T.Text]
  , t_file :: FilePath
  }
  deriving (Show)

compileTikzImage :: TikzImage -> IO ()
compileTikzImage img = do
  let template = $(TH.compileMustacheText "tikz" tikzTemplate)
  let filledTemplate = renderMustache template $ object ["source" .= t_source img, "libraries" .= t_libraries img, "packages" .= t_packages img]
  TIO.writeFile (t_file img -<.> ".tex") filledTemplate
  return ()
