{-# LANGUAGE TemplateHaskell #-}

module Util.Files where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

-- Mustache template for a standalone tikZ image 
tikzTemplate :: T.Text
tikzTemplate = decodeUtf8 $(embedFile "assets/template/figure-template.tex")

-- Minified KaTeX source
katexJs :: ByteString
katexJs = $(embedFile "vendor/katex/katex.min.js")
