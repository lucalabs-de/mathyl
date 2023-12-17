{-# LANGUAGE TemplateHaskell #-}

module Util.Files where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

tikzTemplate :: T.Text
tikzTemplate = decodeUtf8 $(embedFile "assets/template/figure-template.tex")


