{-# LANGUAGE OverloadedStrings #-}

module Parsers.FilePathParser where

import Text.Megaparsec (
  MonadParsec (eof, try),
  anySingle,
  many,
  (<|>),
 )
import Text.Megaparsec.Char

import Parsers.Common

pImageSrc :: Parser FilePath
pImageSrc = string "src=\"" *> pFilePath

pImagePaths :: Parser [FilePath]
pImagePaths = many (try loop) <|> ([] <$ eof)
 where
  loop = pImageSrc <|> (anySingle *> loop)
