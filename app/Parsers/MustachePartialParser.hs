{-# LANGUAGE OverloadedStrings #-}

module Parsers.MustachePartialParser where

import Control.Monad.Combinators (between, skipManyTill, someTill)
import qualified Data.Text as T
import Text.Megaparsec (
  MonadParsec (eof, lookAhead, try),
  anySingle,
  many,
  noneOf,
  (<|>),
 )
import Text.Megaparsec.Char

import Parsers.Common

data TemplateInfo = TemplateInfo
  { partials :: [FilePath]
  , containsKatexInfo :: Bool
  }
  deriving (Show)

instance Semigroup TemplateInfo where
  (<>) t1 t2 =
    TemplateInfo
      { partials = partials t1 <> partials t2
      , containsKatexInfo = containsKatexInfo t1 || containsKatexInfo t2
      }

pPartialBeg :: Parser String
pPartialBeg = T.unpack <$> string "{{>" <* many (char ' ')

pPartialEnd :: Parser String
pPartialEnd = many (char ' ') *> (T.unpack <$> string "}}")

pPartial :: Parser FilePath
pPartial = pPartialBeg *> pFilePath <* pPartialEnd

pPartials :: Parser [FilePath]
pPartials = many (try loop) <|> ([] <$ eof)
 where
  loop = pPartial <|> (anySingle *> loop)

pKatexCss :: Parser String
pKatexCss =
  skipManyTill anySingle
    $ between
      (string "<link")
      (string ">")
    $ lookAhead (skipManyTill anySingle (string' "rel=\"stylesheet\""))
      *> skipManyTill anySingle (string' "href=\"")
      *> someTill anySingle (char '"')
      <* many (noneOf ['>'])

pKatexInfo :: Parser Bool
pKatexInfo = (True <$ try pKatexCss) <|> pure False

pTemplateInfo :: Parser TemplateInfo
pTemplateInfo = flip TemplateInfo <$> lookAhead pKatexInfo <*> pPartials
