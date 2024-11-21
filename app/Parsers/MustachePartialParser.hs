{-# LANGUAGE OverloadedStrings #-}

module Parsers.MustachePartialParser where

import Control.Monad.Combinators (between, skipManyTill, some, someTill)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (
  MonadParsec (eof, lookAhead, try),
  Parsec,
  anySingle,
  many,
  noneOf,
  sepBy1,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char

data TemplateInfo = TemplateInfo
  { partials :: [FilePath]
  , containsKatexInfo :: Bool
  }

instance Semigroup TemplateInfo where
  (<>) t1 t2 =
    TemplateInfo
      { partials = partials t1 <> partials t2
      , containsKatexInfo = containsKatexInfo t1 || containsKatexInfo t2
      }

type Parser = Parsec Void Text

pPartialBeg :: Parser String
pPartialBeg = T.unpack <$> string "{{>" <* many (char ' ')

pPartialEnd :: Parser String
pPartialEnd = many (char ' ') *> (T.unpack <$> string "}}")

pFileName :: Parser String
pFileName = some (noneOf ['{', '}', '<', '>', '/'])

pFilePath :: Parser FilePath
pFilePath =
  concat3
    <$> many (char '/')
    <*> (intercalate "/" <$> sepBy1 pFileName (many (char '/')))
    <*> many (char '/')
      <?> "file path"

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

concat3 :: String -> String -> String -> String
concat3 x y z = x ++ y ++ z
