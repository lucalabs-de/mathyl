{-# LANGUAGE OverloadedStrings #-}

module Parsers.MustachePartialParser where

import Control.Monad.Combinators (some)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (
  MonadParsec (eof, try),
  Parsec,
  anySingle,
  many,
  noneOf,
  sepBy1,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char

type Parser = Parsec Void Text

pPartialBeg :: Parser String
pPartialBeg = T.unpack <$> string "{{>" <* many (char ' ')

pPartialEnd :: Parser String
pPartialEnd = many (char ' ') *> (T.unpack <$> string "}}")

pFileName :: Parser String
pFileName = some (noneOf ['{', '}', '<', '>', '/'])

pFilePath :: Parser String
pFilePath =
  concat3
    <$> many (char '/')
    <*> (intercalate "/" <$> sepBy1 pFileName (many (char '/')))
    <*> many (char '/')
      <?> "file path"

pPartial :: Parser String
pPartial = pPartialBeg *> pFilePath <* pPartialEnd

pPartials :: Parser [String]
pPartials = many (try loop) <|> ([] <$ eof)
 where
  loop = pPartial <|> (anySingle *> loop)

concat3 :: String -> String -> String -> String
concat3 x y z = x ++ y ++ z
