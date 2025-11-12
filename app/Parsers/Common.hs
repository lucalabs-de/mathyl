module Parsers.Common where

import Control.Monad.Combinators (some)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
  Parsec,
  many,
  noneOf,
  sepBy1,
  (<?>),
 )
import Text.Megaparsec.Char

type Parser = Parsec Void Text

pFileName :: Parser String
pFileName = some (noneOf ['{', '}', '<', '>', '/', '"'])

pFilePath :: Parser FilePath
pFilePath =
  concat3
    <$> many (char '/')
    <*> (intercalate "/" <$> sepBy1 pFileName (many (char '/')))
    <*> many (char '/')
      <?> "file path"

concat3 :: String -> String -> String -> String
concat3 x y z = x ++ y ++ z
