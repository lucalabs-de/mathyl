{-# LANGUAGE OverloadedStrings #-}

module Parsers.MetadataParser where

import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Map (Map, fromList, toList)
import Data.Text (Text)
import Text.Pandoc (
  Block (..),
  Meta (..),
  MetaValue (MetaInlines),
  Pandoc (..),
  def,
  nullMeta,
  runPure,
  writeMarkdown,
 )

-- | Parses the Markdown metadata block and returns it as a map
parseMetadata :: Pandoc -> Map Text Text
parseMetadata (Pandoc meta _) = fromList $ parseMetadata_ (unMeta meta)
 where
  parseMetadata_ metaMap =
    toList metaMap <&> \(k, v) -> case v of
      MetaInlines i ->
        -- since we read from markdown, this should give us exactly what the user wrote
        (k, fromRight "" $ runPure $ writeMarkdown def (Pandoc nullMeta [Plain i]))
      _ -> (k, "")
