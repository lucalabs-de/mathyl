{-# LANGUAGE OverloadedStrings #-}

module Util.Helpers where

import Data.Char (isSpace)
import Data.List (isSuffixOf)
import qualified Data.Text as T

endsIn :: [String] -> String -> Bool
endsIn sfxs w = any (`isSuffixOf` w) sfxs

commaSeparatedToList :: T.Text -> [T.Text]
commaSeparatedToList input = T.splitOn "," $ T.pack $ filter (not . isSpace) (T.unpack input)

replace :: String -> String -> String -> String
replace needle replacement haystack =
  T.unpack
    $ T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)
