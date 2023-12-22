{-# LANGUAGE OverloadedStrings #-}

module Util.Helpers where

import Data.Char (isSpace)
import Data.List (isSuffixOf, intercalate)
import qualified Data.Text as T
import System.Exit (ExitCode(ExitFailure))

endsIn :: [String] -> String -> Bool
endsIn sfxs w = any (`isSuffixOf` w) sfxs

commaSeparatedToList :: T.Text -> [T.Text]
commaSeparatedToList input = T.splitOn "," $ T.pack $ filter (not . isSpace) (T.unpack input)

replace :: String -> String -> String -> String
replace needle replacement haystack =
  T.unpack
    $ T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)

indent :: Int -> String -> String
indent n m = indentChars ++ intercalate ("\n" ++ indentChars) (lines m)
  where indentChars = replicate n ' '

isErrorCode :: ExitCode -> Bool
isErrorCode (ExitFailure _) = True
isErrorCode _ = False
