module Logging.Messages (
  noKatexWarning,
  templateNotFound,
  templateCompilationFailed,
  missingMetadataKey,
) where

import System.Console.ANSI (hyperlinkCode)

noKatexWarning :: String
noKatexWarning =
  "Could not find KaTeX resources in the template, you might want to " ++ asHyperlink "include the KaTeX CSS" "https://katex.org/docs/browser" ++ "."

templateNotFound :: String -> String
templateNotFound = (++) "Could not find template "

missingMetadataKey :: String -> String
missingMetadataKey k = "Missing " ++ k ++ " key in metadata"

templateCompilationFailed :: String
templateCompilationFailed = "Could not compile template!"

asHyperlink :: String -> String -> String
asHyperlink text url = if null link then text else link
 where
  link = hyperlinkCode url text
