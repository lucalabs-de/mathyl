module Util.Helpers where

import Data.List (isSuffixOf)

endsIn :: [String] -> String -> Bool
endsIn sfxs w = any (`isSuffixOf` w) sfxs
