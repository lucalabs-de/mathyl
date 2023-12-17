module Util.Helpers where

endsIn :: [String] -> String -> Bool
endsIn sfxs w = any (`isSuffixOf` w) sfxs

