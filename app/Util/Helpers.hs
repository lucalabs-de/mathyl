{-# LANGUAGE OverloadedStrings #-}

module Util.Helpers where

import Data.Char (isSpace)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (dropWhileEnd, intercalate, isSuffixOf)
import Data.Map ((!?))
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.IO.Unsafe (unsafePerformIO)
import System.Exit (ExitCode (ExitFailure))
import Text.Mustache (PName (PName))

--- String Helpers ---
endsIn :: [String] -> String -> Bool
endsIn sfxs w = any (`isSuffixOf` w) sfxs

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

commaSeparatedToList :: T.Text -> [T.Text]
commaSeparatedToList input = T.splitOn "," $ T.pack $ filter (not . isSpace) (T.unpack input)

replace :: String -> String -> String -> String
replace needle replacement haystack =
  T.unpack $
    T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)

indent :: Int -> String -> String
indent n m = indentChars ++ intercalate ("\n" ++ indentChars) (lines m)
 where
  indentChars = replicate n ' '

toPName :: String -> PName
toPName = PName . T.pack

--- Type Helpers ---
isErrorCode :: ExitCode -> Bool
isErrorCode (ExitFailure _) = True
isErrorCode _ = False

{- | If the outer Either is Left x returns Left x; if the outer Either is Right ys returns
Right ys if there are no Lefts in ys, and otherwise Left z for the first Left z in ys
-}
flattenEithers :: Either a [Either a c] -> Either a [c]
flattenEithers = either Left sequence

--- Optimization ---
memoizeIO :: (Ord a) => (a -> IO b) -> IO (a -> IO b)
memoizeIO f = do
  c <- newIORef Map.empty
  let f' x = do
        cache <- readIORef c
        let memoized = cache !? x
        case memoized of
          Just m -> return m
          Nothing -> do
            y <- f x
            writeIORef c (Map.insert x y cache)
            return y
  return f'

-- This is not great, maybe try to find an alternative solution
memoize :: (Ord a) => (a -> IO b) -> (a -> IO b)
memoize = unsafePerformIO . memoizeIO
