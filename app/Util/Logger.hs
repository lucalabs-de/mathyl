{-# LANGUAGE BlockArguments #-}

module Util.Logger (
  Logger,
  mkLogger,
  mkChild,
  logMsg,
  logError,
  Verbosity (..),
) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.IORef (modifyIORef, newIORef, readIORef)
import System.IO (hPutStrLn)
import qualified System.IO as FD (stderr)
import Prelude hiding (log)

data Verbosity = Error | Message deriving (Eq, Ord, Show)

data Logger = Logger
  { depth :: Int
  , verbosity :: Verbosity
  }

mkLogger :: Verbosity -> Logger
mkLogger = Logger 0

mkChild :: Logger -> Logger
mkChild l = Logger (depth l + 1) (verbosity l)

message :: (MonadIO m) => Logger -> Verbosity -> String -> m ()
message l v s = liftIO
  $ when (v >= verbosity l) case v of
    Error -> hPutStrLn FD.stderr (indent (depth l * 2) s)
    Message -> putStrLn (indent (depth l * 2) s)

logMsg :: (MonadIO m) => Logger -> String -> m ()
logMsg logger = message logger Message

logError :: (MonadIO m) => Logger -> String -> m ()
logError logger = message logger Error

indent :: Int -> String -> String
indent n m = replicate n ' ' ++ m
