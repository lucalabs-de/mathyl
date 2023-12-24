{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}

module Logging.Logger where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (intercalate)
import qualified Data.Text as T
import System.IO (hPutStrLn)
import qualified System.IO as FD (stderr)
import Util.Helpers (indent)

class Printable a where
  toString :: a -> String

instance Printable String where
  toString = id

instance Printable T.Text where
  toString = T.unpack

data Verbosity = Message | Error deriving (Eq, Show)

instance Ord Verbosity where
  (<=) _ Error = True
  (<=) Message _ = True
  (<=) _ _ = False

data Logger = Logger
  { depth :: Int
  , verbosity :: Verbosity
  }
  deriving (Show)

mkLogger :: Verbosity -> Logger
mkLogger = Logger 0

mkChild :: Logger -> Logger
mkChild l = Logger (depth l + 1) (verbosity l)

message :: (MonadIO m, Printable s) => Logger -> Verbosity -> s -> m ()
message l v s = liftIO $
  when (v >= verbosity l) case v of
    Error -> hPutStrLn FD.stderr (indent (depth l * 2) (toString s))
    Message -> putStrLn (indent (depth l * 2) (toString s))

logMsg :: (MonadIO m) => Logger -> String -> m ()
logMsg logger = message logger Message

logMsgP :: (MonadIO m, Printable s) => Logger -> s -> m ()
logMsgP logger = message logger Message

logError :: (MonadIO m) => Logger -> String -> m ()
logError logger = message logger Error

logErrorP :: (MonadIO m, Printable s) => Logger -> s -> m ()
logErrorP logger = message logger Error

logErrorObj :: (MonadIO m, Show o) => Logger -> o -> m ()
logErrorObj logger obj = message logger Error (show obj)
