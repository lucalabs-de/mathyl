{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Logging.Logger where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.State (
  MonadTrans (lift),
  StateT (..),
  evalStateT,
  get,
  put,
 )
import qualified Data.Text as T
import System.IO (hPutStrLn)
import qualified System.IO as FD (stderr)
import Util.Helpers (indent)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)

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

newtype LoggerT m a = LoggerT {unLoggerT :: StateT Logger m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    , MonadLogger
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

class (MonadIO m) => MonadLogger m where
  logSubroutine :: m a -> m a
  startSubroutineLog :: m ()
  endSubroutineLog :: m ()
  message :: (Printable s) => Verbosity -> s -> m ()
  logMsg :: String -> m ()
  logMsgP :: (Printable s) => s -> m ()
  logError :: String -> m ()
  logErrorP :: (Printable s) => s -> m ()
  logObj :: (Show o) => o -> m ()
  logErrorObj :: (Show o) => o -> m ()

-- Implementation of MonadLogger/LoggerT in terms of State 
instance (MonadIO m) => MonadLogger (StateT Logger m) where
  startSubroutineLog = get >>= \l -> put l{depth = depth l + 1}
  endSubroutineLog = get >>= \l -> put l{depth = max (depth l - 1) 0}
  logSubroutine r = startSubroutineLog >> r >>= (\x -> endSubroutineLog >> return x)

  message v s =
    get >>= \l ->
      when
        (v >= verbosity l)
        case v of
          Error -> liftIO $ hPutStrLn FD.stderr (indent (depth l * 2) (toString s))
          Message -> liftIO $ putStrLn (indent (depth l * 2) (toString s))

  logMsg = message Message 
  logMsgP = message Message
  logError = message Error
  logErrorP = message Error
  logObj o = message Message (show o)
  logErrorObj o = message Error (show o)

instance (MonadLogger m) => MonadLogger (ReaderT e m) where
  startSubroutineLog = lift startSubroutineLog
  endSubroutineLog = lift endSubroutineLog
  logSubroutine = mapReaderT logSubroutine

  message v s = lift (message v s)

  logMsg = lift . logMsg
  logMsgP = lift . logMsgP
  logError = lift . logError
  logErrorP = lift . logErrorP
  logObj = lift . logObj
  logErrorObj = lift . logErrorObj

runLoggerT :: (Monad m, MonadIO m) => LoggerT m a -> Verbosity -> m a
runLoggerT l v = evalStateT (unLoggerT l) (Logger 0 v)
