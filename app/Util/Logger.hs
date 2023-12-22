{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}

module Util.Logger where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.IORef (modifyIORef, newIORef, readIORef)
import System.IO (hPutStrLn)
import qualified System.IO as FD (stderr)

data Verbosity = Error | Message deriving (Eq, Ord, Show)

data Logger = Logger
  { string :: forall m. (MonadIO m) => Verbosity -> String -> m ()
  , header :: forall m. (MonadIO m) => Verbosity -> String -> m ()
  }

mkLogger :: Verbosity -> IO Logger
mkLogger verbosity = do
  indentation <- newIORef 0

  let msg s = do
        i <- readIORef indentation
        putStrLn $ indent i s
  let err s = do
        i <- readIORef indentation
        hPutStrLn FD.stderr $ indent i s

  let log v s = when
        (verbosity >= v)
        case v of
          Error -> err s
          Message -> msg s

  return
    $ Logger
      { string = \v s -> liftIO $ log v s
      , header = \v s -> liftIO $ log v s >> modifyIORef indentation (+ 2)
      }

indent :: Int -> String -> String
indent n m = repeat ' ' ++ m
