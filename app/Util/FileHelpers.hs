module Util.FileHelpers where

import Control.Exception (catch, throwIO)
import System.Directory (listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO.Error (isDoesNotExistError)

deleteAllExceptFileExtensions :: FilePath -> [String] -> String -> IO ()
deleteAllExceptFileExtensions dir keepExtensions fileName = do
  files <-
    map (dir </>)
      . filter (\f -> takeExtension f `notElem` keepExtensions)
      . filter (\f -> takeBaseName f == fileName)
      <$> listDirectory dir

  mapM_ removeFile files

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists dir = removeDirectoryRecursive dir `catch` handleExists
 where
  handleExists e
    | isDoesNotExistError e = return ()
    | otherwise = throwIO e
