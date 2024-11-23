{-# LANGUAGE BlockArguments #-}

module Util.FileHelpers where

import Control.Exception (catch, throwIO)
import qualified Data.Text as T
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  doesDirectoryExist,
  listDirectory,
  makeAbsolute,
  removeDirectoryRecursive,
  removeFile,
 )
import System.FilePath (
  isValid,
  joinPath,
  normalise,
  splitDirectories,
  takeBaseName,
  takeDirectory,
  takeExtension,
  (</>),
 )
import System.IO.Error (isDoesNotExistError)

import Data.Bifunctor (second)
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Util.Helpers (startsWith)

copyAndCreateParents :: FilePath -> FilePath -> IO ()
copyAndCreateParents from to =
  createDirectoryIfMissing True (takeDirectory to) >> copyFile from to

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir =
  concat
    <$> ( listDirectory dir >>= mapM \f -> do
            let fPath = dir </> f
            isDir <- doesDirectoryExist fPath
            if isDir then listDirectoryRecursive fPath else return [fPath]
        )

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

normalizeFilePath :: FilePath -> FilePath
normalizeFilePath path = joinPath $ removeDetours $ splitDirectories $ normalise path
 where
  removeDetours (".." : dirs) = ".." : removeDetours dirs
  removeDetours (_ : ".." : dirs) = removeDetours dirs
  removeDetours (a : dirs) = a : removeDetours dirs
  removeDetours [] = []

-- | Computes the shortest relative path from @p source to @p target
getRelativePath :: FilePath -> FilePath -> FilePath
getRelativePath source target =
  normalizeFilePath $
    joinPath $
      replicate (length sourceDirs - length commonPrefix) ".."
        ++ fromJust (stripPrefix commonPrefix targetDirs)
 where
  sourceDirs = splitDirectories $ normalise source
  targetDirs = splitDirectories $ normalise target

  commonPrefix =
    fst
      <$> takeWhile
        (uncurry (==))
        (zip sourceDirs targetDirs)

-- | Replaces the topmost occurrence of @p oldDir by @p newDir
replaceTopDirectory :: FilePath -> FilePath -> FilePath -> FilePath
replaceTopDirectory oldDir newDir input
  | T.null back = input
  | otherwise = (T.unpack . T.concat) [front, newDirT, T.drop (length oldDir) back]
 where
  (front, back) = T.breakOn (T.pack oldDir) inputT
  inputT = T.pack input
  newDirT = T.pack newDir

pathToFileScheme :: FilePath -> IO String
pathToFileScheme path = ("file://" ++) <$> makeAbsolute path

-- | Takes a URI and tries to determine whether it is a local path as opposed to a web URL
isProbablyLocalPath :: String -> Bool
isProbablyLocalPath p = isValid p && (not . startsWith ["https://", "http://"]) p
