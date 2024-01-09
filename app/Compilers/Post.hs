module Compilers.Post where

data PostInfo = PostInfo
  { pFileName :: FilePath -- The name of the post
  , pInputFile :: FilePath -- The path of the input post
  , pOutputFile :: FilePath -- The path to the output. Same relative path as inputFile, but starting at the compile output directory
  , pInputDir :: FilePath -- The directory of the input post
  , pOutputDir :: FilePath -- The directory of the output file
  , pAssetDir :: FilePath -- Directory for storing assets such as images
  , pAssetDirName :: FilePath -- The name of the asset directory, not the full path 
  }
