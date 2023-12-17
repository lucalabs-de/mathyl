module TikzCompiler where

import qualified Data.Text as T

newtype TikzSource = TikzSource T.Text deriving (Show)

compileTikzImage :: TikzSource -> IO ()
compileTikzImage = undefined
