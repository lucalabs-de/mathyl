{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compilers.MathCompiler (processMathBlocks) where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Quickjs (JSContextPtr, eval, eval_, quickjs)
import Text.Pandoc (Block (..), Format (..), Inline (..), MathType (..), Pandoc (..))
import Text.Pandoc.Walk

import Util.Files (katexJs)

processMathBlocks :: (MonadIO m, MonadThrow m, MonadMask m) => Pandoc -> m Pandoc
processMathBlocks ast = quickjs $ do
  eval_ katexJs

  flip handleMathBlocks ast $ \math -> do
    mathHtml <- eval (mkFunctionCall math)
    return $ RawInline (Format "html") (fromStringValue mathHtml)

handleMathBlocks :: forall m. (MonadReader (JSContextPtr) m) => (Text -> m Inline) -> Pandoc -> m Pandoc
handleMathBlocks f = walkM parseMath
 where
  parseMath :: Block -> m Block
  parseMath = walkM replaceMath

  replaceMath :: Inline -> m Inline
  replaceMath (Math InlineMath str) = f str
  replaceMath (Math DisplayMath str) = f str
  replaceMath p = return p

mkFunctionCall :: Text -> ByteString
mkFunctionCall m = "renderToString(\")" <> encodeUtf8 m <> "\")"

fromStringValue :: Value -> Text
fromStringValue (String t) = t
fromStringValue _ = ""
