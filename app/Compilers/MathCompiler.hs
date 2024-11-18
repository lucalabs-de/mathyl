{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compilers.MathCompiler (processMathBlocks) where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value (..))
import Data.ByteString (ByteString)
import Data.Text (Text, replace)
import Data.Text.Encoding (encodeUtf8)
import Quickjs (JSContextPtr, eval, eval_, quickjs)
import Text.Pandoc (Format (..), Inline (..), MathType (..), Pandoc (..))
import Text.Pandoc.Walk

import Util.Files (katexJs)

data MathRenderType = Inline | Display

processMathBlocks :: (MonadIO m, MonadThrow m, MonadMask m) => Pandoc -> m Pandoc
processMathBlocks ast = quickjs $ do
  eval_ katexJs

  flip handleMathBlocks ast $ \math rdrMode -> do
    mathHtml <- eval (mkFunctionCall math rdrMode)
    return $ RawInline (Format "html") (fromStringValue mathHtml)

handleMathBlocks ::
  forall m.
  (MonadReader (JSContextPtr) m) =>
  (MathRenderType -> Text -> m Inline) ->
  Pandoc ->
  m Pandoc
handleMathBlocks f = walkM parseMath 
 where
  parseMath :: Inline -> m Inline
  parseMath (Math InlineMath str) = f Inline str
  parseMath (Math DisplayMath str) = f Display str
  parseMath p = return p

mkFunctionCall :: MathRenderType -> Text -> ByteString
mkFunctionCall Inline m =
  "katex.renderToString(\""
    <> encodeUtf8 (replace "\\" "\\\\" m)
    <> "\")"
mkFunctionCall Display m =
  "katex.renderToString(\""
    <> encodeUtf8 (replace "\\" "\\\\" m)
    <> "\", { displayMode: true })"

fromStringValue :: Value -> Text
fromStringValue (String t) = t
fromStringValue _ = ""
