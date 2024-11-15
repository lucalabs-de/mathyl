{-# LANGUAGE BlockArguments #-}

module Conversion.PdfConverter (convertPdfToSvg, convertPdfToPng) where

import Control.Monad (when)
import qualified Data.Text as T
import GI.Cairo.Render (
  Format (FormatARGB32),
  Surface,
  fill,
  rectangle,
  renderWith,
  setSourceRGBA,
  showPage,
  surfaceWriteToPNG,
  withImageSurface,
 )
import GI.Cairo.Render.Connector (toRender)
import GI.Poppler
import System.Exit (ExitCode (..))
import System.FilePath ((-<.>))
import System.Process (readCreateProcessWithExitCode, shell)
import Util.FileHelpers (pathToFileScheme)
import Util.Helpers (isErrorCode)
import Util.MathHelpers (scaleToPixelHeight)

-- This should work, exactly like convert to png does. It seems that
-- gi-cairo-render is broken. Unfortunately the maintainer is no longer active.
-- convertPdfToSvg :: FilePath -> IO ()
-- convertPdfToSvg inPath = do
--   absPath <- T.pack <$> pathToFileScheme inPath
--   doc <- documentNewFromFile absPath Nothing
--   page <- documentGetPage doc 0
--   (height, width) <- pageGetSize page
--
--   let outPath = inPath -<.> ".svg"
--
--   withSVGSurface outPath height width (renderPdfToSurface page)

-- This is a workaround until a fix for gi-cairo-render is available.
-- Requires dvisvgm or pdf2svg to be installed
convertPdfToSvg :: FilePath -> IO ()
convertPdfToSvg inPath = do
  let outPath = inPath -<.> ".svg"

  proc <- shell <$> svgConversionCommand inPath outPath
  (exitCode, _, _) <- readCreateProcessWithExitCode proc ""

  -- TODO return the exception as Either instead
  when (isErrorCode exitCode) $ error "error occured on svg creation"

svgConversionCommand :: FilePath -> FilePath -> IO String
svgConversionCommand inPath outPath = do
  let proc1 = shell "dvisvgm --version"
  (exitCode1, _, _) <- readCreateProcessWithExitCode proc1 ""

  let proc2 = shell "pdf2svg --version"
  (exitCode2, _, _) <- readCreateProcessWithExitCode proc2 ""

  case (exitCode1, exitCode2) of
    (ExitSuccess, _) -> return $ "dvisvgm --pdf " ++ inPath ++ " -o " ++ outPath
    (_, ExitSuccess) -> return $ "pdf2svg " ++ inPath ++ " " ++ outPath
    _ -> error "No PDF -> SVG conversion tool available"

convertPdfToPng :: FilePath -> Int -> IO ()
convertPdfToPng inPath pngHeight = do
  absPath <- T.pack <$> pathToFileScheme inPath
  doc <- documentNewFromFile absPath Nothing
  page <- documentGetPage doc 0

  (pdfHeight, pdfWidth) <- pageGetSize page
  let pngWidth = scaleToPixelHeight pngHeight pdfWidth pdfHeight

  let outPath = inPath -<.> ".png"

  withImageSurface
    FormatARGB32
    pngHeight
    pngWidth
    \surface -> do
      clearSurface surface pngHeight pngWidth
      renderPdfToSurface page surface
      surfaceWriteToPNG surface outPath

clearSurface :: Surface -> Int -> Int -> IO ()
clearSurface surface height width = renderWith surface $ do
  setSourceRGBA 255 255 255 0
  rectangle 0 0 (fromIntegral height) (fromIntegral width)
  fill

renderPdfToSurface :: Page -> Surface -> IO ()
renderPdfToSurface page surface = renderWith surface $ do
  toRender $ pageRenderForPrinting page
  showPage
