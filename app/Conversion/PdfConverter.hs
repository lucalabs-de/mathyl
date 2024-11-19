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
  scale,
  setSourceRGBA,
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
import Util.MathHelpers (scaleToPixelHeight, scaleToPixelWidth)

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

convertPdfToPng :: FilePath -> Maybe Int -> Maybe Int -> IO ()
convertPdfToPng inPath height width = do
  absPath <- T.pack <$> pathToFileScheme inPath
  doc <- documentNewFromFile absPath Nothing
  page <- documentGetPage doc 0

  (pdfHeight, pdfWidth) <- pageGetSize page

  let (pngHeight, pngWidth) = case (height, width) of
        (Just h, Just w) -> (h, w)
        (Just h, Nothing) -> (h, scaleToPixelHeight h pdfHeight pdfWidth)
        (Nothing, Just w) -> (scaleToPixelWidth w pdfWidth pdfHeight, w)
        (Nothing, Nothing) -> (round pdfHeight, round pdfWidth)

  let scaleH = fromIntegral pngHeight / pdfHeight
  let scaleW = fromIntegral pngWidth / pdfWidth

  let outPath = inPath -<.> ".png"

  withImageSurface
    FormatARGB32
    pngWidth
    pngHeight
    \surface -> do
      clearSurface surface pngHeight pngWidth
      renderPdfToSurface scaleH scaleW page surface
      surfaceWriteToPNG surface outPath

clearSurface :: Surface -> Int -> Int -> IO ()
clearSurface surface height width = renderWith surface $ do
  setSourceRGBA 255 255 255 0
  rectangle 0 0 (fromIntegral height) (fromIntegral width)
  fill

renderPdfToSurface :: Double -> Double -> Page -> Surface -> IO ()
renderPdfToSurface scaleH scaleW page surface = renderWith surface $ do
  scale scaleW scaleH
  toRender $ pageRender page
