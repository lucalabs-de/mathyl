module Conversion.PdfConverter (convertPdfToSvg, convertPdfToPng) where

import qualified Data.Text as T
import GI.Poppler
import Util.FileHelpers (pathToFileScheme)
import GI.Cairo.Render (Surface, withSVGSurface, renderWith, showPage, withImageSurface, Format (FormatARGB32))
import GI.Cairo.Render.Connector (toRender)
import System.FilePath ((-<.>))
import System.Process (shell, readCreateProcessWithExitCode, callCommand)
import System.Exit (ExitCode(..))

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
  callCommand =<< svgConversionCommand inPath outPath
  

svgConversionCommand :: FilePath -> FilePath -> IO String
svgConversionCommand inPath outPath = do 
  let proc1 = shell "dvisvgm --version"
  (exitCode1, _, _) <- readCreateProcessWithExitCode proc1 ""

  let proc2 = shell "pdf2svg --version" 
  (exitCode2, _, _) <- readCreateProcessWithExitCode proc2 ""

  case (exitCode1, exitCode2) of
    (ExitSuccess, _) -> return $ "dvisvgm --pdf " ++ inPath
    (_, ExitSuccess) -> return $ "pdf2svg " ++ inPath ++ " " ++ outPath
    _ -> error "No PDF -> SVG conversion tool available"

  

convertPdfToPng :: FilePath -> IO ()
convertPdfToPng inPath = do
  absPath <- T.pack <$> pathToFileScheme inPath
  doc <- documentNewFromFile absPath Nothing
  page <- documentGetPage doc 0
  (height, width) <- pageGetSize page

  withImageSurface FormatARGB32 height width (renderPdfToSurface page)

renderPdfToSurface :: Page -> Surface -> IO ()
renderPdfToSurface page surface = renderWith surface $ do
  toRender $ pageRenderForPrinting page
  showPage
