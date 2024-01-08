module Util.MathHelpers where

scaleToHeight :: Double -> Double -> Double -> Double
scaleToHeight newHeight oldHeight oldWidth = newHeight * (oldWidth / oldHeight)

scaleToPixelHeight :: Int -> Double -> Double -> Int
scaleToPixelHeight newHeight oldHeight oldWidth =
  round $ scaleToHeight (fromIntegral newHeight) oldHeight oldWidth
