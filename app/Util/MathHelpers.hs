module Util.MathHelpers where

scaleToDimension :: Double -> Double -> Double -> Double
scaleToDimension newRefDim oldRefDim oldDim = (newRefDim / oldRefDim) * oldDim

scaleToPixelDimension :: Int -> Double -> Double -> Int
scaleToPixelDimension newRefDim oldRefDim oldDim =
  round $ scaleToDimension (fromIntegral newRefDim) oldRefDim oldDim

scaleToPixelHeight :: Int -> Double -> Double -> Int
scaleToPixelHeight = scaleToPixelDimension

scaleToPixelWidth :: Int -> Double -> Double -> Int
scaleToPixelWidth = scaleToPixelDimension
