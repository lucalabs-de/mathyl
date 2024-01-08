module Settings.Options where

data Options = Options
  { oDefaultPngHeightInPx :: Int
  , oServerSideRendering :: Bool -- TODO implement
  , oUseSvgs :: Bool
  , oContinueOnErrors :: Bool -- TODO implement
  }

defaultSettings :: Options
defaultSettings =
  Options
    { oDefaultPngHeightInPx = 200
    , oServerSideRendering = False
    , oUseSvgs = False
    , oContinueOnErrors = False
    }
