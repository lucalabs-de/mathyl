module Settings.Options where

data Settings = Settings
  { oDefaultPngHeightInPx :: Int
  , oUseSvgs :: Bool
  , oServerSideRendering :: Bool -- TODO implement
  , oContinueOnErrors :: Bool -- TODO implement
  , oQuiet :: Bool
  }
  deriving (Show)

data UserDefinedSettings = UserDefinedSettings
  { uUseSvgs :: Bool
  , uServerSideRendering :: Bool
  , uContinueOnErrors :: Bool
  , uQuiet :: Bool
  }
  deriving (Show)

defaultSettings :: Settings
defaultSettings =
  Settings
    { oDefaultPngHeightInPx = 200
    , oServerSideRendering = False
    , oUseSvgs = False
    , oContinueOnErrors = False
    , oQuiet = True
    }

fromUserDefinedSettings :: UserDefinedSettings -> Settings
fromUserDefinedSettings u =
  defaultSettings
    { oUseSvgs = uUseSvgs u
    , oServerSideRendering = uServerSideRendering u
    , oContinueOnErrors = uContinueOnErrors u
    , oQuiet = uQuiet u
    }
