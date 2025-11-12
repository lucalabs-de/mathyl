module Settings.Options where

data Settings = Settings
  { oDefaultPngHeightInPx :: Int
  , oUseSvgs :: Bool
  , oNiceUrls :: Bool 
  , oContinueOnErrors :: Bool -- TODO implement
  , oQuiet :: Bool
  }
  deriving (Show)

data UserDefinedSettings = UserDefinedSettings
  { uUseSvgs :: Bool
  , uNiceUrls :: Bool
  , uContinueOnErrors :: Bool
  , uQuiet :: Bool
  }
  deriving (Show)

defaultSettings :: Settings
defaultSettings =
  Settings
    { oDefaultPngHeightInPx = 200
    , oUseSvgs = False
    , oNiceUrls = False
    , oContinueOnErrors = False
    , oQuiet = True
    }

fromUserDefinedSettings :: UserDefinedSettings -> Settings
fromUserDefinedSettings u =
  defaultSettings
    { oUseSvgs = uUseSvgs u
    , oNiceUrls = uNiceUrls u
    , oContinueOnErrors = uContinueOnErrors u
    , oQuiet = uQuiet u
    }
