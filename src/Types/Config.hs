module Types.Config
    ( GlobalConfig(..)
    , Config(..)

    , Delay(..)
    , toMicroseconds
    )
    where

import Data.Aeson
import Data.Aeson.Types
import Data.Scientific

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

type Delay = Scientific

toMicroseconds :: Delay -> Int
toMicroseconds = truncate . (*) 1e6

--------------------------------------------------------------------------------
-- Global configuration
--------------------------------------------------------------------------------

data GlobalConfig = GlobalConfig
    { interval   :: Delay
    , stopSignal :: Maybe Int
    , contSignal :: Maybe Int
    }
    deriving (Eq, Show)

instance FromJSON GlobalConfig where
    parseJSON = withObject "Config.global" $ \o -> do
        i  <- o .:? "interval"    .!= 1
        ss <- o .:? "stop_signal"
        cs <- o .:? "cont_signal"
        return $ GlobalConfig i ss cs

defaultGlobalConfig = GlobalConfig 1 Nothing Nothing

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

data Config b = Config
    { global :: GlobalConfig
    , blocks :: Vector b
    }
    deriving (Eq, Show)

instance FromJSON b => FromJSON (Config b) where
    parseJSON = withObject "Config" $ \o -> do
        global <- o .:? "global" .!= defaultGlobalConfig
        blocks <- o .:  "blocks"
        return $ Config global blocks
