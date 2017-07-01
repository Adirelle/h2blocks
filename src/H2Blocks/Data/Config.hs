module H2Blocks.Data.Config
    ( GlobalConfig(..)
    , Config(..)
    )
    where

import Data.Aeson
import Data.Aeson.Types

import H2Blocks.Data.Duration

--------------------------------------------------------------------------------
-- Global configuration
--------------------------------------------------------------------------------

data GlobalConfig = GlobalConfig
    { interval   :: Duration
    , stopSignal :: Maybe Int
    , contSignal :: Maybe Int
    }
    deriving (Eq, Show)

instance FromJSON GlobalConfig where
    parseJSON = withObject "Config.global" $ \o -> do
        i  <- o .:? "interval"    .!= seconds 1
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
