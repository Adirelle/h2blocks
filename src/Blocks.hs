module Blocks
    ( BlockConfig
    , buildBlock
    ) where

import Conduit
import Data.Aeson
import Data.Aeson.Types

import Blocks.Time
import Types

data BlockConfig
    = Time Delay Text
    deriving (Eq, Show)
    
instance FromJSON BlockConfig where
    parseJSON = withObject "block" $ \o -> do
        t <- o .: "type"
        parseConfig t o

parseConfig :: Text -> Object -> Parser BlockConfig

parseConfig "time" o = Time
    <$> o .:? "interval" .!= 1
    <*> o .:? "format"   .!= "%Y-%M-%D %h:%m"

parseConfig t _ = fail $ "Invalid block type: " ++ unpack t

buildBlock :: (MonadBaseControl IO m, MonadIO m) => BlockConfig -> m (Source m Block)
buildBlock (Time i f) = return $ timeBlock i f
