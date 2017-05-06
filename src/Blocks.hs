module Blocks
    ( BlockConfig
    ) where

import Data.Aeson
--import Data.Aeson.Types

import Blocks.Time
import Types

data BlockConfig
    = Time TimeConfig
    | Memory Int
    deriving (Eq, Show)

instance FromJSON BlockConfig where
    parseJSON value = withObject "block" dispatchOnType value
        where
            dispatchOnType obj = do
                blockType <- obj .: "type"
                parseType blockType value

            parseType "time" v = Time <$> parseJSON v
            parseType t      _ = fail $ "Invalid block type: " ++ unpack t
