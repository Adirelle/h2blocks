module H2Blocks.Data.Blocks
    ( BlockConfig
    , buildBlock
    ) where

import Data.Aeson
import Data.Aeson.Types

import H2Blocks.Data.Blocks.Memory
import H2Blocks.Data.Blocks.Time
import H2Blocks.Data.Builder       as B

data BlockConfig
    = Time TimeConfig
    | Memory MemoryConfig

instance FromJSON BlockConfig where
    parseJSON value =
        withObject "block" dispatchOnType value
        where
            dispatchOnType :: Object -> Parser BlockConfig
            dispatchOnType obj = do
                t <- (obj .: "type") :: Parser Text
                case t of
                    "time"   -> Time <$> parseJSON value
                    "memory" -> Memory <$> parseJSON value
                    _        -> fail $ "Invalid block type: " ++ unpack t

buildBlock :: MonadIO m => BlockConfig -> B.Builder m BlockProducer
buildBlock (Time c)   = buildTimeBlock c
buildBlock (Memory c) = buildMemoryBlock c
