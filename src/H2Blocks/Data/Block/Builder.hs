module H2Blocks.Data.Block.Builder
    ( BlockConfig
    , buildBlock
    ) where

import Data.Aeson
import Data.Aeson.Types

import H2Blocks.Data.Block
--import H2Blocks.Data.Block.Memory
import H2Blocks.Data.Block.Time

data BlockConfig
    = Time TimeConfig
--    | Memory MemoryConfig

instance FromJSON BlockConfig where
    parseJSON value =
        withObject "block" dispatchOnType value
        where
            dispatchOnType :: Object -> Parser BlockConfig
            dispatchOnType obj = do
                t <- (obj .: "type") :: Parser Text
                case t of
                    "time" -> Time <$> parseJSON value
--                    "memory" -> Memory <$> parseJSON value
                    _      -> fail $ "Invalid block type: " ++ unpack t

buildBlock :: MonadIO m => BlockConfig -> m BlockSpec
buildBlock (Time c)   = buildTimeBlock c
--buildBlock (Memory c) = buildMemoryBlock c
