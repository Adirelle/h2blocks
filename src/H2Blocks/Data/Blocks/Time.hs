{-# LANGUAGE MultiParamTypeClasses #-}

module H2Blocks.Data.Blocks.Time
    ( TimeConfig
    , buildTimeBlock
    ) where

import Data.Aeson
import Data.Time
import Data.Time.Format

import H2Blocks.Data.Builder         as B
import H2Blocks.Data.Config
import H2Blocks.Data.I3BarIPC.Output

newtype TimeConfig = TimeConfig Text

instance FromJSON TimeConfig where
    parseJSON = withObject "block" $ \o ->
        TimeConfig
        <$> o .:? "format"   .!= "%F %T %z"

buildTimeBlock :: MonadIO m => TimeConfig -> B.Builder m BlockProducer
buildTimeBlock (TimeConfig fmt) = do
    timeZone <- liftIO getCurrentTimeZone
    let chTz = utcToZonedTime timeZone
        format = pack . formatTime defaultTimeLocale (unpack fmt)
        producer = do
            now <- liftIO getCurrentTime
            let timeRepr = format (chTz now)
            return emptyBlock { fullText = timeRepr }
    return producer
