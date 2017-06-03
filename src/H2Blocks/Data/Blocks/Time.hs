{-# LANGUAGE MultiParamTypeClasses #-}

module H2Blocks.Data.Blocks.Time
    ( TimeConfig
    , buildTimeBlock
    ) where

import Data.Aeson
import Data.Time
import Data.Time.Format
import Pipes

import H2Blocks.Data.Builder
import H2Blocks.Data.Config
import H2Blocks.Data.I3BarIPC.Output
import H2Blocks.Data.Pipes

newtype TimeConfig = TimeConfig Text

instance FromJSON TimeConfig where
    parseJSON = withObject "block" $ \o ->
        TimeConfig
        <$> o .:? "format"   .!= "%F %T %z"

buildTimeBlock :: TimeConfig -> BuilderIO
buildTimeBlock (TimeConfig fmt) = do
    timeZone <- liftIO getCurrentTimeZone
    let chTz = utcToZonedTime timeZone
        sFormat = formatTime defaultTimeLocale (unpack fmt)
        format = pack . sFormat . chTz
    return $ forEvents (go format)
    where
        go format Query = do
            now <- liftIO getCurrentTime
            update $ emptyBlock { fullText = format now }
        go _ _ = nop
