{-# LANGUAGE MultiParamTypeClasses #-}

module H2Blocks.Data.Block.Time
    ( TimeConfig
    , buildTimeBlock
    ) where

import Data.Aeson
import Data.Time
import Data.Time.Format

import H2Blocks.Data.Block as B

newtype TimeConfig = TimeConfig Text

instance FromJSON TimeConfig where
    parseJSON = withObject "block" $ \o ->
        TimeConfig
        <$> o .:? "format"   .!= "%F %T %z"

buildTimeBlock :: MonadIO m => TimeConfig -> m BlockSpec
buildTimeBlock (TimeConfig fmt) = do
    timeZone <- liftIO getCurrentTimeZone
    let chTz = utcToZonedTime timeZone
        sFormat = formatTime defaultTimeLocale (unpack fmt)
        format = pack . sFormat . chTz
    return $ fromPipe (pipe format)
    where
        pipe format = B.for cat $ \_ -> do
            now <- liftIO getCurrentTime
            yield $ Set $ emptyBlock { fullText = format now }
