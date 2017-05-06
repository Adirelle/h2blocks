{-# LANGUAGE MultiParamTypeClasses #-}

module Blocks.Time
    ( TimeBlock
    , TimeConfig
    ) where

import Data.Aeson
import Data.Time
import Data.Time.Format

import Tickers
import Types.Agents
import Types.Config     (Delay)
import Types.Output

data TimeBlock = TimeBlock String TimeLocale

data TimeConfig = TimeConfig Delay Text

instance FromJSON TimeConfig where
    parseJSON = withObject "block" $ \o ->
        TimeConfig
        <$> o .:? "interval" .!= 1
        <*> o .:? "format"   .!= "%Y-%M-%D %h:%m"

newtype TimeEvent = Tick LocalTime

instance BlockAgent TimeConfig TimeEvent TimeBlock where

    create (TimeConfig delay fmt) events = do
        tz <- liftIO getCurrentTimeZone
        let fmt' = unpack fmt
        ticks <- ticker delay (getLocalTime tz)
        fork $ runEffect $ fromInput ticks >-> toOutput events
        return $ TimeBlock fmt' defaultTimeLocale
        where
            getLocalTime tz = do
                utc <- liftIO getCurrentTime
                let local = utcToLocalTime tz utc
                return $ Tick local

    step a @ (TimeBlock fmt locale) (Tick localTime) = do
        let txt = formatTime locale fmt localTime
            blk = emptyBlock { fullText = pack txt }
        return (a, Just blk)
