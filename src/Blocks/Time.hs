module Blocks.Time (timeBlock) where

import System.IO.Unsafe (unsafePerformIO)

import Conduit
import Data.Time
import Data.Time.Format

import Tickers
import Types.Output
import Types.Config (Delay)

timeBlock :: (MonadBaseControl IO m, MonadIO m) => Delay -> Text -> Source m Block
timeBlock delay fmt = tickerSource delay getCurrentTime' $= mapC formatTime' =$= mapC mkBlock
    where 
        getCurrentTime' = liftIO getCurrentTime
        formatTime' = pack . formatTime defaultTimeLocale fmt' . utcToLocalTime tz
        mkBlock t = emptyBlock { fullText = t }
        fmt' = unpack fmt
        tz = unsafePerformIO getCurrentTimeZone
