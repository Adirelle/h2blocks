module Tickers where

import Conduit

import Types.Config (Delay, toMicroseconds)

tickerSource :: (MonadBaseControl IO m, MonadIO m) => Delay -> m a -> Source m a
tickerSource delay tick = repeatMC (wait >> tick)
    where
        wait = threadDelay $ toMicroseconds delay
