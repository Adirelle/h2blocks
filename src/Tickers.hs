module Tickers
    ( ticker
    )
    where

import Pipes.Concurrent hiding (atomically)

import Types.Config     (Delay, toMicroseconds)

ticker :: (MonadBaseControl IO m, MonadIO m) => Delay -> m a -> m (Input a)
ticker delay gen = do
    (outp, inp) <- liftIO $ spawn buffer
    fork $ forever $ do
        value <- gen
        atomically $ void $ send outp value
        threadDelay msDelay
    return inp
    where
        msDelay = toMicroseconds delay
        buffer = newest 1
