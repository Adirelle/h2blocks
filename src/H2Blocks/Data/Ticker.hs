module H2Blocks.Data.Ticker
    ( ticker
    )
    where

import Pipes.Concurrent hiding (atomically)

import H2Blocks.Data.Config     (Delay, toMicroseconds)

ticker :: (MonadBaseControl IO m, MonadIO m) => Delay -> m (Input UTCTime)
ticker delay = do
    (outp, inp) <- liftIO $ spawn buffer
    fork $ forever $ do
        value <- liftIO getCurrentTime
        atomically $ void $ send outp value
        threadDelay msDelay
    return inp
    where
        msDelay = toMicroseconds delay
        buffer = newest 1
