module Framework.Timers where

import Framework.Stoppable

newtype Timer = Timer ThreadId

instance Stoppable Timer where
    stop = stopTimer

startRepeatedTimer :: Int -> IO () -> IO Timer
startRepeatedTimer delay tick  = do
    tid <- fork $ forever $ do
        threadDelay delay
        fork tick
    return $ Timer tid

stopTimer :: Timer -> IO ()
stopTimer (Timer tid) = killThread tid
