module H2Blocks.Data.TimedPipes where

import Data.Foldable
import Pipes                  as P
import Pipes.Concurrent       hiding (atomically)
import Pipes.Core             as P
import System.Clock
import Data.Maybe

import H2Blocks.Data.Duration

now :: MonadIO m => m TimeSpec
now = liftIO $ getTime Monotonic

timestamp :: MonadIO m => Pipe a (TimeSpec,a) m ()
timestamp = P.for cat $ \x -> do
    t <- now
    yield (t, x)

periodically :: (MonadBaseControl IO m, MonadIO m) => Duration -> m a -> Producer a m ()
periodically d f = do
    t <- fromTimeSpec <$> now
    loop (t + d)
    where
        loop t = do
            t' <- fromTimeSpec <$> now
            when (t - t' > 0) $ do
                sleep (t - t')
                x <- lift f
                yield x
            loop (t + d)

ticker :: (MonadBaseControl IO m, MonadIO m) => Duration -> Producer TimeSpec m ()
ticker d = periodically d now

cache :: (MonadBaseControl IO m, MonadIO m) => TimeSpec -> Pipe a a m ()
cache d = timestamp >-> refresh
    where
        refresh = do
            (t, x) <- await
            serve x (t + d)
        serve x e = do
            n <- now
            if n < e
                then yield x >> serve x e
                else refresh
