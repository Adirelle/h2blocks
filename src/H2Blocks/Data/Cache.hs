{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module H2Blocks.Data.Cache
    ( Cache(storeSTM, fetchSTM)
    , newCacheSTM
    , newCache
    , storeCache
    , fetchCache
    , newMemo0
    , newMemo1

    , TimeSpec
    )
    where

import System.Clock
import Data.HashMap.Strict as HS

data CacheItem a = CacheItem TimeSpec a

class CacheStorage s where
    type Key s
    type Value s
    emptyCS :: s
    insertCS :: s -> Key s -> Value s -> s
    removeCS :: s -> Key s -> s
    lookupCS :: s -> Key s -> Maybe (Value s)

instance CacheStorage (Maybe a) where
    type Key (Maybe a) = ()
    type Value (Maybe a) = a
    emptyCS = Nothing
    insertCS _ () = Just
    removeCS _ () = Nothing
    lookupCS x () = x

instance (Eq k, Hashable k) => CacheStorage (HS.HashMap k v) where
    type Key (HashMap k v) = k
    type Value (HashMap k v) = v
    emptyCS = mempty
    insertCS s k v = HS.insert k v s
    removeCS = flip HS.delete
    lookupCS = flip HS.lookup

now :: MonadBaseControl IO m => m TimeSpec
now = liftBase $ getTime Monotonic

data (Eq k, Hashable k) => Cache k v = MkCache
    { storeSTM :: TimeSpec -> k -> v -> STM ()
    , fetchSTM :: TimeSpec -> k -> TimeSpec -> STM (Maybe v)
    }

newCacheSTM :: (CacheStorage s, a ~ Key s, CacheItem b ~ Value s, Eq a, Hashable a) => s -> STM (Cache a b)
newCacheSTM storage = do
    var <- newTMVar storage
    let
        store now k v = do
            st <- readTMVar var
            let i = CacheItem now v
                st' = insertCS st k i
            putTMVar var st'

        fetch now k t = do
            st <- readTMVar var
            let i = lookupCS st k
            case i of
                Just (CacheItem t' v)
                    | t' >= now - t -> return $ Just v
                    | otherwise -> do
                        let st' = removeCS st k
                        putTMVar var st'
                        return Nothing
                Nothing -> return Nothing

    return $ MkCache store fetch

newCache s = atomically $ newCacheSTM s

newEmptyCacheSTM :: (Eq a, Hashable a) => STM (Cache a b)
newEmptyCacheSTM = newCacheSTM HS.empty

newEmptyCache :: (Eq a, Hashable a, MonadIO m) => m (Cache a b)
newEmptyCache = atomically newEmptyCacheSTM

storeCache c k v = do
    t <- now
    atomically $ storeSTM c t k v

fetchCache c k t = do
    t' <- now
    atomically $ fetchSTM c t' k t

newMemo' :: (Eq a, Hashable a, MonadIO m, MonadBaseControl IO m) => Cache a b -> (a -> m b) -> IO (a -> TimeSpec -> m b)
newMemo' c f = return $ \k t -> do
    v <- fetchCache c k t
    case v of
        Just v' -> return v'
        Nothing -> do
            v' <- f k
            storeCache c k v'
            return v'

newMemo0 :: (MonadIO m, MonadBaseControl IO m) => m a -> IO (TimeSpec -> m a)
newMemo0 f = do
    c <- newCache Nothing
    f' <- newMemo' c (const f)
    return $ f' ()

newMemo1 :: (Eq a, Hashable a, MonadIO m, MonadBaseControl IO m) => (a -> m b) -> IO (a -> TimeSpec -> m b)
newMemo1 f = do
    c <- newCache HS.empty
    newMemo' c f
