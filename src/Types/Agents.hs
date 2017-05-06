{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Types.Agents
    ( BlockAgent(..)
    , BlockInstance(blocks, clicks, signals)
    , startAgent
    , module Pipes
    , module Pipes.Concurrent
    )
    where

import Data.Aeson
import Pipes
import Pipes.Concurrent
import System.Posix.Signals (Signal)

import Types.Input          (ClickEvent)
import Types.Output         (Block, emptyBlock)

data BlockInstance = BlockInstance
    { blocks  :: Input Block
    , clicks  :: Maybe (Output ClickEvent)
    , signals :: Maybe (Signal, Output Signal)
    }

class BlockAgent c e a | c -> a, c -> e, e -> a, e -> c where
    handleClicks :: c -> Bool
    handleClicks _ = False

    handleSignals :: c -> Maybe Signal
    handleSignals _ = Nothing

    clicked :: (MonadBaseControl IO m, MonadIO m) => ClickEvent -> m e
    clicked _ = error "Does not support clicks"

    signaled :: (MonadBaseControl IO m, MonadIO m) => Signal -> m e
    signaled _ = error "Does not support clicks"

    create :: (MonadBaseControl IO m, MonadIO m) => c -> Output e -> m a
    step :: (MonadBaseControl IO m, MonadIO m) => a -> e -> m (a, Maybe Block)

startAgent :: (MonadBaseControl IO m, MonadIO m, BlockAgent c e a)  => c -> m BlockInstance
startAgent cfg = do
    (blocks_out, blocks_in) <- liftIO $ spawn (latest emptyBlock)
    (events_out, events_in) <- liftIO $ spawn (bounded 5)
    agent <- create cfg events_out
    fork $ runAgent cfg agent events_in blocks_out
    return $ BlockInstance blocks_in Nothing Nothing

runAgent :: (MonadBaseControl IO m, MonadIO m, BlockAgent c e a) => c -> a -> Input e -> Output Block -> m ()
runAgent cfg agent events blocks =
    runEffect $ fromInput events >-> processEvents agent >-> toOutput blocks
    where
        processEvents agent = do
            event <- await
            (agent', mblock) <- liftIO $ step agent event
            case mblock of
                Just block -> yield block
                Nothing -> return ()
            processEvents agent'
