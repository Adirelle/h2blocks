{-# LANGUAGE TypeFamilies #-}

module H2Blocks.Data.Block
    ( BlockSpec(handleClicks, handleSignals, timer)
    , BlockProcess(..)
    , fromPipe
    , startBlock
    , signalHandlers
    , clickHandlers
    , module Pipes
    , module H2Blocks.Data.Duration
    , module H2Blocks.Data.Event
    , module H2Blocks.Data.I3BarIPC.Output
    )
     where

import qualified Data.Foldable                 as F
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Traversable              as T
import qualified Data.Vector                   as V
import           Pipes
import           Pipes.Concurrent              hiding (atomically)
import qualified Pipes.Prelude                 as P
import           System.Clock
import           System.Posix.Signals

import           H2Blocks.Data.Duration
import           H2Blocks.Data.Event
import           H2Blocks.Data.I3BarIPC.Output
import           H2Blocks.Data.TimedPipes

data BlockSpec = BlockSpec
    { pipe          :: Pipe Event Result IO ()
    , handleClicks  :: Bool
    , handleSignals :: Set Signal
    , timer         :: Maybe Duration
    }

data BlockProcess = BlockProcess
    { spec   :: BlockSpec
    , input  :: Output Event
    , output :: Input Result
    , stop   :: IO ()
    }

fromPipe :: Pipe Event Result IO () -> BlockSpec
fromPipe p = BlockSpec p False mempty Nothing

startBlock :: BlockSpec -> IO BlockProcess
startBlock spec @ (BlockSpec pipe _ _ _) = do
    (evOutput, evInput, evClose) <- spawn' unbounded
    (resOutput, resInput, resClose) <- spawn' unbounded
    let effect = fromInput evInput >-> pipeControl >-> pipe >-> dropSkip >-> toOutput resOutput
    tid <- fork $ do
        runEffect effect
        atomically (evClose >> resClose)
    let close = atomically (evClose >> resClose) >> killThread tid
        bp = BlockProcess spec evOutput resInput close
    startTimer bp

pipeControl :: Monad m => Pipe Event Event m ()
pipeControl = loop False Resume
    where
        loop _     Quit   = return ()
        loop _     Resume = yield Display >> await >>= loop True
        loop False _      = await >>= loop False
        loop True  Pause  = await >>= loop False
        loop True  x      = yield x >> await >>= loop True

dropSkip :: Monad m => Pipe Result Result m ()
dropSkip = P.filter (not . isSkip)
    where
        isSkip Skip = True
        isSkip _    = False

startTimer :: BlockProcess -> IO BlockProcess
startTimer b @ (BlockProcess (BlockSpec _ _ _ t) o _ s) = case t of
        Nothing -> return b
        Just d -> do
            tid <- fork $ runEffect $ ticker d >-> P.map Tick >-> toOutput o
            return $ b { stop = s >> killThread tid }

signalHandlers :: Foldable f => f BlockProcess -> Map Signal (Output Event)
signalHandlers = F.foldl' forEach M.empty
    where
        --forEach :: Map Signal (Output Event) -> BlockProcess -> Map Signal (Output Event)
        forEach hs bp @ (BlockProcess (BlockSpec _ _ sigs _) o _ _)
            | null sigs = hs
            | otherwise = foldl' (forEach2 o) hs sigs
        forEach2 o hs sig = insertWith mappend sig o hs

clickHandlers :: Traversable t => t BlockProcess -> (Map Text (Output Event), t BlockProcess)
clickHandlers = T.mapAccumL forEach M.empty
    where
        forEach :: Map Text (Output Event) -> BlockProcess -> (Map Text (Output Event), BlockProcess)
        forEach hs bp @ (BlockProcess (BlockSpec _ False _ _) _ _ _) = (hs, bp)
        forEach hs bp @ (BlockProcess (BlockSpec _ True _ _) i o _) =
            let inst = pack $ show $ M.size hs
                hs' = M.insertWith mappend inst i hs
                mark (Set b) = Set $ b { instance_ = Just inst }
                mark x       = x
                bp' = bp { output = fmap mark o }
            in (hs', bp')
