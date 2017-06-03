{-# LANGUAGE RankNTypes #-}

module H2Blocks.Data.Pipes where

import           Pipes
import qualified Pipes.Lift                    as Lift
import           Pipes.Prelude                 as P
import           System.Clock
import           System.Posix.Signals

import           H2Blocks.Data.I3BarIPC.Input  as I
import           H2Blocks.Data.I3BarIPC.Output as O

data InputEvent
    = Click !I.ClickEvent
    | Signal !Signal
    | Tick !TimeSpec
    | Pause
    | Resume
    | Query
    deriving (Show, Eq)

data OutputEvent
    = Update Int !O.Block
    | Refresh
    deriving (Show, Eq)

type Processor m = Pipe InputEvent OutputEvent m

forEvents :: Monad m => (InputEvent -> Producer' OutputEvent m ()) -> Processor m ()
forEvents body = forever $ await >>= body

update :: Monad m => O.Block -> Producer' OutputEvent m ()
update b = yield (Update 0 b)

refresh :: Monad m => Producer' OutputEvent m ()
refresh = yield Refresh

nop :: Monad m => Producer' OutputEvent m ()
nop = return ()

pauseFilter :: Monad m => Pipe InputEvent InputEvent m ()
pauseFilter = loop True
    where
        loop s = do
            e <- await
            let (s', y) = filter s e
            when y $ yield e
            loop $! s'
        filter _ Pause  = (False, False)
        filter _ Resume = (True, False)
        filter s _      = (s, s)

clickInputFilter :: Monad m => Text -> Pipe InputEvent InputEvent m ()
clickInputFilter inst_ = P.filter checkInstance
    where
        checkInstance (Click ce) = I.instance_ ce == Just inst_
        checkInstance _          = True

clickOutputFilter :: Monad m => Text -> Pipe OutputEvent OutputEvent m ()
clickOutputFilter inst_ = P.map tagBlock
    where
        tagBlock (Update i b) = Update i b { O.instance_ = Just inst_ }
        tagBlock x            = x

signalFilter :: Monad m => SignalSet -> Pipe InputEvent InputEvent m ()
signalFilter sigs = P.filter checkSignal
    where
        checkSignal (Signal s) = inSignalSet s sigs
        checkSignal _          = True
