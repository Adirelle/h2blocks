{-# LANGUAGE RankNTypes #-}

module H2Blocks.Data.Builder where

import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Set             as Set
import           Pipes
import qualified System.Posix         as Posix

import           H2Blocks.Data.Pipes

data BuilderOutput = BuilderOutput !(Set Posix.Signal) !Bool

instance Monoid BuilderOutput where
    mempty = BuilderOutput Set.empty False
    (BuilderOutput sa ca) `mappend` (BuilderOutput sb cb) = BuilderOutput (sa `mappend` sb)  (ca || cb)

type BuilderT m = WriterT BuilderOutput m

type Builder = BuilderT Identity (Processor Identity ())
type BuilderIO = BuilderT IO (Processor IO ())

handleSignal :: Monad m => Posix.Signal -> BuilderT m ()
handleSignal s = tell $ BuilderOutput (Set.singleton s) False

handleClick :: Monad m => BuilderT m ()
handleClick = tell $ BuilderOutput Set.empty True

build :: (Monad m, Monad m') => Text -> BuilderT m (Processor m' ()) -> m (Processor m' (), BuilderOutput)
build i b = do
    (p, bo) <- runWriterT b
    let (BuilderOutput sigs clickable) = bo
        p1 = if Set.null sigs
            then p
            else signalFilter (foldr Posix.addSignal Posix.emptySignalSet sigs) >-> p
        p2 = if clickable
            then clickInputFilter i >-> p >-> clickOutputFilter i
            else p1
        p3 = pauseFilter >-> p2
    return (p3, bo)
