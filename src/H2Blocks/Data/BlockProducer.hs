module H2Blocks.Data.BlockProducer
    ( ClickHandler
    , SignalHandler
    , BlockProducer
    , Builder
    , onClick
    , onSignal
    , askConfig
    , askIndex
    , buildProducers
    )
    where

import qualified Data.Set                      as S
import           Prelude                       hiding (Builder, Handler)
import           System.Posix.Signals          hiding (Handler)
--import Data.Aeson.Types
import           Control.Monad.Reader
import           Control.Monad.Writer          hiding ((<>))
import           Data.Monoid                   (Monoid (..))
import qualified Data.Vector                   as V

import           H2Blocks.Data.I3BarIPC.Input  as I
import           H2Blocks.Data.I3BarIPC.Output as O

type ClickHandler = ClickEvent -> IO ()
type SignalHandler = Signal -> IO ()
type BlockProducer = IO Block

data Handler
    = OnClick { ch:: ClickHandler }
    | OnSignal { sig :: Signal, sh :: SignalHandler }

isClickHandler (OnClick _) = True
isClickHandler _           = False

isSignalHandler (OnSignal _ _) = True
isSignalHandler _              = False

filterHandlers :: [Handler] -> ([ClickHandler], Set Signal, [SignalHandler])
filterHandlers handlers =
    let
        clickHandlers = map ch $ filter isClickHandler handlers
        s = filter isSignalHandler handlers
        signals = S.fromList $ map sig s
        signalHandlers = map sh s
    in
        (clickHandlers, signals, signalHandlers)

data Context c = Context c Text

type Builder c m a = ReaderT (Context c) (WriterT [Handler] m) a

askConfig :: Monad m => Builder c m c
askConfig = do
    (Context c _) <- ask
    return c

askIndex :: Monad m => Builder c m Text
askIndex = do
    (Context _ i) <- ask
    return i

onClick :: Monad m => ClickHandler -> Builder c m ()
onClick ch = do
    idx <- askIndex
    tell [OnClick (filterOnIndex idx)]
    where
        filterOnIndex idx ce @ ClickEvent { I.instance_ = Just idx' } | idx' == idx = ch ce
        filterOnIndex _ _                                             = return ()

onSignal :: Monad m => Signal -> SignalHandler -> Builder c m ()
onSignal sig sh = tell [OnSignal sig filterOnSignal]
    where
        filterOnSignal sig' | sig' == sig = sh sig
                            | otherwise   = return ()

buildProducers :: Monad m => Builder c m BlockProducer -> Vector c -> m (Vector BlockProducer, [ClickHandler], Set Signal, [SignalHandler])
buildProducers builder confs = do
    (producers, handlers) <- runWriterT (V.imapM runBuilder confs)
    let (clickHandlers, signals, signalHandlers) = filterHandlers handlers
    return (producers, clickHandlers, signals, signalHandlers)
    where
        runBuilder idx config = do
            let idx' = tshow idx
            producer <- runReaderT builder (Context config idx')
            return $ setInstance producer idx'

        setInstance producer idx = do
            block <- producer
            return block { O.instance_ = Just idx }
