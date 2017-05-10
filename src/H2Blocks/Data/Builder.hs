module H2Blocks.Data.Builder
    ( ClickHandler
    , SignalHandler
    , BlockProducer
    , Builder
    , onClick
    , onSignal
    , askIndex
    , build
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

newtype Context = Context Text

type Builder m a = ReaderT Context (WriterT [Handler] m) a

askIndex :: Monad m => Builder m Text
askIndex = do
    (Context i) <- ask
    return i

onClick :: Monad m => ClickHandler -> Builder m ()
onClick ch = do
    idx <- askIndex
    tell [OnClick (filterOnIndex idx)]
    where
        filterOnIndex idx ce @ ClickEvent { I.instance_ = Just idx' } | idx' == idx = ch ce
        filterOnIndex _ _                                             = return ()

onSignal :: Monad m => Signal -> SignalHandler -> Builder m ()
onSignal sig sh = tell [OnSignal sig filterOnSignal]
    where
        filterOnSignal sig' | sig' == sig = sh sig
                            | otherwise   = return ()

build :: Monad m => (c -> Builder m BlockProducer) -> Vector c -> m (Vector BlockProducer, [ClickHandler], Set Signal, [SignalHandler])
build builder confs = do
    (producers, handlers) <- runWriterT (V.imapM runBuilder confs)
    let (clickHandlers, signals, signalHandlers) = filterHandlers handlers
    return (producers, clickHandlers, signals, signalHandlers)
    where
        runBuilder idx config = do
            let idx' = tshow idx
            producer <- runReaderT (builder config) (Context idx')
            return $ setInstance producer idx'

        setInstance producer idx = do
            block <- producer
            return block { O.instance_ = Just idx }
