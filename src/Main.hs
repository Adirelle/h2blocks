{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Monad.Trans.State.Strict (StateT, get, modify)
import qualified Data.Aeson                       as A
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy.Char8       as BLC
import qualified Data.Foldable                    as F
import qualified Data.Map                         as M
import qualified Data.Traversable                 as T
import qualified Data.Vector                      as V
import qualified Data.Yaml                        as Y
import           Pipes                            as P
import           Pipes.Concurrent                 as P hiding (atomically)
import           Pipes.Lift                       as P
import qualified Pipes.Prelude                    as P

import           System.Posix.Signals             (Handler (Catch), Signal, installHandler)

import           H2Blocks.Data.Block              (clickHandlers, input, output, signalHandlers, startBlock, stop)
import           H2Blocks.Data.Block.Builder
import           H2Blocks.Data.Config
import           H2Blocks.Data.Event
import           H2Blocks.Data.I3BarIPC.Input     as I
import           H2Blocks.Data.I3BarIPC.Output    (StatusLine, clickEvents, emptyHeader, emptyStatusLine)
import           H2Blocks.Data.TimedPipes

main :: IO ()
main = do
    cfg <- readConfig "h2blocks.yaml"

    specs <- mapM buildBlock $ blocks cfg
    procs <- mapM startBlock specs

    let sh = signalHandlers procs
        (ch, procs') = clickHandlers procs

    handleSignals sh

    let procOutput = mconcat $ V.toList $ indexResults $ V.map output procs'
        procInput = mconcat $ V.toList $ V.map input procs'
        outputPipe = generateStatusLines (not $ null ch) procOutput
        inputPipe = handleInput ch
        startLine = emptyStatusLine (length procs')

    fork $ runEffect $ ticker (interval $ global cfg) >-> P.map (const Display) >-> toOutput procInput
    fork $ runEffect $ void $ execStateP startLine outputPipe
    runEffect inputPipe

    forM_ procs' stop

readConfig :: FilePath -> IO (Config BlockConfig)
readConfig fp = do
    res <- Y.decodeFileEither fp
    case res of
        Left e  -> throw e
        Right c -> return c

handleSignals :: Map Signal (Output Event) -> IO ()
handleSignals sh =
    forM_ (M.toList sh) $ \(s, o) ->
        let snd = atomically $ send o (Signal s)
            hdlr = Catch (void snd)
        in
        void $ installHandler s hdlr Nothing

indexResults :: Vector (Input Result) -> Vector (Input (Int, Result))
indexResults = V.imap $ \i -> fmap ((,) i)

generateStatusLines :: MonadIO m => Bool -> Input (Int, Result) -> Effect (StateT StatusLine m) ()
generateStatusLines clicks input =
        do
            header >-> encode
            fromInput input >-> updateStatusLine >-> encode
        >-> P.map BLC.unpack
        >-> P.stdoutLn
    where
        header = yield $ emptyHeader { clickEvents = Just clicks }

        encode :: Monad m' =>  forall a . A.ToJSON a => Pipe a LByteString m' ()
        encode = P.for cat $ \x -> yield $! A.encode x

        updateStatusLine = do
            res <- await
            lift $ modify $ handleResult res
            line <- lift get
            yield line

        handleResult :: (Int, Result) -> StatusLine -> StatusLine
        handleResult (_, ForceDisplay) l = l
        handleResult (i, Set b)        l = l V.// [(i, b)]

handleInput :: MonadIO m => Map Text (Output Event) -> Effect m ()
handleInput handlers =
    P.stdinLn
    >-> P.map BC.pack
    >-> parse
    >-> dispatch
    where
        parse = P.for cat $ \ln ->
            case A.eitherDecodeStrict ln of
                Left e  -> print e
                Right c -> yield c

        dispatch = P.for cat $ \e ->
            case I.instance_ e >>= flip M.lookup handlers of
                Nothing -> return ()
                Just o' -> void $ atomically $ send o' $ Click e
