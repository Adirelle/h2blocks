module Main where

-- import           GHC.Generics
import qualified System.IO                as IO

import           Conduit
import qualified Data.ByteString.Char8    as BC
import qualified Data.Vector              as V
import qualified Data.Yaml             as Y
import Data.Void 

import           Types
import           Types.Config
import           Tickers
import           Blocks
import Conduits

main :: IO ()
main = do
    cfg <- readConfig "h2blocks.yaml"

    sources <- buildBlocks (blocks cfg)
    
    status <- newMVar $ emptyStatusLine (length sources)
    
    forM_ sources $ \s -> forkC_ $ s =$ vectorUpdateSink status
    
    let i = interval $ global cfg  
    forkC_ $ statusLineSource status i $= statusLineEncoder emptyHeader =$ stdoutC

    runConduit $ stdinC $= clickDecoder =$ printC

readConfig :: FilePath -> IO (Config BlockConfig)
readConfig fp = do
    yaml <- IO.withFile fp IO.ReadMode BC.hGetContents
    return $ either error id $ Y.decodeEither yaml

buildBlocks :: (MonadBaseControl IO m, MonadIO m) => Vector BlockConfig -> m (Vector (Source m (Int, Block)))
buildBlocks = V.imapM buildBlockUpdater
    where
        buildBlockUpdater :: (MonadBaseControl IO m, MonadIO m) => Int -> BlockConfig -> m (Source m (Int, Block))
        buildBlockUpdater idx conf = do
            src <- buildBlock conf
            return $ src =$= mapC ((,) idx)

forkC :: (MonadBaseControl IO m, MonadIO m) => ConduitM () Data.Void.Void m () -> m ThreadId
forkC = fork . runConduit 

forkC_ :: (MonadBaseControl IO m, MonadIO m) => ConduitM () Data.Void.Void m () -> m ()
forkC_ = void . forkC