module Main where

-- import           GHC.Generics
import qualified System.IO                as IO

import           Pipes
import qualified Data.ByteString.Char8    as BC
import qualified Data.Vector              as V
import qualified Data.Yaml             as Y

import           Types
import           Types.Config
import           Tickers
import           Blocks

main :: IO ()
main = do
    cfg <- readConfig "h2blocks.yaml"

    sources <- buildBlocks (blocks cfg)

    return ()

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
