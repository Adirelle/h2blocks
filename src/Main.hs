module Main where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector           as V
import qualified Data.Yaml             as Y
import qualified System.IO             as IO

import           H2Blocks.Data.Blocks
import           H2Blocks.Data.Builder (BlockProducer, build)
import           H2Blocks.Data.Config

main :: IO ()
main = do
    cfg <- readConfig "h2blocks.yaml"

    (prods, _, _, _) <- build buildBlock (blocks cfg)

    let delay = toMicroseconds (interval (global cfg))
    forever $ do
        emitStatusline prods
        threadDelay delay

readConfig :: FilePath -> IO (Config BlockConfig)
readConfig fp = do
    yaml <- IO.withFile fp IO.ReadMode BC.hGetContents
    return $ either error id $ Y.decodeEither yaml

emitStatusline :: Vector BlockProducer -> IO ()
emitStatusline prods = do
    blocks <- sequence prods
    print blocks
