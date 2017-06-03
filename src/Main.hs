module Main where

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy.Char8    as BLC
import qualified Data.Yaml                     as Y

import           H2Blocks.Data.Blocks
import           H2Blocks.Data.Builder
import           H2Blocks.Data.Config
import           H2Blocks.Data.I3BarIPC.Output

main :: IO ()
main = do
    cfg <- readConfig "h2blocks.yaml"

    (procs, BuilderOutput sigs c) <- buildBlocks $ blocks cfg

    let hdr = emptyHeader
    outputWorker stdout hdr prods (interval (global cfg))

readConfig :: FilePath -> IO (Config BlockConfig)
readConfig fp = do
    res <- Y.decodeFileEither fp
    case res of
        Left e  -> throw e
        Right c -> return c

buildBlocks :: Vector BlockConfig -> IO (Vector (Processor IO ()), BuilderOutput)
buildBlocks c = undefined

-- outputWorker :: Handle -> Header -> Vector BlockProducer -> Delay -> IO ()
-- outputWorker h header prods delay = do
--     put (A.encode header)
--     put "["
--     forever $ do
--         blocks <- sequence prods
--         put (A.encode blocks)
--         threadDelay msDelay
--     where
--         msDelay = toMicroseconds delay
--         put = BLC.hPutStrLn h
