module Conduits where

import           Conduit
import           Data.Aeson               as A
import           Data.Aeson.Text          as A
import qualified Data.Conduit.Combinators as C
import qualified Data.Vector              as V

import           Types
import           Tickers

clickDecoder :: MonadIO m => Conduit ByteString m ClickEvent
clickDecoder = C.splitOnUnboundedE (== 10) =$= (expectListStart >> readClicks)
    where
        expectListStart = do
            line <- await
            print line
            case line of
                Just "[" -> return ()
                x        -> error $ "Expected '[', not" ++ show x
        readClicks = mapC decodeClick
        decodeClick = either reportError id . A.eitherDecodeStrict
        reportError = error . ("Invalid ClickEvent: " ++)

statusLineEncoder :: Monad m => Header -> Conduit StatusLine m ByteString
statusLineEncoder h = sendHeader >> startList >> sendLines
    where
        sendHeader = yield $ strictEncode h
        startList = yield "["
        sendLines = mapC strictEncode
        strictEncode :: ToJSON a => a -> ByteString
        strictEncode x = toStrict (A.encode x ++ "\n")

vectorUpdateSink :: (MonadBaseControl IO m, MonadIO m) => MVar (Vector a) -> Sink (Int, a) m ()
vectorUpdateSink var = C.mapM_ updateVar
     where
         updateVar upd = modifyMVar_ var (updateVec upd)
         updateVec upd vec = return $ vec V.// [upd]
         
statusLineSource :: (MonadBaseControl IO m, MonadIO m) => MVar StatusLine -> Delay -> Source m StatusLine
statusLineSource v d = tickerSource d $ readMVar v
