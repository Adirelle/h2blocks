{-# LANGUAGE DeriveGeneric #-}

module I3BarIPC.Input
    ( ClickEvent(..)
    , run
    )
    where

import           Data.Aeson
import qualified Data.ByteString.Char8     as BC
import           GHC.Generics
import           GHC.IO.Handle

import           I3BarIPC.Internal.Options

--------------------------------------------------------------------------------
-- ClickEvent
--------------------------------------------------------------------------------

data ClickEvent = ClickEvent
    { name      :: Maybe Text
    , instance_ :: Maybe Text
    , x         :: Int
    , y         :: Int
    , button    :: Int
    }
    deriving (Show, Eq, Generic)

instance FromJSON ClickEvent where
    parseJSON = genericParseJSON jsonOptions

--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------

run :: Handle -> (ClickEvent -> IO ()) -> IO ()
run h handler = do
    line <- BC.hGetLine h
    when (line /= "[") $
        error "Expecting ["
    forever $ do
        line <- BC.hGetLine h
        let decoded = eitherDecodeStrict line
        case decoded of
            Right c -> handler c
            Left e  -> hPutStr stderr $ "Error decoding input: " ++ e ++ "\n"
