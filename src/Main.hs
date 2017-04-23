module Main where

import Data.Aeson
import Data.ByteString

import I3BarIPC.Types

main :: IO ()
main = do
    let x = encode bl
    Prelude.hPut stdout (toStrict x)

bl = emptyBlock
    { fullText = "bal"
    , color = Just $ RGB 255 5 8
    , minWidth = Just $ ForText "BlaBla"
    , urgent = Just True
    , markup = Just Pango
    }
