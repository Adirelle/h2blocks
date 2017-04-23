{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Aeson
import Data.ByteString
--
-- import I3BarIPC.Output
--
-- main :: IO ()
-- main = do
--     o <- start stdout emptyHeader
--     emit o [ bl ]
--
-- bl = emptyBlock
--     { fullText = "bal"
--     , color = Just $ RGB 255 5 8
--     , minWidth = Just $ ForText "BlaBla"
--     , urgent = Just True
--     , markup = Just Pango
--     }

import I3BarIPC.Input

main :: IO ()
main =  run stdin print
