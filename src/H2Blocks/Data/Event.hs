module H2Blocks.Data.Event
    ( Event(..)
    , Result(..)
    , TimeSpec
    , Signal
    , ClickEvent
    , Block
    ) where

import System.Clock
import System.Posix.Signals

import H2Blocks.Data.I3BarIPC.Input
import H2Blocks.Data.I3BarIPC.Output

data Event
    = Display
    | Click ClickEvent
    | Signal Signal
    | Tick TimeSpec
    | Pause
    | Resume
    | Quit
    deriving (Show, Eq)

data Result
    = Set Block
    | ForceDisplay
    | Skip
    deriving (Show, Eq)
