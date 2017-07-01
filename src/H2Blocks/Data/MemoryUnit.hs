module H2Blocks.Data.MemoryUnit
    ( MemoryUnit(..)
    , muDivisor
    )
    where

import Data.Aeson

data MemoryUnit = Terabyte | Gigabyte | Megabyte | Kilobyte | Byte
    deriving (Eq, Show)

instance FromJSON MemoryUnit where
    parseJSON = withText "MemoryUnit" $ return . parseUnit
        where
            parseUnit "TB" = Terabyte
            parseUnit "GB" = Gigabyte
            parseUnit "MB" = Megabyte
            parseUnit "kB" = Kilobyte
            parseUnit "B"  = Byte

muDivisor :: MemoryUnit -> Double
muDivisor Terabyte = 2.0 ** 40
muDivisor Gigabyte = 2.0 ** 30
muDivisor Megabyte = 2.0 ** 20
muDivisor Kilobyte = 2.0 ** 10
muDivisor Byte     = 2.0 **  0
