module Blocks.Memory
    ( memoryBlock
    , MemoryUnit
    , MemoryConfig
    )
    where

import Conduit
import Text.Mustache
import Text.Mustache.Types as M
import Data.Aeson

import Tickers
import Types.Output
import Types.Config (Delay)
import System.SysInfo

data MemoryUnit = Terabyte | Gigabyte | Megabyte | Kilobyte | Byte
    deriving (Eq, Show)

instance FromJSON MemoryUnit where
    parseJSON = withText "MemoryUnit" $ return . parseUnit
        where
            parseUnit "TB" = Terabyte
            parseUnit "GB" = Gigabyte
            parseUnit "MB" = Megabyte
            parseUnit "kB" = Kilobyte
            parseUnit "B" = Byte

muDivisor :: MemoryUnit -> Double
muDivisor Terabyte = 2.0 ** 40
muDivisor Gigabyte = 2.0 ** 30
muDivisor Megabyte = 2.0 ** 20
muDivisor Kilobyte = 2.0 ** 10
muDivisor Byte     = 2.0 **  0

instance ToMustache MemoryUnit where
    toMustache Terabyte = M.String "TB"
    toMustache Gigabyte = M.String "GB"
    toMustache Megabyte = M.String "MB"
    toMustache Kilobyte = M.String "kB"
    toMustache Byte     = M.String "B"

data MemoryInfo = MemoryInfo MemoryUnit SysInfo

instance ToMustache MemoryInfo where
    toMustache (MemoryInfo omu (SysInfo _ _ tr fr sr br ts fs _ _ _ imu)) = M.object
      [ "totalRam"    ~> (fromIntegral tr * cf)
      , "freeRam"     ~> (fromIntegral fr * cf)
      , "sharedRam"   ~> (fromIntegral sr * cf)
      , "bufferRam"   ~> (fromIntegral br * cf)
      , "freeRamPct"  ~> ((100.0 :: Double) * fromIntegral fr / fromIntegral tr)
      , "totalSwap"   ~> (fromIntegral ts * cf)
      , "freeSwap"    ~> (fromIntegral fs * cf)
      , "freeSwapPct" ~> ((100.0 :: Double) * fromIntegral fs / fromIntegral ts)
      , "unit"        ~> omu
      ]
      where
          cf :: Double
          cf = fromIntegral imu / muDivisor omu

data MemoryConfig = MkMemoryConfig Delay Text MemoryUnit
    deriving (Show)

instance FromJSON MemoryConfig where
    parseJSON = withObject "MemoryConfig" $ \o -> MkMemoryConfig
        <$> o .:? "interval" .!= 1
        <*> o .:? "template" .!= "RAM: {{ freeRam }} / {{ totalRam }} SWAP: {{ freeSwap }} / {{ totalSwap }}"
        <*> o .:? "unit"     .!= Megabyte

memoryBlock :: (MonadBaseControl IO m, MonadIO m) => MemoryConfig -> Source m Block
memoryBlock (MkMemoryConfig delay tpl unit) = tickerSource delay (liftIO sysInfo) $= mapC applyUnit =$= mapC format =$= mapC makeBlock
    where
        applyUnit = MemoryInfo unit
        format = substitute tpl'
        tpl' = either (error . show) id $ compileTemplate "memory template" tpl
        makeBlock t = emptyBlock { fullText = t }
