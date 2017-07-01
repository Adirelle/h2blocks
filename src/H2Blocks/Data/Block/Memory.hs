module H2Blocks.Data.Block.Memory
    ( MemoryConfig
    , buildMemoryBlock
    )
    where

import Data.Aeson
import Text.Mustache
import Text.Mustache.Types      as M
import Text.Parsec.Error
import Text.Printf

import H2Blocks.Data.Block      as B
import H2Blocks.Data.MemoryUnit
import H2Blocks.System.SysInfo

data MemoryConfig = MkMemoryConfig Text MemoryUnit Int
    deriving (Show)

instance FromJSON MemoryConfig where
    parseJSON = withObject "MemoryConfig" $ \o -> MkMemoryConfig
        <$> o .:? "template"  .!= "RAM: {{ freeRam }} / {{ totalRam }}{{ unit }} SWAP: {{ freeSwap }} / {{ totalSwap }}{{ unit }}"
        <*> o .:? "unit"      .!= Megabyte
        <*> o .:? "precision" .!= 1

data MemoryInfo = MemoryInfo MemoryConfig SysInfo

instance ToMustache MemoryInfo where
    toMustache (MemoryInfo (MkMemoryConfig _ omu prec) (SysInfo _ _ tr fr sr br ts fs _ _ _ imu)) = M.object
        [ "totalRam"    ~> memQty tr
        , "freeRam"     ~> memQty fr
        , "sharedRam"   ~> memQty sr
        , "bufferRam"   ~> memQty br
        , "totalSwap"   ~> memQty ts
        , "freeSwap"    ~> memQty fs
        , "freeRamPct"  ~> formatNumber ((100.0 :: Double) * fromIntegral fr / fromIntegral tr)
        , "freeSwapPct" ~> formatNumber ((100.0 :: Double) * fromIntegral fs / fromIntegral ts)
        , "unit"        ~> omu
        ]
        where
            memQty :: Integral a => a -> Text
            memQty x = formatNumber (fromIntegral x * unitRatio)

            unitRatio :: Double
            unitRatio = fromIntegral imu / muDivisor omu

            formatNumber :: PrintfArg a => a -> Text
            formatNumber x = pack (printf numberFmt x)

            numberFmt :: String
            numberFmt = "%." ++ show prec ++ "g"

buildMemoryBlock :: MonadIO m -> MemoryConfig -> m BlockSpec
buildMemoryBlock cfg @ (MkMemoryConfig tpl _ _) =
    return $ fromPipe pipe
    where
        prepare = MemoryInfo cfg
        tpl' = either showErrors id (compileTemplate "" tpl)
        format = substitute tpl'
        pipe = for cat $ \_ -> do
            si <- liftIO sysInfo
            let mi = prepare si
                res = format mi
            yield $ Set $ emptyBlock { fullText = res }
        go _ = nop
        showErrors err = error (concatMap messageString (errorMessages err))
