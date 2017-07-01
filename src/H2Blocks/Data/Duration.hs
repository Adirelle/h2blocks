{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module H2Blocks.Data.Duration
    ( Duration
    , sleep
    , microseconds
    , milliseconds
    , seconds
    , minutes
    , hours
    , toTimeSpec
    , fromTimeSpec
    ) where

import Data.Aeson
import Data.Scientific
import System.Clock
import Text.Parsec                              as P
import Text.ParserCombinators.Parsec.Combinator as P
import Text.ParserCombinators.Parsec.Numeric    as P

newtype Duration = Duration Int
    deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

instance FromJSON Duration where
    parseJSON = withText "Duration" $ \t -> do
        r <- runParserT durationParser () "Duration" t
        case r of
            Right d -> return d
            Left e  -> fail $ show e

durationParser :: Monad m => ParsecT Text () m Duration
durationParser = do
    n <- floating <?> "value"
    P.optional spaces
    c <- option seconds ( choice
        [ string "µs" *> return microseconds
        , string "ms" *> return milliseconds
        , string "s" *> return seconds
        , string "m" *> return minutes
        , string "h" *> return hours
        ] <?> "unit" )
    eof
    return $ c n

duration :: (Real a, Real b) => a -> b -> Duration
duration u n = Duration $ truncate (toRational n * toRational u * 1.0e-6)

sleep :: MonadIO m => Duration -> m ()
sleep (Duration ms) = liftIO $ threadDelay ms

microseconds :: Real a => a -> Duration
microseconds = duration 1.0e-6

milliseconds :: Real a => a -> Duration
milliseconds = duration 1.0e-3

seconds :: Real a => a -> Duration
seconds = duration 1.0

minutes :: Real a => a -> Duration
minutes = duration 60.0

hours :: Real a => a -> Duration
hours = duration 3600.0

toTimeSpec :: Duration -> TimeSpec
toTimeSpec (Duration ms) = TimeSpec 0 (fromIntegral ms * 1000)

fromTimeSpec :: TimeSpec -> Duration
fromTimeSpec t = microseconds $ toNanoSecs t `div` 1000
