{-# LANGUAGE DeriveGeneric #-}

module I3BarIPC.Output
    ( Output
    , start
    , emit
    , Header(..)
    , emptyHeader
    , Color(..)
    , MinWidth(..)
    , Markup(..)
    , Align(..)
    , Block(..)
    , emptyBlock
    , Statusline
    )
    where

import           Data.Aeson
import           Data.Aeson.Encoding       (text)
import           Data.Aeson.Types          (Options (..), camelTo2, defaultOptions)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Formatting                (format, now, (%), (%.))
import           Formatting.Formatters     (hex, left)
import           GHC.Generics

import           I3BarIPC.Internal.Options

--------------------------------------------------------------------------------
-- Color
--------------------------------------------------------------------------------

data Color = RGB Word8 Word8 Word8 | Color Text
    deriving (Show, Eq)

instance ToJSON Color where
    toJSON (Color name) = String name
    toJSON (RGB r g b) = String $ TL.toStrict hexColor
        where
            hexColor = format (now "#" % hexWord8 % hexWord8 % hexWord8) r g b
            hexWord8 = left 2 '0' %. hex

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

data MinWidth = ForText Text | Pixels Int
    deriving (Show, Eq)

instance ToJSON MinWidth where
    toJSON (ForText t) = String t
    toJSON (Pixels n)  = Number  (fromIntegral n)

--------------------------------------------------------------------------------
-- Markup
--------------------------------------------------------------------------------

data Markup = Pango | None
    deriving (Show, Eq, Enum)

instance ToJSON Markup where
    toJSON Pango = String "pango"
    toJSON None  = String "none"

--------------------------------------------------------------------------------
-- Align
--------------------------------------------------------------------------------

data Align = AlignLeft | AligCenter | AlignRight
    deriving (Show, Eq, Enum)

instance ToJSON Align where
    toJSON AlignLeft  = String "left"
    toJSON AligCenter = String "center"
    toJSON AlignRight = String "right"

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

data Header = Header
    { version     :: Int
    , contSignal  :: Maybe Int
    , stopSignal  :: Maybe Int
    , clickEvents :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

emptyHeader = Header
    { version = 1
    , contSignal = Nothing
    , stopSignal = Nothing
    , clickEvents = Nothing
    }

instance ToJSON Header where
    toEncoding = genericToEncoding jsonOptions

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

data Block = Block
    { fullText            :: Text
    , name                :: Maybe Text
    , instance_           :: Maybe Text
    , shortText           :: Maybe Text
    , color               :: Maybe Color
    , background          :: Maybe Color
    , border              :: Maybe Color
    , minWidth            :: Maybe MinWidth
    , urgent              :: Maybe Bool
    , separator           :: Maybe Bool
    , separatorBlockWidth :: Maybe Int
    , markup              :: Maybe Markup
    }
    deriving (Show, Eq, Generic)

emptyBlock = Block
    { fullText            = ""
    , name                = Nothing
    , instance_           = Nothing
    , shortText           = Nothing
    , color               = Nothing
    , background          = Nothing
    , border              = Nothing
    , minWidth            = Nothing
    , urgent              = Nothing
    , separator           = Nothing
    , separatorBlockWidth = Nothing
    , markup              = Nothing
    }

instance ToJSON Block where
    toEncoding = genericToEncoding jsonOptions

--------------------------------------------------------------------------------
-- Statusline
--------------------------------------------------------------------------------

type Statusline = Vector Block

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

newtype Output = Output Handle

start :: Handle -> Header -> IO Output
start h hdr = do
    let o = Output h
    writeJSON o hdr
    hPut h "[\n"
    return o

emit :: Output -> Statusline -> IO ()
emit  = writeJSON

writeJSON :: ToJSON d => Output -> d -> IO ()
writeJSON (Output o) x = do
    hPut o $ toStrict $ encode x
    hPut o "\n"
