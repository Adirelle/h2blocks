{-# LANGUAGE FlexibleInstances #-}

module Types.Output
    ( Header (..)
    , emptyHeader

    , Block (..)
    , emptyBlock

    , Align ()
    , Color (..)
    , MinWidth (..)
    , Markup (..)

    , StatusLine (..)
    , emptyStatusLine
    )
    where

import           Data.Aeson
import qualified Data.Aeson.Encoding   as E
import           Data.Aeson.Types
import qualified Data.Text.Lazy        as TL
import           Formatting            (format, (%), (%.))
import           Formatting.Formatters (hex, left)

--------------------------------------------------------------------------------
-- Color
--------------------------------------------------------------------------------

data Color = Color Text | RGB Word8 Word8 Word8
    deriving (Show, Eq)

colorToString :: Color -> Text
colorToString (Color name) = name
colorToString (RGB r g b) = toStrict $ format ("#" % hexWord8 % hexWord8 % hexWord8) r g b
    where
        hexWord8 = left 2 '0' %. hex

instance ToJSON Color where
    toJSON = String . colorToString
    toEncoding = E.text . colorToString

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

data MinWidth = ForText Text | Pixels Int
    deriving (Show, Eq)

instance ToJSON MinWidth where
    toJSON (ForText t) = String t
    toJSON (Pixels n)  = Number  (fromIntegral n)

    toEncoding (ForText t) = E.text t
    toEncoding (Pixels n)  = E.int32  (fromIntegral n)

--------------------------------------------------------------------------------
-- Markup
--------------------------------------------------------------------------------

data Markup = Pango | None
    deriving (Show, Eq, Enum)

markupLabel :: Markup -> Text
markupLabel Pango = "pango"
markupLabel None  = "none"

instance ToJSON Markup where
    toJSON = String . markupLabel
    toEncoding = E.text . markupLabel

--------------------------------------------------------------------------------
-- Align
--------------------------------------------------------------------------------

data Align = AlignLeft | AligCenter | AlignRight
    deriving (Show, Eq, Enum)

alignLabel :: Align -> Text
alignLabel AlignLeft  = "left"
alignLabel AligCenter = "center"
alignLabel AlignRight = "right"

instance ToJSON Align where
    toJSON  = String . alignLabel
    toEncoding = E.text . alignLabel

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Allows to build a list of [Pair]
instance (KeyValue kv) => KeyValue [kv] where
    k .= v = [k .= v]

-- Allows to add a field only if its value is Just something
(.=?) :: (Monoid kv, KeyValue kv, ToJSON a) => Text -> Maybe a -> kv
k .=? (Just v) = k .= v
_ .=? Nothing  = mempty
infixr 8 .=?

-- To write the conversion only once
class ToKeyValue a where
    toKeyValue :: (Semigroup kv, Monoid kv, KeyValue kv) => a -> kv

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

data Header = Header
    { contSignal  :: Maybe Int
    , stopSignal  :: Maybe Int
    , clickEvents :: Maybe Bool
    }
    deriving (Show, Eq)

emptyHeader = Header
    { contSignal = Nothing
    , stopSignal = Nothing
    , clickEvents = Nothing
    }

instance ToKeyValue Header where
    toKeyValue (Header cs ss ce)
        =  "version"      .=  (1 :: Int)
        <> "cont_signal"  .=? cs
        <> "stop_signal"  .=? ss
        <> "click_events" .=? ce

instance ToJSON Header where
    toJSON = object . toKeyValue
    toEncoding = E.pairs . toKeyValue

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
    deriving (Show, Eq)

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

instance ToKeyValue Block where
    toKeyValue (Block ft n i st c bg bd mw u s sbw m)
        =  "full_text"             .=  ft
        <> "name"                  .=? n
        <> "instance"              .=? i
        <> "short_text"            .=? st
        <> "color"                 .=? c
        <> "background"            .=? bg
        <> "border"                .=? bd
        <> "min_width"             .=? mw
        <> "urgent"                .=? u
        <> "separator"             .=? s
        <> "separator_block_width" .=? sbw
        <> "markup"                .=? m

instance ToJSON Block where
    toJSON = object . toKeyValue
    toEncoding = E.pairs . toKeyValue

--------------------------------------------------------------------------------
-- StatusLine
--------------------------------------------------------------------------------

type StatusLine = Vector Block

emptyStatusLine :: Int -> StatusLine
emptyStatusLine n = replicate n emptyBlock
