module Types.Input
    ( ClickEvent(..)
    )
    where

import Data.Aeson
import Data.Aeson.Types

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
    deriving (Show, Eq)

instance FromJSON ClickEvent where
    parseJSON = withObject "ClickEvent" $ \o -> do
        n <- o .:? "name"
        i <- o .:? "instance"
        x <- o .:  "x"
        y <- o .:  "y"
        b <- o .:  "b"
        return $ ClickEvent n i x y b
