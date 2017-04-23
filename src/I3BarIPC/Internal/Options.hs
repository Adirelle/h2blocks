module I3BarIPC.Internal.Options where

import Data.Aeson.Types (Options (..), camelTo2, defaultOptions)

jsonOptions = defaultOptions
    { fieldLabelModifier = labelModifier
    , constructorTagModifier = camelTo2 '_'
    , allNullaryToStringTag = True
    , omitNothingFields = True
    , unwrapUnaryRecords = True
    }

labelModifier :: String -> String
labelModifier x
    | x == "instance_" = "instance"
    | otherwise        = camelTo2 '_' x
