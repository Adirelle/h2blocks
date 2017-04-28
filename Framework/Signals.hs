{-# LANGUAGE TypeFamilies #-}

module Framework.Block where

import Control.Monad        (void)
import Data.Map.Strict      (assocs, fromListWith)
import Foreign.C.Types      (CInt (..))
import System.Posix.Signals (Handler (Catch), Signal (..), installHandler)


data SignalHandler = SignalHandler Signal (IO ())

installHandlers :: [SignalHandler] -> IO ()
installHandlers handlers =
    forM_ merged $ \(s, hs) ->
        void $ installHandler s (Catch $ chained hs) Nothing
    where
        merged :: [(Signal, [IO ()])]
        merged = assocs $ fromListWith (++) $ map unwrap handlers

        unwrap :: SignalHandler -> (Signal, [IO ()])
        unwrap (SignalHandler s h) = (s, [h])

        chained :: [IO ()] -> IO ()
        chained [h]  = h
        chained hs@_ = forM_ hs id

