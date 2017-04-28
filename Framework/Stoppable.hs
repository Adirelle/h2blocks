module Framework.Stoppable where

class Stoppable s where
    stop :: s -> IO ()

instance Stoppable ThreadId where
    stop = killThread

instance Stoppable b => Stoppable [b] where
    stop = mapM_ stop
