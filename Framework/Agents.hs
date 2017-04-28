module Framework.Agents
    ( Queue
    , newQueue
    , sendToQueue
    , Agent
    , newAgent
    , newAgent'
    , start
    , stop
    , sendToAgent
    )
    where

import qualified Framework.Stoppable as S

newtype Queue msg = Queue (TMChan msg)

instance S.Stoppable (Queue msg) where
    stop = closeQueue

newQueue :: IO (Queue msg)
newQueue = Queue <$> newTMChanIO

sendToQueue :: Queue msg -> msg -> IO ()
sendToQueue (Queue chan) = atomically . writeTMChan chan

closeQueue :: Queue msg -> IO ()
closeQueue (Queue chan) = atomically $ closeTMChan chan

data Agent state msg = Agent (Queue msg) (IO state) (state -> IO ()) (state -> msg -> IO state)

instance S.Stoppable (Agent state msg) where
    stop = stop

newAgent :: Queue msg -> IO state -> (state -> IO ()) -> (state -> msg -> IO state) -> Agent state msg
newAgent =  Agent

newAgent' :: Queue msg -> state -> (state -> msg -> IO state) -> Agent state msg
newAgent' q s = newAgent q (return s) (\_ -> return ())

start :: Agent state msg -> IO ThreadId
start = fork . run

stop :: Agent state msg -> IO ()
stop (Agent q _ _ _) = closeQueue q

run :: Agent state msg -> IO ()
run (Agent (Queue chan) setUp tearDown process) =
    setUp >>= loop >>= tearDown
    where
        loop state = do
            msg <- atomically $ readTMChan chan
            case msg of
                Nothing   -> return state
                Just msg' -> process state msg' >>= loop

sendToAgent :: Agent state msg -> msg -> IO ()
sendToAgent (Agent q _ _ _) = sendToQueue q

