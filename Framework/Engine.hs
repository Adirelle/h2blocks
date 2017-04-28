module Framework.Engine
    ( Engine
    , newEngine
    , update
    , tick
    , Framework.Agents.start
    , Framework.Agents.stop
    )
    where

import           Framework.Agents
import qualified I3BarIPC.Input   as I
import qualified I3BarIPC.Output  as O

data Event = BlockUpdate !Int !O.Block | Tick

type Engine = Agent EngineState Event

data EngineState = EngineState
    { line   :: !O.Statusline
    , output :: !O.Output
    }

newEngine :: Int -> O.Output -> IO Engine
newEngine numBlocks output = do
    let state = EngineState (O.emptyStatusline numBlocks) output
    queue <- newQueue
    return $ newAgent' queue state handleEvent

update :: Engine -> Int -> O.Block -> IO ()
update e i b = sendToAgent e $ BlockUpdate i b

tick :: Engine -> IO ()
tick e = sendToAgent e Tick

handleEvent :: EngineState -> Event -> IO EngineState

handleEvent e @ EngineState { output = output, line = line } Tick = do
    O.emit output line
    return e

handleEvent e @ EngineState { line =  line } (BlockUpdate i b) =
    return $ e { line = O.setBlock line i b }
