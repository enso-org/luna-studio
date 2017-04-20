module NodeEditor.Event.Loop where

import           Control.Concurrent.Chan  (Chan)
import qualified Control.Concurrent.Chan  as Chan
import           Control.Concurrent.MVar  (MVar)
import           Control.Monad            (forever)
import           Common.Prelude
import           NodeEditor.State.Global (State)


data LoopRef = LoopRef { _queue :: Chan (IO ())
                       , _state :: MVar State
                       }

makeLenses ''LoopRef

schedule :: LoopRef -> IO () -> IO ()
schedule loop = Chan.writeChan (loop ^. queue)

start :: Chan (IO ()) -> IO ()
start = forever . join . Chan.readChan
