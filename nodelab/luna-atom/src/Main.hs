{-# LANGUAGE RecursiveDo #-}
module Main where

import           Control.Concurrent.Chan     (Chan)
import qualified Control.Concurrent.Chan     as Chan
import           Control.Concurrent.MVar
import           Data.DateTime               (getCurrentTime)
import           Luna.Prelude
import           System.Random               (newStdGen)

import qualified JS.GraphLocation            as GraphLocation
import           JS.UUID                     (generateUUID)
import           WebSocket                (WebSocket)
import qualified Internal.Batch.Workspace as Workspace
import           Internal.Event.Engine    (LoopRef (LoopRef))
import qualified Internal.Event.Engine    as Engine
import           Internal.State.Global    (mkState)
import qualified Internal.State.Global    as Global


runApp :: Chan (IO ()) -> WebSocket -> IO ()
runApp chan socket = do
    lastLocation <- GraphLocation.loadLocation
    random       <- newStdGen
    clientId             <- generateUUID
    initTime             <- getCurrentTime
    mdo
        let loop = LoopRef chan state
        Engine.scheduleInit loop
        let initState = mkState initTime clientId random
                      & Global.workspace . Workspace.lastUILocation .~ lastLocation
        state <- newMVar initState
        Engine.connectEventSources socket loop

main :: IO ()
main = do
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.start chan
