{-# LANGUAGE RecursiveDo #-}
module Main where

import           Control.Concurrent.Chan     (Chan)
import qualified Control.Concurrent.Chan     as Chan
import           Control.Concurrent.MVar
import           Data.DateTime               (getCurrentTime)
import           Internal.Prelude
import qualified React.Flux                  as React
import           System.Random               (newStdGen)

import qualified JS.Config                   as Config
import qualified JS.GraphLocation            as GraphLocation
import           JS.UUID                     (generateUUID)
import           JS.WebSocket                (WebSocket)
import qualified Internal.Batch.Workspace as Workspace
import           Internal.Event.Engine    (LoopRef (LoopRef))
import qualified Internal.Event.Engine    as Engine
-- import qualified Internal.React.Store     as Store
-- import qualified Internal.React.View.App  as App
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
        -- appRef <- Store.createApp $ Engine.scheduleEvent loop
        -- React.reactRender Config.mountPoint (App.app appRef) ()
        let initState = mkState initTime clientId random
                      & Global.workspace . Workspace.lastUILocation .~ lastLocation
        state <- newMVar initState
        Engine.connectEventSources socket loop
    -- App.focus

main :: IO ()
main = do
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.start chan
