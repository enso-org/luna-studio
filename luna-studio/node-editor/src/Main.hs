{-# LANGUAGE RecursiveDo #-}
module Main where

import           Common.Prelude
import           Control.Concurrent.Chan    (Chan)
import qualified Control.Concurrent.Chan    as Chan
import           Control.Concurrent.MVar
import           Data.DateTime              (getCurrentTime)
import qualified React.Flux                 as React
import           System.Random              (newStdGen)

import qualified JS.Config                  as Config
import qualified JS.GraphLocation           as GraphLocation
import           JS.UUID                    (generateUUID)
import qualified NodeEditor.Batch.Workspace as Workspace
import           NodeEditor.Event.Engine    (LoopRef (LoopRef))
import qualified NodeEditor.Event.Engine    as Engine
import qualified NodeEditor.React.Store     as Store
import qualified NodeEditor.React.View.App  as App
import           NodeEditor.State.Global    (mkState)
import qualified NodeEditor.State.Global    as Global
import           WebSocket                  (WebSocket)


runApp :: Chan (IO ()) -> WebSocket -> IO ()
runApp chan socket = do
    lastLocation <- GraphLocation.loadLocation
    random       <- newStdGen
    clientId             <- generateUUID
    initTime             <- getCurrentTime
    let openedFile = Config.openedFile
    mdo
        let loop = LoopRef chan state
        Engine.scheduleInit loop
        appRef <- Store.createApp $ Engine.scheduleEvent loop
        React.reactRender Config.mountPoint (App.app appRef) ()
        let initState = mkState appRef clientId openedFile initTime random
                      & Global.workspace . _Just . Workspace.lastUILocation .~ lastLocation
        state <- newMVar initState
        Engine.connectEventSources socket loop
    App.focus

main :: IO ()
main = do
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.start chan
