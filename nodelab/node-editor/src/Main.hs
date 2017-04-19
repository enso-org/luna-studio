{-# LANGUAGE RecursiveDo #-}
module Main where

import           Control.Concurrent.Chan     (Chan)
import qualified Control.Concurrent.Chan     as Chan
import           Control.Concurrent.MVar
import           Data.DateTime               (getCurrentTime)
import           Luna.Prelude
import qualified React.Flux                  as React
import           System.Random               (newStdGen)

import qualified JS.Config                   as Config
import qualified JS.GraphLocation            as GraphLocation
import           JS.UUID                     (generateUUID)
import           WebSocket                   (WebSocket)
import qualified Node.Editor.Batch.Workspace as Workspace
import           Node.Editor.Event.Engine    (LoopRef (LoopRef))
import qualified Node.Editor.Event.Engine    as Engine
import qualified Node.Editor.React.Store     as Store
import qualified Node.Editor.React.View.App  as App
import           Node.Editor.State.Global    (mkState)
import qualified Node.Editor.State.Global    as Global


runApp :: Chan (IO ()) -> WebSocket -> IO ()
runApp chan socket = do
    lastLocation <- GraphLocation.loadLocation
    random       <- newStdGen
    clientId             <- generateUUID
    initTime             <- getCurrentTime
    let openedFile = fromMaybe def Config.openedFile
    mdo
        let loop = LoopRef chan state
        Engine.scheduleInit loop
        appRef <- Store.createApp $ Engine.scheduleEvent loop
        React.reactRender Config.mountPoint (App.app appRef) ()
        let initState = mkState appRef clientId openedFile initTime random
                      & Global.workspace . Workspace.lastUILocation .~ lastLocation
        state <- newMVar initState
        Engine.connectEventSources socket loop
    App.focus

main :: IO ()
main = do
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.start chan
