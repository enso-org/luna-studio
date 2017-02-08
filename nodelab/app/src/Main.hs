{-# LANGUAGE RecursiveDo #-}
module Main where

import           Control.Concurrent.Chan              (Chan)
import qualified Control.Concurrent.Chan              as Chan
import           Control.Concurrent.MVar
import           Data.DateTime                        (getCurrentTime)
import           Luna.Studio.Prelude
import qualified React.Flux                           as React
import           System.Random                        (newStdGen)

import qualified JS.Config                            as Config
import qualified JS.GraphLocation                     as GraphLocation
import           JS.UUID                              (generateUUID)
import           JS.WebSocket                         (WebSocket)
import qualified Luna.Studio.Batch.Workspace          as Workspace
import qualified Luna.Studio.Event.Engine             as Engine
import qualified Luna.Studio.React.Store              as Store
import qualified Luna.Studio.React.View.App           as App
import           Luna.Studio.State.Global             (mkState)
import qualified Luna.Studio.State.Global             as Global


runApp :: Chan (IO ()) -> WebSocket -> IO ()
runApp chan socket = do
    lastLocation <- GraphLocation.loadLocation
    random       <- newStdGen
    clientId             <- generateUUID
    initTime             <- getCurrentTime
    mdo
        Engine.scheduleInit chan state
        appRef <- Store.createApp $ Engine.scheduleEvent chan state
        React.reactRender Config.mountPoint (App.app appRef) ()
        let initState = mkState initTime clientId random appRef
                      & Global.workspace . Workspace.lastUILocation .~ lastLocation
        state <- newMVar initState
        Engine.connectEventSources socket chan state
    App.focus

main :: IO ()
main = do
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.startLoop chan
