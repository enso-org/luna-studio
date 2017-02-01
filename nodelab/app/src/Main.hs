{-# LANGUAGE RecursiveDo #-}
module Main where

import           Control.Concurrent.Chan              (Chan)
import qualified Control.Concurrent.Chan              as Chan
import           Control.Concurrent.MVar
import           Data.DateTime                        (getCurrentTime)
import qualified Data.Set                             as Set
import           Luna.Studio.Prelude
import qualified React.Flux                           as React
import           System.Random                        (newStdGen)

import qualified JS.Atom                              as Atom
import qualified JS.GraphLocation                     as GraphLocation
import           JS.UUID                              (generateUUID)
import           JS.WebSocket                         (WebSocket)
import qualified Luna.Studio.Batch.Connector.Commands as BatchCmd
import qualified Luna.Studio.Batch.Workspace          as Workspace
import qualified Luna.Studio.Engine                   as Engine
import qualified Luna.Studio.React.Store              as Store
import qualified Luna.Studio.React.View.App           as App
import           Luna.Studio.State.Global             (mkState)
import qualified Luna.Studio.State.Global             as Global



runApp :: Chan (IO ()) -> WebSocket -> IO ()
runApp chan socket = do
    mountPoint   <- fromMaybe "luna-studio-mount" <$> Atom.mountPoint
    lastLocation <- GraphLocation.loadLocation
    random       <- newStdGen
    projectListRequestId <- generateUUID
    clientId             <- generateUUID
    initTime             <- getCurrentTime
    mdo
        appRef <- Store.createApp $ Engine.scheduleEvent chan state
        React.reactRender mountPoint (App.app appRef) ()
        let initState = mkState initTime clientId random appRef
                      & Global.workspace . Workspace.lastUILocation .~ lastLocation
                      & Global.pendingRequests %~ Set.insert projectListRequestId
        state <- newMVar initState
        Engine.connectEventSources socket chan state
    App.focus
    BatchCmd.listProjects projectListRequestId

main :: IO ()
main = do
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.startLoop chan
