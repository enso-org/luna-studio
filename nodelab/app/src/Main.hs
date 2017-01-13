{-# LANGUAGE RecursiveDo #-}
module Main where


--      _|      _|
--      _|_|    _|    _|_|    _|      _|      _|
--      _|  _|  _|  _|_|_|_|  _|      _|      _|
--      _|    _|_|  _|          _|  _|  _|  _|
--      _|      _|    _|_|_|      _|      _|

--      _|_|_|                _|
--      _|    _|  _|    _|  _|_|_|_|    _|_|
--      _|_|_|    _|    _|    _|      _|_|_|_|
--      _|    _|  _|    _|    _|      _|
--      _|_|_|      _|_|_|      _|_|    _|_|_|
--                      _|
--                  _|_|

--        _|_|                    _|
--      _|    _|  _|  _|_|    _|_|_|    _|_|    _|  _|_|
--      _|    _|  _|_|      _|    _|  _|_|_|_|  _|_|
--      _|    _|  _|        _|    _|  _|        _|
--        _|_|    _|          _|_|_|    _|_|_|  _|


-- http://www.network-science.de/ascii/

import           Data.DateTime                        (getCurrentTime)
import qualified Data.Set                             as Set
import           Luna.Studio.Prelude
import qualified React.Flux                           as React
import           System.Random                        (newStdGen)

import           Control.Concurrent.MVar
import qualified JS.GraphLocation                     as GraphLocation
import           JS.Tutorial                          (shouldRunTutorial)
import           JS.Tutorial                          (showStep)
import           JS.UUID                              (generateUUID)
import           JS.WebSocket                         (WebSocket)
import qualified Luna.Studio.Batch.Connector.Commands as BatchCmd
import qualified Luna.Studio.Batch.Workspace          as Workspace
import qualified Luna.Studio.Engine                   as Engine
import qualified Luna.Studio.React.Store              as Store
import qualified Luna.Studio.React.View.App           as App
import           Luna.Studio.State.Global             (initialState)
import qualified Luna.Studio.State.Global             as Global



runApp :: WebSocket -> IO ()
runApp socket = do
    lastLocation <- GraphLocation.loadLocation
    random <- newStdGen
    projectListRequestId <- generateUUID
    clientId             <- generateUUID
    initTime             <- getCurrentTime
    tutorial'            <- shouldRunTutorial
    let tutorial = if tutorial' then Just 0 else Nothing
    withJust tutorial $ \step -> showStep step

    mdo
        appRef <- Store.createApp $ Engine.processEvent state
        React.reactRender "nodelab-app" (App.app appRef) ()

        let initState = initialState initTime clientId random tutorial appRef
                      & Global.workspace . Workspace.lastUILocation .~ lastLocation
                      & Global.pendingRequests %~ Set.insert projectListRequestId

        state <- newMVar initState
        Engine.start socket state

    App.focus
    BatchCmd.listProjects projectListRequestId

main :: IO ()
main = Engine.withActiveConnection runApp
