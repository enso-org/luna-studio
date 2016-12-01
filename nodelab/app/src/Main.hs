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

import           Data.DateTime                  (getCurrentTime)
import qualified Data.Set                       as Set
import qualified React.Flux                     as React
import           System.Random                  (newStdGen)
import           Utils.PreludePlus

import qualified Batch.Workspace                as Workspace
import qualified BatchConnector.Commands        as BatchCmd
import           Control.Concurrent.MVar
import qualified JS.GraphLocation               as GraphLocation
import           JS.Tutorial                    (shouldRunTutorial)
import           JS.Tutorial                    (showStep)
import           JS.UUID                        (generateUUID)
import           JS.WebSocket                   (WebSocket)
import qualified React.Store                    as Store
import qualified React.View.App                 as App
import qualified Reactive.Plugins.Core.Network  as CoreNetwork
import qualified Reactive.Plugins.Loader.Loader as Loader
import           Reactive.State.Global          (initialState)
import qualified Reactive.State.Global          as Global



runMainNetwork :: WebSocket -> IO ()
runMainNetwork socket = do
    lastLocation <- GraphLocation.loadLocation
    random <- newStdGen
    projectListRequestId <- generateUUID
    clientId             <- generateUUID
    initTime             <- getCurrentTime
    tutorial'            <- shouldRunTutorial
    let tutorial = if tutorial' then Just 0 else Nothing
    withJust tutorial $ \step -> showStep step

    mdo
        appRef <- Store.createApp $ CoreNetwork.processEvent state
        React.reactRender "nodelab-app" (App.app appRef) ()

        let initState = initialState initTime clientId random tutorial appRef
                      & Global.workspace . Workspace.lastUILocation .~ lastLocation
                      & Global.pendingRequests %~ Set.insert projectListRequestId

        state <- newMVar initState
        CoreNetwork.makeNetworkDescription socket state

    BatchCmd.listProjects projectListRequestId

main :: IO ()
main = Loader.withActiveConnection runMainNetwork
