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

import           Data.DateTime                     (getCurrentTime)
import qualified Data.Set                          as Set
import qualified React.Flux                        as React
import           System.Random                     (newStdGen)
import           Utils.PreludePlus

import qualified Batch.Workspace                   as Workspace
import qualified BatchConnector.Commands           as BatchCmd
import           Control.Concurrent.MVar
import qualified JS.GraphLocation                  as GraphLocation
import           JS.Tutorial                       (shouldRunTutorial)
import           JS.Tutorial                       (showStep)
import           JS.UI                             (initializeGl, initializeHelp, render, triggerWindowResize)
import           JS.UUID                           (generateUUID)
import           JS.WebSocket                      (WebSocket)
import qualified React.Dispatcher                  as Dispatcher
import qualified React.Store.Nodelab               as Nodelab
import qualified React.View.Nodelab                as Nodelab
import           Reactive.Commands.Command         (execCommand)
import qualified Reactive.Plugins.Core.Action.Init as Init
import qualified Reactive.Plugins.Core.Network     as CoreNetwork
import qualified Reactive.Plugins.Loader.Loader    as Loader
import           Reactive.State.Global             (initialState)
import qualified Reactive.State.Global             as Global



runMainNetwork :: WebSocket -> IO ()
runMainNetwork socket = do
    store <- Nodelab.store
    store1 <- Nodelab.store
    store2 <- Nodelab.store
    React.reactRender "nodelab-app" (Nodelab.nodelabApp store1 store2 store) $ Nodelab.Props 10

    React.alterStore store1 Nodelab.Sub
    React.alterStore store2 Nodelab.Add
    -- initializeGl
    -- initializeHelp
    -- render

    lastLocation <- GraphLocation.loadLocation

    random <- newStdGen
    projectListRequestId <- generateUUID
    clientId             <- generateUUID
    initTime             <- getCurrentTime
    tutorial'            <- shouldRunTutorial
    let tutorial = if tutorial' then Just 0 else Nothing

    withJust tutorial $ \step -> showStep step


    let initState = initialState initTime clientId random tutorial & Global.workspace . Workspace.lastUILocation .~ lastLocation
                                                                   & Global.pendingRequests %~ Set.insert projectListRequestId
    let (initActions, initState') = execCommand Init.initialize initState
    -- initActions

    state <- newMVar initState'
    -- state <- newMVar initState
    -- CoreNetwork.makeNetworkDescription socket state
    -- triggerWindowResize
    --
    -- BatchCmd.listProjects projectListRequestId
    return ()

main :: IO ()
main = Loader.withActiveConnection runMainNetwork
