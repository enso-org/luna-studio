module Reactive.Plugins.Core.Action.Backend.Graph
    ( toAction
    ) where


import qualified Batch.Workspace                             as Workspace
import           Utils.PreludePlus

import qualified Empire.API.Data.Connection                  as Connection
import qualified Empire.API.Data.Graph                       as Graph
import           Empire.API.Data.GraphLocation               (GraphLocation (..))
import qualified Empire.API.Data.Node                        as Node
import qualified Empire.API.Graph.AddNode                    as AddNode
import qualified Empire.API.Graph.AddSubgraph                as AddSubgraph
import qualified Empire.API.Graph.CodeUpdate                 as CodeUpdate
import qualified Empire.API.Graph.Connect                    as Connect
import qualified Empire.API.Graph.Disconnect                 as Disconnect
import qualified Empire.API.Graph.GetProgram                 as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate           as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearcherUpdate         as NodeSearcherUpdate
import qualified Empire.API.Graph.NodesUpdate                as NodesUpdate
import qualified Empire.API.Graph.RemoveNodes                as RemoveNodes
import qualified Empire.API.Graph.RenameNode                 as RenameNode
import qualified Empire.API.Graph.UpdateNodeMeta             as UpdateNodeMeta
import qualified Empire.API.Response                         as Response

import           Event.Batch                                 (Event (..))
import qualified Event.Event                                 as Event

import           Reactive.Commands.Batch                     (collaborativeModify, requestCollaborationRefresh)
import           Reactive.Commands.Camera                    (autoZoom)
import           Reactive.Commands.Command                   (Command, performIO)
import           Reactive.Commands.Graph                     (updateConnection)
import           Reactive.Commands.Graph.Connect             (localConnectNodes)
import           Reactive.Commands.Graph.Disconnect          (localDisconnectAll)
import           Reactive.Commands.Graph.Render              (renderGraph)
import           Reactive.Commands.Graph.Selection           (selectNodes)
import           Reactive.Commands.Node                      (renameNode)
import           Reactive.Commands.Node.Create               (addDummyNode)
import           Reactive.Commands.Node.NodeMeta             (updateNodesMeta)
import           Reactive.Commands.Node.Remove               (localRemoveNodes)
import           Reactive.Commands.Node.Update               (updateNode, updateNodeProfilingData, updateNodeValue)
import           Reactive.Commands.ProjectManager            (displayCurrentBreadcrumb)
import           Reactive.Commands.UUID                      (isOwnRequest)
import           Reactive.Plugins.Core.Action.Backend.Common (doNothing, handleResponse)
import           Reactive.State.Global                       (State)
import qualified Reactive.State.Global                       as Global

import qualified JS.TextEditor                               as UI


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ Global.workspace . Workspace.isGraphLoaded
    return $ icl && igl

toAction :: Event.Event -> Maybe (Command State ())
toAction (Event.Batch ev) = Just $ case ev of
    ProgramFetched response -> handleResponse response $ \_ result -> do
            let location = response ^. Response.request . GetProgram.location
            isGraphLoaded  <- use $ Global.workspace . Workspace.isGraphLoaded
            isGoodLocation <- isCurrentLocation location
            when (isGoodLocation && not isGraphLoaded) $ do
                let nodes       = result ^. GetProgram.graph . Graph.nodes
                    connections = result ^. GetProgram.graph . Graph.connections
                    code        = result ^. GetProgram.code
                    nsData      = result ^. GetProgram.nodeSearcherData
                    breadcrumb  = result ^. GetProgram.breadcrumb

                Global.workspace . Workspace.nodeSearcherData .= nsData
                displayCurrentBreadcrumb breadcrumb
                renderGraph nodes connections
                autoZoom
                performIO $ UI.setText code
                Global.workspace . Workspace.isGraphLoaded .= True
                requestCollaborationRefresh

    AddNodeResponse response@(Response.Response uuid (AddNode.Request loc _ _ _) _) -> do
        shouldProcess   <- isCurrentLocationAndGraphLoaded loc
        correctLocation <- isCurrentLocation loc
        shouldSelect    <- isOwnRequest uuid
        handleResponse response $ \_ node -> do
            when (shouldProcess && correctLocation) $ do
                addDummyNode node
                let nodeId = node ^. Node.nodeId
                collaborativeModify [nodeId]
                when shouldSelect $ selectNodes [nodeId]

    AddSubgraphResponse response@(Response.Response uuid (AddSubgraph.Request loc nodes connections) _) -> do
        shouldProcess   <- isCurrentLocationAndGraphLoaded loc
        correctLocation <- isCurrentLocation loc
        when (shouldProcess && correctLocation) $ do
            mapM_ addDummyNode nodes
            connectionIds <- forM connections $ \conn -> localConnectNodes (conn ^. Connection.src) (conn ^. Connection.dst)
            mapM_ updateConnection connectionIds
            whenM (isOwnRequest uuid) $ do
                let nodeIds = map (^. Node.nodeId) nodes
                collaborativeModify nodeIds
                selectNodes nodeIds
        handleResponse response doNothing

    NodesConnected update -> do
        whenM (isCurrentLocation $ update ^. Connect.location') $ do
            connectionId <- localConnectNodes (update ^. Connect.src') (update ^. Connect.dst')
            updateConnection connectionId

    NodesDisconnected update -> do
        whenM (isCurrentLocation $ update ^. Disconnect.location') $ do
            localDisconnectAll $ [update ^. Disconnect.dst']

    NodeMetaUpdated update -> do
        shouldProcess   <- isCurrentLocationAndGraphLoaded (update ^. UpdateNodeMeta.location')
        correctLocation <- isCurrentLocation (update ^. UpdateNodeMeta.location')
        when (shouldProcess && correctLocation) $ updateNodesMeta (update ^. UpdateNodeMeta.updates')

    NodeAdded update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. AddNode.location')
        correctLocation <- isCurrentLocation (update ^. AddNode.location')
        when (shouldProcess && correctLocation) $ addDummyNode (update ^. AddNode.node')

    NodesUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodesUpdate.location)
        correctLocation <- isCurrentLocation (update ^. NodesUpdate.location)
        when (shouldProcess && correctLocation) $ mapM_ updateNode $ update ^. NodesUpdate.nodes

    NodeRenamed update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. RenameNode.location')
        correctLocation <- isCurrentLocation (update ^. RenameNode.location')
        when (shouldProcess && correctLocation) $ renameNode (update ^. RenameNode.nodeId') (update ^. RenameNode.name')

    NodesRemoved update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. RemoveNodes.location')
        correctLocation <- isCurrentLocation (update ^. RemoveNodes.location')
        when (shouldProcess && correctLocation) $ localRemoveNodes $ update ^. RemoveNodes.nodeIds'

    NodeResultUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodeResultUpdate.location)
        correctLocation <- isCurrentLocation (update ^. NodeResultUpdate.location)
        when (shouldProcess && correctLocation) $ do
            updateNodeValue         (update ^. NodeResultUpdate.nodeId) (update ^. NodeResultUpdate.value)
            updateNodeProfilingData (update ^. NodeResultUpdate.nodeId) (update ^. NodeResultUpdate.execTime)

    NodeSearcherUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodeSearcherUpdate.location)
        correctLocation <- isCurrentLocation (update ^. NodeSearcherUpdate.location)
        when (shouldProcess && correctLocation) $ Global.workspace . Workspace.nodeSearcherData .= update ^. NodeSearcherUpdate.nodeSearcherData

    CodeUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. CodeUpdate.location)
        correctLocation <- isCurrentLocation (update ^. CodeUpdate.location)
        when (shouldProcess && correctLocation) $ performIO $ UI.setText $ update ^. CodeUpdate.code

    -- CollaborationUpdate update -> -- handled in Collaboration.hs
    RemoveNodesResponse          response -> handleResponse response doNothing
    ConnectResponse              response -> handleResponse response doNothing
    DisconnectResponse           response -> handleResponse response doNothing
    NodeMetaResponse             response -> handleResponse response doNothing
    NodeRenameResponse           response -> handleResponse response doNothing
    UpdateNodeExpressionResponse response -> handleResponse response doNothing

    _ -> return ()
toAction _ = Nothing
