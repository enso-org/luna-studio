module Luna.Studio.Handler.Backend.Graph
    ( handle
    ) where


import qualified Luna.Studio.Batch.Workspace         as Workspace
import           Luna.Studio.Prelude

import qualified Empire.API.Data.Connection          as Connection
import qualified Empire.API.Data.Graph               as Graph
import           Empire.API.Data.GraphLocation       (GraphLocation (..))
import qualified Empire.API.Data.Node                as Node
import qualified Empire.API.Graph.AddNode            as AddNode
import qualified Empire.API.Graph.AddSubgraph        as AddSubgraph
import qualified Empire.API.Graph.CodeUpdate         as CodeUpdate
import qualified Empire.API.Graph.Connect            as Connect
import qualified Empire.API.Graph.Disconnect         as Disconnect
import qualified Empire.API.Graph.GetProgram         as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate   as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearcherUpdate as NodeSearcherUpdate
import qualified Empire.API.Graph.NodesUpdate        as NodesUpdate
import qualified Empire.API.Graph.RemoveNodes        as RemoveNodes
import qualified Empire.API.Graph.RenameNode         as RenameNode
import qualified Empire.API.Graph.UpdateNodeMeta     as UpdateNodeMeta
import qualified Empire.API.Response                 as Response

import           Luna.Studio.Event.Batch             (Event (..))
import qualified Luna.Studio.Event.Event             as Event

import           Luna.Studio.Action.Batch            (collaborativeModify, requestCollaborationRefresh)
import           Luna.Studio.Action.Camera           (centerGraph)
import qualified Luna.Studio.Action.CodeEditor       as CodeEditor
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.Graph            (localConnectNodes, localRemoveConnections, renderGraph, selectNodes,
                                                      updateConnectionsForNodes)
import           Luna.Studio.Action.Node             (addDummyNode, localRemoveNodes, updateNode, updateNodeProfilingData, updateNodeValue,
                                                      updateNodesMeta)
import qualified Luna.Studio.Action.Node             as Node
import           Luna.Studio.Action.ProjectManager   (setCurrentBreadcrumb)
import           Luna.Studio.Action.UUID             (isOwnRequest)
import           Luna.Studio.Handler.Backend.Common  (doNothing, handleResponse)
import           Luna.Studio.State.Global            (State)
import qualified Luna.Studio.State.Global            as Global


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ Global.workspace . Workspace.isGraphLoaded
    return $ icl && igl

handle :: Event.Event -> Maybe (Command State ())
handle (Event.Batch ev) = Just $ case ev of
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
            setCurrentBreadcrumb breadcrumb
            renderGraph nodes connections
            centerGraph
            CodeEditor.setCode code
            Global.workspace . Workspace.isGraphLoaded .= True
            requestCollaborationRefresh

    AddNodeResponse response@(Response.Response uuid guiID (AddNode.Request loc _ _ _ _) _ _) -> do
        shouldProcess   <- isCurrentLocationAndGraphLoaded loc
        correctLocation <- isCurrentLocation loc
        shouldSelect    <- isOwnRequest uuid
        handleResponse response $ \_ node ->
            when (shouldProcess && correctLocation) $ do
                addDummyNode node
                let nodeId = node ^. Node.nodeId
                collaborativeModify [nodeId]
                when shouldSelect $ selectNodes [nodeId]

    AddSubgraphResponse response@(Response.Response uuid guiID (AddSubgraph.Request loc nodes connections _) _ _) -> do
        shouldProcess   <- isCurrentLocationAndGraphLoaded loc
        correctLocation <- isCurrentLocation loc
        when (shouldProcess && correctLocation) $ do
            mapM_ addDummyNode nodes
            -- TODO[react]: Find out if we need this
            forM_ connections $ \conn -> localConnectNodes (conn ^. Connection.src) (conn ^. Connection.dst)
            -- mapM_ updateConnection connectionIds
            whenM (isOwnRequest uuid) $ do
                let nodeIds = map (^. Node.nodeId) nodes
                collaborativeModify nodeIds
                selectNodes nodeIds
        handleResponse response doNothing

    NodesConnected update ->
        whenM (isCurrentLocation $ update ^. Connect.location') $
            void $ localConnectNodes (update ^. Connect.src') (update ^. Connect.dst')

    NodesDisconnected update ->
        whenM (isCurrentLocation $ update ^. Disconnect.location') $
            localRemoveConnections [update ^. Disconnect.dst']

    NodeMetaUpdated update -> do
        shouldProcess   <- isCurrentLocationAndGraphLoaded (update ^. UpdateNodeMeta.location')
        correctLocation <- isCurrentLocation (update ^. UpdateNodeMeta.location')
        when (shouldProcess && correctLocation) $ do
            updateNodesMeta (update ^. UpdateNodeMeta.updates')
            updateConnectionsForNodes $ fst <$> (update ^. UpdateNodeMeta.updates')

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
        when (shouldProcess && correctLocation) $ Node.rename (update ^. RenameNode.nodeId') (update ^. RenameNode.name')

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
        when (shouldProcess && correctLocation) $ CodeEditor.setCode $ update ^. CodeUpdate.code

    -- CollaborationUpdate update -> -- handled in Collaboration.hs
    RemoveNodesResponse          response -> handleResponse response doNothing
    ConnectResponse              response -> handleResponse response doNothing
    DisconnectResponse           response -> handleResponse response doNothing
    NodeMetaResponse             response -> handleResponse response doNothing
    NodeRenameResponse           response -> handleResponse response doNothing
    UpdateNodeExpressionResponse response -> handleResponse response doNothing

    _ -> return ()
handle _ = Nothing
