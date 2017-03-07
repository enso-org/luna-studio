module Luna.Studio.Handler.Backend.Graph
    ( handle
    ) where

import qualified Data.Map                               as Map

import qualified Luna.Studio.Batch.Workspace            as Workspace
import           Luna.Studio.Prelude

import qualified Empire.API.Data.Connection             as Connection
import qualified Empire.API.Data.Graph                  as Graph
import           Empire.API.Data.GraphLocation          (GraphLocation (..))
import qualified Empire.API.Data.Node                   as Node
import           Empire.API.Data.Port                   (InPort (Arg), OutPort (Projection), PortId (InPortId, OutPortId))
import qualified Empire.API.Data.PortRef                as PortRef
import qualified Empire.API.Graph.AddNode               as AddNode
import qualified Empire.API.Graph.AddSubgraph           as AddSubgraph
import qualified Empire.API.Graph.CodeUpdate            as CodeUpdate
import qualified Empire.API.Graph.Connect               as Connect
import qualified Empire.API.Graph.Disconnect            as Disconnect
import qualified Empire.API.Graph.GetProgram            as GetProgram
import qualified Empire.API.Graph.MonadsUpdate          as MonadsUpdate
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearch            as NodeSearch
import qualified Empire.API.Graph.NodesUpdate           as NodesUpdate
import qualified Empire.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate
import qualified Empire.API.Graph.RemoveNodes           as RemoveNodes
import qualified Empire.API.Graph.RemovePort            as RemovePort
import qualified Empire.API.Graph.RenameNode            as RenameNode
import qualified Empire.API.Graph.RenamePort            as RenamePort
import qualified Empire.API.Graph.SetCode               as SetCode
import qualified Empire.API.Graph.UpdateNodeMeta        as UpdateNodeMeta
import qualified Empire.API.Response                    as Response

import           Luna.Studio.Event.Batch                (Event (..))
import qualified Luna.Studio.Event.Event                as Event

import           Luna.Studio.Action.Graph.AddNode       (localAddNode, localUpdateNode)

import           Luna.Studio.Action.Batch               (collaborativeModify, requestCollaborationRefresh)
import           Luna.Studio.Action.Camera              (centerGraph)
import qualified Luna.Studio.Action.CodeEditor          as CodeEditor
import           Luna.Studio.Action.Command             (Command)
import qualified Luna.Studio.Action.Edge                as Edge
import           Luna.Studio.Action.Graph               (createGraph, localAddConnection, localRemoveConnections, selectNodes,
                                                         updateConnectionsForEdges, updateConnectionsForNodes, updateMonads)
import           Luna.Studio.Action.Node                (localRemoveNodes, typecheckNode, updateNodeProfilingData, updateNodeValue,
                                                         updateNodesMeta)
import qualified Luna.Studio.Action.Node                as Node
import           Luna.Studio.Action.ProjectManager      (setCurrentBreadcrumb)
import qualified Luna.Studio.Action.Searcher            as Searcher
import           Luna.Studio.Action.UUID                (isOwnRequest)
import           Luna.Studio.Handler.Backend.Common     (doNothing, handleResponse)
import           Luna.Studio.State.Global               (State)
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as StateGraph


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
                monads      = result ^. GetProgram.graph . Graph.monads
                code        = result ^. GetProgram.code
                nsData      = result ^. GetProgram.nodeSearcherData
                breadcrumb  = result ^. GetProgram.breadcrumb

            Global.workspace . Workspace.nodeSearcherData .= nsData
            setCurrentBreadcrumb breadcrumb
            createGraph nodes connections monads
            centerGraph
            CodeEditor.setCode code
            Global.workspace . Workspace.isGraphLoaded .= True
            requestCollaborationRefresh

    AddNodeResponse response@(Response.Response uuid _ (AddNode.Request loc _ _ _ _) _ _) -> do
        shouldProcess   <- isCurrentLocationAndGraphLoaded loc
        shouldSelect    <- isOwnRequest uuid
        handleResponse response $ \_ node -> when shouldProcess $ do
            localAddNode node
            let nodeId = node ^. Node.nodeId
            collaborativeModify [nodeId]
            when shouldSelect $ selectNodes [nodeId]

    AddSubgraphResponse response@(Response.Response uuid _ (AddSubgraph.Request loc nodes connections) _ (Response.Ok newNodes)) -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded loc
        --TODO[LJK]: Implement this correctly
        return ()

    NodesConnected update ->
        whenM (isCurrentLocation $ update ^. Connect.location') $ void $
            localAddConnection $ update ^. Connect.connection'

    NodesDisconnected update ->
        whenM (isCurrentLocation $ update ^. Disconnect.location') $
            localRemoveConnections [update ^. Disconnect.dst']

    NodeMetaUpdated update -> do
        shouldProcess   <- isCurrentLocationAndGraphLoaded (update ^. UpdateNodeMeta.location')
        when shouldProcess $ do
            updateNodesMeta (update ^. UpdateNodeMeta.updates')
            updateConnectionsForNodes $ fst <$> (update ^. UpdateNodeMeta.updates')

    NodeAdded update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. AddNode.location')
        when shouldProcess $ localAddNode (update ^. AddNode.node')

    NodesUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodesUpdate.location)
        when shouldProcess $ mapM_ localUpdateNode $ update ^. NodesUpdate.nodes

    MonadsUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. MonadsUpdate.location)
        when shouldProcess $ updateMonads $ update ^. MonadsUpdate.monads

    NodeTypechecked update -> do
      shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodeTCUpdate.location)
      when shouldProcess $ typecheckNode $ update ^. NodeTCUpdate.node

    NodeRenamed update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. RenameNode.location')
        when shouldProcess $ Node.rename (update ^. RenameNode.nodeId') (update ^. RenameNode.name')

    PortRenamed update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. RenamePort.location')
        when shouldProcess $ Edge.portRename (update ^. RenamePort.portRef') (update ^. RenamePort.name')

    NodeCodeSet update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. SetCode.location')
        correctLocation <- isCurrentLocation (update ^. SetCode.location')
        when (shouldProcess && correctLocation) $ Node.setCode (update ^. SetCode.nodeId') (update ^. SetCode.code')

    NodesRemoved update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. RemoveNodes.location')
        when shouldProcess $ localRemoveNodes $ update ^. RemoveNodes.nodeIds'

    NodeResultUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodeResultUpdate.location)
        when shouldProcess $ do
            updateNodeValue         (update ^. NodeResultUpdate.nodeId) (update ^. NodeResultUpdate.value)
            updateNodeProfilingData (update ^. NodeResultUpdate.nodeId) (update ^. NodeResultUpdate.execTime)
            updateConnectionsForNodes [update ^. NodeResultUpdate.nodeId]

    NodeSearchResponse response -> handleResponse response $ \request result -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (request ^. NodeSearch.location)
        when shouldProcess $ do
            Global.workspace . Workspace.nodeSearcherData .= result ^. NodeSearch.nodeSearcherData
            Searcher.updateHints

    CodeUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. CodeUpdate.location)
        when shouldProcess $ CodeEditor.setCode $ update ^. CodeUpdate.code

    RemovePortResponse response -> handleResponse response $ \request result -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (request ^. RemovePort.location)
        when shouldProcess $ do
            let portRef = request ^. RemovePort.anyPortRef
                nodeId  = portRef ^. PortRef.nodeId
                portId  = portRef ^. PortRef.portId
            localUpdateNode result
            graph <- use Global.graph
            localRemoveConnections $ map (view Connection.dst) $ StateGraph.connectionsContainingPort portRef graph
            let shouldUpdate = case portRef ^. PortRef.portId of
                    InPortId  (Arg _)        -> True
                    OutPortId (Projection _) -> True
                    _                        -> False
            when shouldUpdate $ do
                graph' <- use Global.graph
                let connectionsToUpdate = StateGraph.connectionsContainingNodes [nodeId] graph'
                forM_ connectionsToUpdate $ \conn -> do
                    let src = conn ^. Connection.src
                    let dst = conn ^. Connection.dst
                    if src ^. PortRef.srcNodeId == nodeId then case (src ^. PortRef.srcPortId, portId) of
                            (Projection num, OutPortId (Projection num')) -> when (num > num') $ do
                                Global.graph . StateGraph.connectionsMap . at dst ?=
                                    (conn & Connection.src . PortRef.srcPortId .~ Projection (num - 1))
                            _ -> return ()
                        else case (dst ^. PortRef.dstPortId, portId) of
                            (Arg num, InPortId (Arg num')) -> when (num > num') $ do
                                Global.graph . StateGraph.connectionsMap . at dst .= Nothing
                                let newConn = conn & Connection.src . PortRef.srcPortId .~ Projection (num - 1)
                                Global.graph . StateGraph.connectionsMap . at (newConn ^. Connection.dst) ?= newConn
                            _ -> return ()
                updateConnectionsForEdges

    -- CollaborationUpdate update -> -- handled in Collaboration.hs
    AddPortResponse              response -> handleResponse response doNothing
    MovePortResponse             response -> handleResponse response doNothing
    ConnectResponse              response -> handleResponse response doNothing
    DisconnectResponse           response -> handleResponse response doNothing
    NodeMetaResponse             response -> handleResponse response doNothing
    NodeRenameResponse           response -> handleResponse response doNothing
    RemoveNodesResponse          response -> print response >> handleResponse response doNothing
    UpdateNodeExpressionResponse response -> handleResponse response doNothing

    _ -> return ()
handle _ = Nothing
