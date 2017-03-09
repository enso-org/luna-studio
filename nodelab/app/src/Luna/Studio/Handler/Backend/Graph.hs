module Luna.Studio.Handler.Backend.Graph
    ( handle
    ) where

import qualified Data.DateTime                                  as DT
import qualified Data.Map                                       as Map
import qualified Empire.API.Data.Connection                     as Connection
import qualified Empire.API.Data.Graph                          as Graph
import           Empire.API.Data.GraphLocation                  (GraphLocation (..))
import qualified Empire.API.Data.Node                           as Node
import           Empire.API.Data.Port                           (InPort (Arg), OutPort (Projection), PortId (InPortId, OutPortId))
import qualified Empire.API.Data.PortRef                        as PortRef
import qualified Empire.API.Graph.AddNode                       as AddNode
import qualified Empire.API.Graph.AddPort                       as AddPort
import qualified Empire.API.Graph.AddSubgraph                   as AddSubgraph
import qualified Empire.API.Graph.CodeUpdate                    as CodeUpdate
import qualified Empire.API.Graph.CollaborationUpdate           as CollaborationUpdate
import qualified Empire.API.Graph.Connect                       as Connect
import qualified Empire.API.Graph.GetProgram                    as GetProgram
import qualified Empire.API.Graph.MonadsUpdate                  as MonadsUpdate
import qualified Empire.API.Graph.MovePort                      as MovePort
import qualified Empire.API.Graph.NodeResultUpdate              as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearch                    as NodeSearch
import qualified Empire.API.Graph.NodesUpdate                   as NodesUpdate
import qualified Empire.API.Graph.NodeTypecheckerUpdate         as NodeTCUpdate
import qualified Empire.API.Graph.RemoveConnection              as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes                   as RemoveNodes
import qualified Empire.API.Graph.RemovePort                    as RemovePort
import qualified Empire.API.Graph.RenameNode                    as RenameNode
import qualified Empire.API.Graph.RenamePort                    as RenamePort
import qualified Empire.API.Graph.SetCode                       as SetCode
import qualified Empire.API.Graph.UpdateNodeMeta                as UpdateNodeMeta
import qualified Empire.API.Response                            as Response
import           Luna.Studio.Action.Batch                       (collaborativeModify, requestCollaborationRefresh)
import           Luna.Studio.Action.Camera                      (centerGraph)
import qualified Luna.Studio.Action.CodeEditor                  as CodeEditor
import           Luna.Studio.Action.Command                     (Command)
import qualified Luna.Studio.Action.Edge                        as Edge
import           Luna.Studio.Action.Graph                       (createGraph, localAddConnection, localRemoveConnection, selectNodes,
                                                                 updateConnectionsForEdges, updateConnectionsForNodes, updateMonads)
import           Luna.Studio.Action.Graph.AddNode               (localAddNode)
import           Luna.Studio.Action.Graph.AddPort               (localAddPort)
import           Luna.Studio.Action.Graph.AddSubgraph           (localAddSubgraph)
import           Luna.Studio.Action.Graph.CodeUpdate            (updateCode)
import           Luna.Studio.Action.Graph.CollaborationUpdate   (bumpTime, modifyTime, refreshTime, touchCurrentlySelected, updateClient)
import           Luna.Studio.Action.Graph.MovePort              (localMovePort)
import           Luna.Studio.Action.Graph.NodeResultUpdate      (updateNodeProfilingData, updateNodeValue)
import           Luna.Studio.Action.Graph.NodeSearch            (updateHints)
import           Luna.Studio.Action.Graph.NodesUpdate           (localUpdateNode, localUpdateNodes)
import           Luna.Studio.Action.Graph.NodeTypecheckerUpdate (typecheckNode)
import           Luna.Studio.Action.Graph.RemoveNodes           (localRemoveNodes)
import           Luna.Studio.Action.Graph.Revert                (isCurrentLocation, isCurrentLocationAndGraphLoaded, revertAddNode,
                                                                 revertAddPort, revertAddSubgraph, revertConnect, revertMovePort,
                                                                 revertRemoveConnection, revertRemoveNodes)
import qualified Luna.Studio.Action.Node                        as Node
import           Luna.Studio.Action.ProjectManager              (setCurrentBreadcrumb)
import qualified Luna.Studio.Action.Searcher                    as Searcher
import           Luna.Studio.Action.UUID                        (isOwnRequest)
import qualified Luna.Studio.Batch.Workspace                    as Workspace
import           Luna.Studio.Event.Batch                        (Event (..))
import qualified Luna.Studio.Event.Event                        as Event
import           Luna.Studio.Handler.Backend.Common             (doNothing, handleResponse)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node                   as NodeModel
import qualified Luna.Studio.React.Model.NodeEditor             as NodeEditor
import           Luna.Studio.State.Global                       (State)
import qualified Luna.Studio.State.Global                       as Global
import qualified Luna.Studio.State.Graph                        as StateGraph


handle :: Event.Event -> Maybe (Command State ())
handle (Event.Batch ev) = Just $ case ev of
    GetProgramResponse response -> handleResponse response success doNothing where
        location       = response ^. Response.request . GetProgram.location
        success result = do
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

    AddNodeResponse response -> handleResponse response success failure where
        requestId    = response ^. Response.requestId
        request      = response ^. Response.request
        location     = request  ^. AddNode.location
        failure _    = whenM (isOwnRequest requestId) $ revertAddNode request
        success node = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $ do
                if ownRequest then do
                     localUpdateNode node
                     collaborativeModify [node ^. Node.nodeId]
                else localAddNode node

    AddPortResponse response -> handleResponse response success failure where
        requestId    = response ^. Response.requestId
        request      = response ^. Response.request
        location     = request  ^. AddPort.location
        portRef      = request  ^. AddPort.anyPortRef
        failure _    = whenM (isOwnRequest requestId) $ revertAddPort request
        success node = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $ do
                if ownRequest then do
                     localUpdateNode node
                     collaborativeModify [node ^. Node.nodeId]
                else do
                    --TODO[LJK, PM]: What should happen if localAddPort fails? (Example reason - node is not in graph)
                    void $ localAddPort portRef
                    localUpdateNode node

    AddSubgraphResponse response -> handleResponse response success failure where
        requestId     = response ^. Response.requestId
        request       = response ^. Response.request
        location      = request  ^. AddSubgraph.location
        conns         = request  ^. AddSubgraph.connections
        failure _     = whenM (isOwnRequest requestId) $ revertAddSubgraph request
        success nodes = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $ do
                if ownRequest then do
                    localUpdateNodes nodes
                    collaborativeModify $ flip map nodes $ view Node.nodeId
                else localAddSubgraph nodes conns

    CodeUpdate update -> do
       shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. CodeUpdate.location
       when shouldProcess $ updateCode $ update ^. CodeUpdate.code

    CollaborationUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. CollaborationUpdate.location
        let clientId = update ^. CollaborationUpdate.clientId
            touchNodes nodeIds setter = Global.modifyNodeEditor $
                forM_ nodeIds $ \nodeId -> NodeEditor.nodes . at nodeId %= fmap setter
        myClientId   <- use Global.clientId
        currentTime  <- use Global.lastEventTimestamp
        when (shouldProcess && clientId /= myClientId) $ do
            clientColor <- updateClient clientId
            case update ^. CollaborationUpdate.event of
                CollaborationUpdate.Touch       nodeIds -> touchNodes nodeIds $  NodeModel.collaboration . NodeModel.touch  . at clientId ?~ (DT.addSeconds (2 * refreshTime) currentTime, clientColor)
                CollaborationUpdate.Modify      nodeIds -> touchNodes nodeIds $ (NodeModel.collaboration . NodeModel.modify . at clientId ?~ DT.addSeconds modifyTime currentTime) . (NodeModel.collaboration . NodeModel.touch  . at clientId %~ bumpTime (DT.addSeconds modifyTime currentTime) clientColor)
                CollaborationUpdate.CancelTouch nodeIds -> touchNodes nodeIds $  NodeModel.collaboration . NodeModel.touch  . at clientId .~ Nothing
                CollaborationUpdate.Refresh             -> touchCurrentlySelected

    ConnectResponse response -> handleResponse response success failure where
        requestId          = response ^. Response.requestId
        request            = response ^. Response.request
        location           = request  ^. Connect.location
        failure _          = whenM (isOwnRequest requestId) $ revertConnect request
        success connection = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            when shouldProcess $ void $ localAddConnection connection

    ConnectUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. Connect.location'
        when shouldProcess $ void $ localAddConnection $ update ^. Connect.connection'

    DumpGraphVizResponse response -> handleResponse response doNothing doNothing

    MonadsUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. MonadsUpdate.location
        when shouldProcess $ updateMonads $ update ^. MonadsUpdate.monads

    MovePortResponse response -> handleResponse response success failure where
        requestId          = response ^. Response.requestId
        request            = response ^. Response.request
        location           = request  ^. MovePort.location
        portRef            = request  ^. MovePort.portRef
        newPortRef         = request  ^. MovePort.newPortRef
        failure _          = whenM (isOwnRequest requestId) $ revertMovePort request
        success node       = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    localUpdateNode node
                else void $ localMovePort portRef newPortRef

    NodeResultUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. NodeResultUpdate.location
        when shouldProcess $ do
            let nodeId = update ^. NodeResultUpdate.nodeId
            updateNodeValue           nodeId $ update ^. NodeResultUpdate.value
            updateNodeProfilingData   nodeId $ update ^. NodeResultUpdate.execTime
            updateConnectionsForNodes [nodeId]

    NodeSearchResponse response -> handleResponse response success doNothing where
        requestId      = response ^. Response.requestId
        location       = response ^. Response.request . NodeSearch.location
        success result = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when (ownRequest && shouldProcess) $
                updateHints $ result ^. NodeSearch.nodeSearcherData

    NodesUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. NodesUpdate.location
        when shouldProcess $ localUpdateNodes $ update ^. NodesUpdate.nodes

    NodeTypecheckerUpdate update -> do
      shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. NodeTCUpdate.location
      when shouldProcess $ typecheckNode $ update ^. NodeTCUpdate.node

    RedoResponse response -> $notImplemented

    RemoveConnectionResponse response -> handleResponse response success failure where
        requestId          = response ^. Response.requestId
        request            = response ^. Response.request
        location           = request  ^. RemoveConnection.location
        connId             = request  ^. RemoveConnection.connId
        failure inverse    = whenM (isOwnRequest requestId) $ revertRemoveConnection request inverse
        success _          = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    --TODO[LJK]: This is left to remind to set Confirmed flag in changes
                    return ()
                else void $ localRemoveConnection connId

    RemoveConnectionUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded  $ update ^. RemoveConnection.location'
        when shouldProcess $ void $ localRemoveConnection $ update ^. RemoveConnection.connId'

    RemoveNodesResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RemoveNodes.location
        nodeIds         = request  ^. RemoveNodes.nodeIds
        failure inverse = whenM (isOwnRequest requestId) $ revertRemoveNodes request inverse
        success _       = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    --TODO[LJK]: This is left to remind to set Confirmed flag in changes
                    return ()
                else void $ localRemoveNodes nodeIds

    --
    -- NodeMetaUpdated update -> do
    --     shouldProcess   <- isCurrentLocationAndGraphLoaded (update ^. UpdateNodeMeta.location')
    --     when shouldProcess $ do
    --         updateNodesMeta (update ^. UpdateNodeMeta.updates')
    --         updateConnectionsForNodes $ fst <$> (update ^. UpdateNodeMeta.updates')
    --
    -- NodeAdded update -> do
    --     shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. AddNode.location')
    --     when shouldProcess $ localAddNode (update ^. AddNode.node')
    --
    -- NodeRenamed update -> do
    --     shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. RenameNode.location')
    --     when shouldProcess $ Node.rename (update ^. RenameNode.nodeId') (update ^. RenameNode.name')
    --
    -- PortRenamed update -> do
    --     shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. RenamePort.location')
    --     when shouldProcess $ Edge.portRename (update ^. RenamePort.portRef') (update ^. RenamePort.name')
    --
    -- NodeCodeSet update -> do
    --     shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. SetCode.location')
    --     correctLocation <- isCurrentLocation (update ^. SetCode.location')
    --     when (shouldProcess && correctLocation) $ Node.setCode (update ^. SetCode.nodeId') (update ^. SetCode.code')
    --
    --
    -- NodeResultUpdated update -> do
    --     shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodeResultUpdate.location)
    --     when shouldProcess $ do
    --         updateNodeValue         (update ^. NodeResultUpdate.nodeId) (update ^. NodeResultUpdate.value)
    --         updateNodeProfilingData (update ^. NodeResultUpdate.nodeId) (update ^. NodeResultUpdate.execTime)
    --         updateConnectionsForNodes [update ^. NodeResultUpdate.nodeId]
    --
    -- RemovePortResponse response -> handleResponse response $ \request result -> do
    --     shouldProcess <- isCurrentLocationAndGraphLoaded (request ^. RemovePort.location)
    --     when shouldProcess $ do
    --         let portRef = request ^. RemovePort.anyPortRef
    --             nodeId  = portRef ^. PortRef.nodeId
    --             portId  = portRef ^. PortRef.portId
    --         localUpdateNode result
    --         graph <- use Global.graph
    --         localRemoveConnections $ map (view Connection.dst) $ StateGraph.connectionsContainingPort portRef graph
    --         let shouldUpdate = case portRef ^. PortRef.portId of
    --                 InPortId  (Arg _)        -> True
    --                 OutPortId (Projection _) -> True
    --                 _                        -> False
    --         when shouldUpdate $ do
    --             graph' <- use Global.graph
    --             let connectionsToUpdate = StateGraph.connectionsContainingNodes [nodeId] graph'
    --             forM_ connectionsToUpdate $ \conn -> do
    --                 let src = conn ^. Connection.src
    --                 let dst = conn ^. Connection.dst
    --                 if src ^. PortRef.srcNodeId == nodeId then case (src ^. PortRef.srcPortId, portId) of
    --                         (Projection num, OutPortId (Projection num')) -> when (num > num') $ do
    --                             Global.graph . StateGraph.connectionsMap . at dst ?=
    --                                 (conn & Connection.src . PortRef.srcPortId .~ Projection (num - 1))
    --                         _ -> return ()
    --                     else case (dst ^. PortRef.dstPortId, portId) of
    --                         (Arg num, InPortId (Arg num')) -> when (num > num') $ do
    --                             Global.graph . StateGraph.connectionsMap . at dst .= Nothing
    --                             let newConn = conn & Connection.src . PortRef.srcPortId .~ Projection (num - 1)
    --                             Global.graph . StateGraph.connectionsMap . at (newConn ^. Connection.dst) ?= newConn
    --                         _ -> return ()
    --             updateConnectionsForEdges
    --
    -- -- CollaborationUpdate update -> -- handled in Collaboration.hs
    -- AddPortResponse              response -> handleResponse response doNothing
    -- MovePortResponse             response -> handleResponse response doNothing
    -- ConnectResponse              response -> handleResponse response doNothing
    -- DisconnectResponse           response -> print response >> handleResponse response doNothing
    -- NodeMetaResponse             response -> handleResponse response doNothing
    -- NodeRenameResponse           response -> handleResponse response doNothing
    -- RemoveNodesResponse          response -> print response >> handleResponse response doNothing
    -- UpdateNodeExpressionResponse response -> handleResponse response doNothing

    _ -> return ()
handle _ = Nothing
