module Luna.Studio.Handler.Backend.Graph
    ( handle
    ) where

import qualified Data.DateTime                                as DT
import qualified Empire.API.Data.Breadcrumb                   as Breadcrumb
import qualified Empire.API.Data.Graph                        as Graph
import qualified Empire.API.Data.GraphLocation                as GraphLocation
import qualified Empire.API.Data.Node                         as Node
import qualified Empire.API.Graph.AddConnection               as AddConnection
import qualified Empire.API.Graph.AddNode                     as AddNode
import qualified Empire.API.Graph.AddPort                     as AddPort
import qualified Empire.API.Graph.AddSubgraph                 as AddSubgraph
import qualified Empire.API.Graph.CodeUpdate                  as CodeUpdate
import qualified Empire.API.Graph.CollaborationUpdate         as CollaborationUpdate
import qualified Empire.API.Graph.ConnectUpdate               as ConnectUpdate
import qualified Empire.API.Graph.GetProgram                  as GetProgram
import qualified Empire.API.Graph.GetSubgraphs                as GetSubgraphs
import qualified Empire.API.Graph.MonadsUpdate                as MonadsUpdate
import qualified Empire.API.Graph.MovePort                    as MovePort
import qualified Empire.API.Graph.NodeResultUpdate            as NodeResultUpdate
import qualified Empire.API.Graph.NodesUpdate                 as NodesUpdate
import qualified Empire.API.Graph.NodeTypecheckerUpdate       as NodeTCUpdate
import qualified Empire.API.Graph.RemoveConnection            as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes                 as RemoveNodes
import qualified Empire.API.Graph.RemovePort                  as RemovePort
import qualified Empire.API.Graph.RenameNode                  as RenameNode
import qualified Empire.API.Graph.RenamePort                  as RenamePort
import qualified Empire.API.Graph.SearchNodes                 as SearchNodes
import qualified Empire.API.Graph.SetNodeCode                 as SetNodeCode
import qualified Empire.API.Graph.SetNodeExpression           as SetNodeExpression
import qualified Empire.API.Graph.SetNodesMeta                as SetNodesMeta
import qualified Empire.API.Graph.SetPortDefault              as SetPortDefault
import qualified Empire.API.Response                          as Response
import           Luna.Studio.Action.Basic                     (createGraph, localAddConnection, localAddNode, localAddPort,
                                                               localAddSubgraph, localMerge, localMovePort, localRemoveConnection,
                                                               localRemoveNodes, localRemovePort, localRenameNode, localSetCode,
                                                               localSetNodeCode, localSetNodeExpression, localSetNodesMeta,
                                                               localSetPortDefault, localSetSearcherHints, localUpdateNode,
                                                               localUpdateNodeTypecheck, localUpdateNodes, setNodeProfilingData,
                                                               setNodeValue)
import           Luna.Studio.Action.Basic.Revert              (revertAddConnection, revertAddNode, revertAddPort, revertAddSubgraph,
                                                               revertMovePort, revertRemoveConnection, revertRemoveNodes, revertRemovePort,
                                                               revertRenameNode, revertSetNodeCode, revertSetNodeExpression,
                                                               revertSetNodesMeta, revertSetPortDefault)
import           Luna.Studio.Action.Basic.UpdateCollaboration (bumpTime, modifyTime, refreshTime, touchCurrentlySelected, updateClient)
import           Luna.Studio.Action.Batch                     (collaborativeModify, requestCollaborationRefresh)
import           Luna.Studio.Action.Camera                    (centerGraph)
import           Luna.Studio.Action.Command                   (Command)
import           Luna.Studio.Action.State.App                 (setBreadcrumbs)
import           Luna.Studio.Action.State.Graph               (isCurrentLocation, isCurrentLocationAndGraphLoaded)
import           Luna.Studio.Action.State.NodeEditor          (modifyNodeEditor, updateMonads)
import           Luna.Studio.Action.UUID                      (isOwnRequest)
import qualified Luna.Studio.Batch.Workspace                  as Workspace
import           Luna.Studio.Event.Batch                      (Event (..))
import qualified Luna.Studio.Event.Event                      as Event
import           Luna.Studio.Handler.Backend.Common           (doNothing, handleResponse)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node                 as NodeModel
import qualified Luna.Studio.React.Model.NodeEditor           as NodeEditor
import           Luna.Studio.State.Global                     (State)
import qualified Luna.Studio.State.Global                     as Global


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
                setBreadcrumbs breadcrumb
                createGraph nodes connections monads
                centerGraph
                localSetCode code
                Global.workspace . Workspace.isGraphLoaded .= True
                requestCollaborationRefresh

    AddConnectionResponse response -> handleResponse response success failure where
        requestId          = response ^. Response.requestId
        request            = response ^. Response.request
        location           = request  ^. AddConnection.location
        failure _          = whenM (isOwnRequest requestId) $ revertAddConnection request
        success connection = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            when shouldProcess $ void $ localAddConnection connection

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
                     void $ localUpdateNode node
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
                     void $ localUpdateNode node
                     collaborativeModify [node ^. Node.nodeId]
                else do
                    --TODO[LJK, PM]: What should happen if localAddPort fails? (Example reason - node is not in graph)
                    void $ localAddPort portRef
                    void $ localUpdateNode node

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
                else void $ localAddSubgraph nodes conns

    CodeUpdate update -> do
       shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. CodeUpdate.location
       when shouldProcess $ localSetCode $ update ^. CodeUpdate.code

    CollaborationUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. CollaborationUpdate.location
        let clientId = update ^. CollaborationUpdate.clientId
            touchNodes nodeIds setter = modifyNodeEditor $
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

    ConnectUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. ConnectUpdate.location'
        when shouldProcess $ void $ localAddConnection $ update ^. ConnectUpdate.connection'

    DumpGraphVizResponse response -> handleResponse response doNothing doNothing

    --TODO[LJK, PM]: Review this Handler
    GetSubgraphsResponse response -> handleResponse response success doNothing where
        request        = response ^. Response.request
        success result = do
            let parentId = request ^. GetSubgraphs.location
                                    . GraphLocation.breadcrumb
                                    . Breadcrumb.items . to last . Breadcrumb.nodeId
            localMerge parentId $ result ^. GetSubgraphs.graphs


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
                    void $ localUpdateNode node
                else void $ localMovePort portRef newPortRef

    NodeResultUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. NodeResultUpdate.location
        when shouldProcess $ do
            let nodeId = update ^. NodeResultUpdate.nodeId
            setNodeValue           nodeId $ update ^. NodeResultUpdate.value
            setNodeProfilingData   nodeId $ update ^. NodeResultUpdate.execTime

    NodesUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. NodesUpdate.location
        when shouldProcess $ localUpdateNodes $ update ^. NodesUpdate.nodes

    NodeTypecheckerUpdate update -> do
      shouldProcess <- isCurrentLocationAndGraphLoaded $ update ^. NodeTCUpdate.location
      when shouldProcess $ void . localUpdateNodeTypecheck $ update ^. NodeTCUpdate.node

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

    RemovePortResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RemovePort.location
        portRef         = request  ^. RemovePort.portRef
        failure inverse = whenM (isOwnRequest requestId) $ revertRemovePort request inverse
        success _       = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    --TODO[LJK]: This is left to remind to set Confirmed flag in changes
                    return ()
                else void $ localRemovePort portRef

    RenameNodeResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RenameNode.location
        nodeId          = request  ^. RenameNode.nodeId
        name            = request  ^. RenameNode.name
        failure inverse = whenM (isOwnRequest requestId) $ revertRenameNode request inverse
        success _       = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    --TODO[LJK]: This is left to remind to set Confirmed flag in changes
                    return ()
                else void $ localRenameNode nodeId name

    RenamePortResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RenamePort.location
        portRef         = request  ^. RenamePort.portRef
        name            = request  ^. RenamePort.name
        failure inverse = whenM (isOwnRequest requestId) $ $notImplemented
        success _       = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    --TODO[LJK]: This is left to remind to set Confirmed flag in changes
                    return ()
                else void $ $notImplemented


    SearchNodesResponse response -> handleResponse response success doNothing where
        requestId      = response ^. Response.requestId
        location       = response ^. Response.request . SearchNodes.location
        success result = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when (ownRequest && shouldProcess) $
                localSetSearcherHints $ result ^. SearchNodes.nodeSearcherData

    SetNodeCodeResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetNodeCode.location
        nodeId          = request  ^. SetNodeCode.nodeId
        code            = request  ^. SetNodeCode.newCode
        failure inverse = whenM (isOwnRequest requestId) $ revertSetNodeCode request inverse
        success _       = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    return ()
                else void $ localSetNodeCode nodeId code

    SetNodeExpressionResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetNodeExpression.location
        nodeId          = request  ^. SetNodeExpression.nodeId
        expression      = request  ^. SetNodeExpression.expression
        failure inverse = whenM (isOwnRequest requestId) $ revertSetNodeExpression request inverse
        success _       = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    return ()
                else void $ localSetNodeExpression nodeId expression

    SetNodesMetaResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetNodesMeta.location
        updates         = request  ^. SetNodesMeta.updates
        failure inverse = whenM (isOwnRequest requestId) $ revertSetNodesMeta request inverse
        success _       = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    return ()
                else void $ localSetNodesMeta updates

    SetPortDefaultResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetPortDefault.location
        portRef         = request  ^. SetPortDefault.portRef
        defaultVal      = request  ^. SetPortDefault.defaultValue
        failure inverse = whenM (isOwnRequest requestId) $ revertSetPortDefault request inverse
        success _       = do
            shouldProcess <- isCurrentLocationAndGraphLoaded location
            ownRequest    <- isOwnRequest requestId
            when shouldProcess $
                if ownRequest then
                    return ()
                else void $ localSetPortDefault portRef defaultVal

    TypeCheckResponse response -> handleResponse response doNothing doNothing

    UndoResponse response -> $notImplemented

    _ -> return ()
handle _ = Nothing
