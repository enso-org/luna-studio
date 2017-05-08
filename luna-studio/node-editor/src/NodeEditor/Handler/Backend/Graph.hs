module NodeEditor.Handler.Backend.Graph
    ( handle
    ) where

import           Common.Prelude
import           Common.Report
import           Control.Arrow                               ((&&&))
import qualified Data.DateTime                               as DT
import           Empire.API.Data.Connection                  (dst, src)
import qualified Empire.API.Data.Graph                       as Graph
import           Empire.API.Data.Node                        (Node (ExpressionNode', InputSidebar', OutputSidebar'), nodeId)
import           Empire.API.Data.NodeLoc                     (NodePath, nodeLoc, prependPath)
import qualified Empire.API.Data.NodeLoc                     as NodeLoc
import           Empire.API.Data.PortRef                     (AnyPortRef (InPortRef', OutPortRef'))
import qualified Empire.API.Graph.AddConnection              as AddConnection
import qualified Empire.API.Graph.AddNode                    as AddNode
import qualified Empire.API.Graph.AddPort                    as AddPort
import qualified Empire.API.Graph.AddSubgraph                as AddSubgraph
import qualified Empire.API.Graph.AutolayoutNodes            as AutolayoutNodes
import qualified Empire.API.Graph.CollaborationUpdate        as CollaborationUpdate
import qualified Empire.API.Graph.GetProgram                 as GetProgram
import qualified Empire.API.Graph.GetSubgraphs               as GetSubgraphs
import qualified Empire.API.Graph.MonadsUpdate               as MonadsUpdate
import qualified Empire.API.Graph.MovePort                   as MovePort
import qualified Empire.API.Graph.NodeResultUpdate           as NodeResultUpdate
import qualified Empire.API.Graph.NodeTypecheckerUpdate      as NodeTCUpdate
import qualified Empire.API.Graph.RemoveConnection           as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes                as RemoveNodes
import qualified Empire.API.Graph.RemovePort                 as RemovePort
import qualified Empire.API.Graph.RenameNode                 as RenameNode
import qualified Empire.API.Graph.RenamePort                 as RenamePort
import qualified Empire.API.Graph.Result                     as Result
import qualified Empire.API.Graph.SearchNodes                as SearchNodes
import qualified Empire.API.Graph.SetNodeExpression          as SetNodeExpression
import qualified Empire.API.Graph.SetNodesMeta               as SetNodesMeta
import qualified Empire.API.Graph.SetPortDefault             as SetPortDefault
import qualified Empire.API.Response                         as Response
import           NodeEditor.Action.Basic                     (localAddConnection, localAddConnections, localAddExpressionNode, localAddPort,
                                                              localMerge, localMoveNodes, localMovePort, localRemoveConnection,
                                                              localRemoveConnections, localRemoveNodes, localRemovePort, localSetNodesMeta,
                                                              localSetPortDefault, localSetSearcherHints, localUpdateExpressionNode,
                                                              localUpdateExpressionNodes, localUpdateInputNode, localUpdateNodeTypecheck,
                                                              localUpdateOrAddExpressionNode, localUpdateOrAddInputNode,
                                                              localUpdateOrAddOutputNode, setNodeProfilingData, setNodeValue, updateGraph,
                                                              updateScene)
import           NodeEditor.Action.Basic.Revert              (revertAddConnection, revertAddNode, revertAddPort, revertAddSubgraph,
                                                              revertMovePort, revertRemoveConnection, revertRemoveNodes, revertRemovePort,
                                                              revertRenameNode, revertSetNodeExpression, revertSetNodesMeta,
                                                              revertSetPortDefault)
import           NodeEditor.Action.Basic.UpdateCollaboration (bumpTime, modifyTime, refreshTime, touchCurrentlySelected, updateClient)
import           NodeEditor.Action.Batch                     (collaborativeModify, getProgram, requestCollaborationRefresh)
import           NodeEditor.Action.Camera                    (centerGraph)
import           NodeEditor.Action.Command                   (Command)
import           NodeEditor.Action.State.App                 (setBreadcrumbs)
import           NodeEditor.Action.State.Graph               (inCurrentLocation, isCurrentLocation)
import           NodeEditor.Action.State.NodeEditor          (isGraphLoaded, modifyExpressionNode, setGraphLoaded, updateMonads)
import           NodeEditor.Action.UUID                      (isOwnRequest)
import qualified NodeEditor.Batch.Workspace                  as Workspace
import           NodeEditor.Event.Batch                      (Event (..))
import qualified NodeEditor.Event.Event                      as Event
import           NodeEditor.Handler.Backend.Common           (doNothing, handleResponse)
import qualified NodeEditor.React.Model.Node.ExpressionNode  as Node
import           NodeEditor.State.Global                     (State)
import qualified NodeEditor.State.Global                     as Global


applyResult :: NodePath -> Result.Result -> Command State ()
applyResult path res = do
    void $ localRemoveNodes       . map (convert . (path,)) $ res ^. Result.removedNodes
    void $ localRemoveConnections . map (prependPath path)  $ res ^. Result.removedConnections
    mapM_ (localUpdateOrAddExpressionNode . convert . (path,)) $ res ^. Result.graphUpdates . Graph.nodes
    void $ localAddConnections . map (\(src, dst) -> (prependPath path src, prependPath path dst)) $ res ^. Result.graphUpdates . Graph.connections
    let inputSidebar  = res ^. Result.graphUpdates . Graph.inputSidebar
        outputSidebar = res ^. Result.graphUpdates . Graph.outputSidebar
    when (isJust inputSidebar)  $ forM_ inputSidebar  $ localUpdateOrAddInputNode  . convert . (path,)
    when (isJust outputSidebar) $ forM_ outputSidebar $ localUpdateOrAddOutputNode . convert . (path,)

handle :: Event.Event -> Maybe (Command State ())
handle (Event.Batch ev) = Just $ case ev of
    GetProgramResponse response -> handleResponse response success failure where
        location       = response ^. Response.request . GetProgram.location
        success result = do
            whenM (isCurrentLocation location) $ do
                putStrLn "GetProgram"
                let nodes       = convert . (NodeLoc.empty,) <$> result ^. GetProgram.graph . Graph.nodes
                    input       = convert . (NodeLoc.empty,) <$> result ^. GetProgram.graph . Graph.inputSidebar
                    output      = convert . (NodeLoc.empty,) <$> result ^. GetProgram.graph . Graph.outputSidebar
                    connections = result ^. GetProgram.graph . Graph.connections
                    monads      = result ^. GetProgram.graph . Graph.monads
                    code        = result ^. GetProgram.code
                    nsData      = result ^. GetProgram.nodeSearcherData
                    breadcrumb  = result ^. GetProgram.breadcrumb
                Global.workspace . _Just . Workspace.nodeSearcherData .= nsData
                setBreadcrumbs breadcrumb
                updateGraph nodes input output connections monads
                unlessM isGraphLoaded $ do
                    centerGraph
                    requestCollaborationRefresh
                setGraphLoaded True
                updateScene
        failure _ = do
            isOnTop <- fromMaybe True <$> preuses (Global.workspace . traverse) Workspace.isOnTopBreadcrumb
            if isOnTop
                then fatal "Cannot get file from backend"
                else do
                    Global.workspace . _Just %= Workspace.upperWorkspace
                    getProgram

    AddConnectionResponse response -> handleResponse response success failure where
        requestId          = response ^. Response.requestId
        request            = response ^. Response.request
        location           = request  ^. AddConnection.location
        failure _          = whenM (isOwnRequest requestId) $ revertAddConnection request
        success result = inCurrentLocation location $ \path -> do
            case result ^. AddConnection.srcNode of
                ExpressionNode' n -> localUpdateOrAddExpressionNode $ convert (path, n)
                InputSidebar'   n -> localUpdateOrAddInputNode      $ convert (path, n)
                OutputSidebar'  n -> localUpdateOrAddOutputNode     $ convert (path, n) -- this should not happen
            case result ^. AddConnection.dstNode of
                ExpressionNode' n -> localUpdateOrAddExpressionNode $ convert (path, n)
                OutputSidebar'  n -> localUpdateOrAddOutputNode     $ convert (path, n)
                InputSidebar'   n -> localUpdateOrAddInputNode      $ convert (path, n) -- this should not happen
            void $ localAddConnection (prependPath path (result ^. AddConnection.connection . src)) (prependPath path (result ^. AddConnection.connection . dst))

    AddNodeResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddNode.location
        failure _      = whenM (isOwnRequest requestId) $ revertAddNode request
        success result = inCurrentLocation location $ \path -> do
            let node = convert (path, result ^. AddNode.node)
            ownRequest <- isOwnRequest requestId
            if ownRequest then do
                 void $ localUpdateExpressionNode node
                 collaborativeModify [node ^. nodeLoc]
            else localAddExpressionNode node
            withJust (result ^. AddNode.connectedNode) $ \case
                ExpressionNode' n -> localUpdateOrAddExpressionNode $ convert (path, n)
                InputSidebar'   n -> localUpdateOrAddInputNode      $ convert (path, n) -- this may happen but no reason why
                OutputSidebar'  n -> localUpdateOrAddOutputNode     $ convert (path, n) -- this should not happen
            void . localAddConnections . map convert $ result ^. AddNode.newConns

    AddPortResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddPort.location
        portRef        = request  ^. AddPort.outPortRef
        failure _      = whenM (isOwnRequest requestId) $ revertAddPort request
        success result = inCurrentLocation location $ \path -> do
            let sidebar = convert (path, result ^. AddPort.sidebar)
            ownRequest <- isOwnRequest requestId
            if ownRequest then do
                 void $ localUpdateOrAddInputNode sidebar
            else do
                --TODO[LJK, PM]: What should happen if localAddPort fails? (Example reason - node is not in graph)
                void $ localAddPort (prependPath path portRef) Nothing
                void $ localAddConnections . catMaybes $ flip map (request ^. AddPort.connectTo) $ \case
                    OutPortRef' _   -> $notImplemented
                    InPortRef'  dst -> Just (portRef, dst)
                void $ localUpdateOrAddInputNode sidebar
            mapM_ (localUpdateOrAddExpressionNode . convert) $ map (path,) $ result ^. AddPort.dstNodes

    AddSubgraphResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddSubgraph.location
        conns          = request  ^. AddSubgraph.connections
        failure _      = whenM (isOwnRequest requestId) $ revertAddSubgraph request
        success result = inCurrentLocation location $ \path -> do
            forM_ (result ^. AddSubgraph.newAndUpdatedNodes) $ \case
                ExpressionNode' n -> localUpdateOrAddExpressionNode $ convert (path, n)
                InputSidebar'   n -> localUpdateOrAddInputNode      $ convert (path, n)
                OutputSidebar'  n -> localUpdateOrAddOutputNode     $ convert (path, n)
            void . localAddConnections . map (prependPath path . view src &&& prependPath path . view dst) $ result ^. AddSubgraph.newAndUpdatedConnections
            whenM (isOwnRequest requestId) $ collaborativeModify $ map (convert . (path,) . view nodeId) $ result ^. AddSubgraph.newAndUpdatedNodes

    AutolayoutNodesResponse response -> handleResponse response success doNothing where
        location       = response ^. Response.request . AutolayoutNodes.location
        success result = inCurrentLocation location $ \_ -> void $ localMoveNodes result

    CollaborationUpdate update -> inCurrentLocation (update ^. CollaborationUpdate.location) $ \path -> do
        let clientId = update ^. CollaborationUpdate.clientId
            touchNodes nodeLocs setter = forM_ nodeLocs $ \nl ->
                modifyExpressionNode (prependPath path nl) setter
        myClientId  <- use $ Global.backend . Global.clientId
        currentTime <- use Global.lastEventTimestamp
        when (clientId /= myClientId) $ do
            clientColor <- updateClient clientId
            case update ^. CollaborationUpdate.event of
                CollaborationUpdate.Touch       nodeLocs -> touchNodes nodeLocs $  Node.collaboration . Node.touch  . at clientId ?= (DT.addSeconds (2 * refreshTime) currentTime, clientColor)
                CollaborationUpdate.Modify      nodeLocs -> touchNodes nodeLocs $ do
                    Node.collaboration . Node.touch  . at clientId %= bumpTime (DT.addSeconds modifyTime currentTime) clientColor
                    Node.collaboration . Node.modify . at clientId ?= DT.addSeconds modifyTime currentTime
                CollaborationUpdate.CancelTouch nodeLocs -> touchNodes nodeLocs $  Node.collaboration . Node.touch  . at clientId .= Nothing
                CollaborationUpdate.Refresh             -> touchCurrentlySelected

    DumpGraphVizResponse response -> handleResponse response doNothing doNothing

    --TODO[LJK, PM]: Review this Handler
    GetSubgraphsResponse response -> handleResponse response success doNothing where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request ^. GetSubgraphs.location
        success result = inCurrentLocation location $ \path ->
            whenM (isOwnRequest requestId) $
                localMerge path $ result ^. GetSubgraphs.graphs

    MonadsUpdate update -> do
        inCurrentLocation (update ^. MonadsUpdate.location) $ \path ->
            updateMonads $ update ^. MonadsUpdate.monads --FIXME updateMonads in path!

    MovePortResponse response -> handleResponse response success failure where
        requestId     = response ^. Response.requestId
        request       = response ^. Response.request
        location      = request  ^. MovePort.location
        portRef       = request  ^. MovePort.portRef
        newPos        = request  ^. MovePort.newPortPos
        failure _     = whenM (isOwnRequest requestId) $ revertMovePort request
        success node' = inCurrentLocation location $ \path -> do
            let node = convert (path, node')
            ownRequest <- isOwnRequest requestId
            if ownRequest then
                void $ localUpdateInputNode node
            else void $ localMovePort portRef newPos >> localUpdateInputNode node

    NodeResultUpdate update -> do
        let location = update ^. NodeResultUpdate.location
        inCurrentLocation location $ \path -> do
            let nid = update ^. NodeResultUpdate.nodeId
            setNodeValue         (convert (path, nid)) $ update ^. NodeResultUpdate.value
            setNodeProfilingData (convert (path, nid)) $ update ^. NodeResultUpdate.execTime

    NodeTypecheckerUpdate update -> do
      inCurrentLocation (update ^. NodeTCUpdate.location) $ \path ->
          void $ localUpdateNodeTypecheck path $ update ^. NodeTCUpdate.node

    RedoResponse response -> $notImplemented

    RemoveConnectionResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RemoveConnection.location
        connId          = request  ^. RemoveConnection.connId
        failure inverse = whenM (isOwnRequest requestId) $ revertRemoveConnection request inverse
        success result  = inCurrentLocation location $ \path -> do
            case result ^. RemoveConnection.dstNode of
                ExpressionNode' n -> localUpdateOrAddExpressionNode $ convert (path, n)
                InputSidebar'   n -> localUpdateOrAddInputNode      $ convert (path, n)
                OutputSidebar'  n -> localUpdateOrAddOutputNode     $ convert (path, n)
            ownRequest <- isOwnRequest requestId
            if ownRequest then
                --TODO[LJK]: This is left to remind to set Confirmed flag in changes
                return ()
            else void $ localRemoveConnection $ prependPath path connId
            void . localAddConnections . map (prependPath path . view src &&& prependPath path . view dst) $ result ^. RemoveConnection.newConns

    RemoveConnectionUpdate update -> do
        inCurrentLocation (update ^. RemoveConnection.location') $ \path ->
            void $ localRemoveConnection $ prependPath path $ update ^. RemoveConnection.connId'

    RemoveNodesResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RemoveNodes.location
        nodeLocs        = request  ^. RemoveNodes.nodeLocs
        failure inverse = whenM (isOwnRequest requestId) $ revertRemoveNodes request inverse
        success _       = inCurrentLocation location $ \path -> do
            ownRequest <- isOwnRequest requestId
            if ownRequest then
                --TODO[LJK]: This is left to remind to set Confirmed flag in changes
                return ()
            else void $ localRemoveNodes $ prependPath path <$> nodeLocs

    RemovePortResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RemovePort.location
        portRef         = request  ^. RemovePort.portRef
        failure inverse = whenM (isOwnRequest requestId) $ revertRemovePort request inverse
        success result  = inCurrentLocation location $ \path -> do
            ownRequest <- isOwnRequest requestId
            if ownRequest then
                --TODO[LJK]: This is left to remind to set Confirmed flag in changes
                return ()
            else void $ localRemovePort $ prependPath path portRef
            localUpdateOrAddInputNode $ convert (path, result)

    RenameNodeResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RenameNode.location
        nid             = request  ^. RenameNode.nodeId
        name            = request  ^. RenameNode.name
        failure inverse = whenM (isOwnRequest requestId) $ revertRenameNode request inverse
        success nodes   = inCurrentLocation location $ \path ->
            forM_ nodes $ localUpdateOrAddExpressionNode . convert . (path,)

    RenamePortResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RenamePort.location
        portRef         = request  ^. RenamePort.portRef
        name            = request  ^. RenamePort.name
        failure inverse = whenM (isOwnRequest requestId) $ $notImplemented
        success result  = inCurrentLocation location $ \path -> do
            localUpdateOrAddInputNode $ convert (path, result ^. RenamePort.sidebar)
            forM_ (result ^. RenamePort.updatedNodes) $ localUpdateOrAddExpressionNode . convert . (path,)


    SearchNodesResponse response -> handleResponse response success doNothing where
        requestId      = response ^. Response.requestId
        location       = response ^. Response.request . SearchNodes.location
        success result = inCurrentLocation location $ \path -> do
            ownRequest <- isOwnRequest requestId
            when ownRequest $
                localSetSearcherHints $ result ^. SearchNodes.nodeSearcherData

    SetNodeExpressionResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetNodeExpression.location
        nid             = request  ^. SetNodeExpression.nodeId
        expression      = request  ^. SetNodeExpression.expression
        failure inverse = whenM (isOwnRequest requestId) $ revertSetNodeExpression request inverse
        success result  = inCurrentLocation location $ \path -> applyResult path result


    SetNodesMetaResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetNodesMeta.location
        updates         = request  ^. SetNodesMeta.updates
        failure inverse = whenM (isOwnRequest requestId) $ revertSetNodesMeta request inverse
        success result  = whenM (not <$> isOwnRequest requestId) $ inCurrentLocation location $ \path -> applyResult path result

    SetPortDefaultResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetPortDefault.location
        portRef         = request  ^. SetPortDefault.portRef
        defaultVal      = request  ^. SetPortDefault.defaultValue
        failure inverse = whenM (isOwnRequest requestId) $ revertSetPortDefault request inverse
        success result  = inCurrentLocation location $ \path -> applyResult path result

    TypeCheckResponse response -> handleResponse response doNothing doNothing

    UndoResponse response -> $notImplemented

    _ -> return ()
handle _ = Nothing
