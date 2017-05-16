module NodeEditor.Handler.Backend.Graph
    ( handle
    ) where

import           Common.Prelude
import           Common.Report
import qualified Data.DateTime                               as DT
import qualified LunaStudio.API.Graph.AddConnection          as AddConnection
import qualified LunaStudio.API.Graph.AddNode                as AddNode
import qualified LunaStudio.API.Graph.AddPort                as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph            as AddSubgraph
import qualified LunaStudio.API.Graph.AutolayoutNodes        as AutolayoutNodes
import qualified LunaStudio.API.Graph.CollaborationUpdate    as CollaborationUpdate
import qualified LunaStudio.API.Graph.GetProgram             as GetProgram
import qualified LunaStudio.API.Graph.GetSubgraphs           as GetSubgraphs
import qualified LunaStudio.API.Graph.MonadsUpdate           as MonadsUpdate
import qualified LunaStudio.API.Graph.MovePort               as MovePort
import qualified LunaStudio.API.Graph.NodeResultUpdate       as NodeResultUpdate
import qualified LunaStudio.API.Graph.NodeTypecheckerUpdate  as NodeTCUpdate
import qualified LunaStudio.API.Graph.RemoveConnection       as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes            as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort             as RemovePort
import qualified LunaStudio.API.Graph.RenameNode             as RenameNode
import qualified LunaStudio.API.Graph.RenamePort             as RenamePort
import qualified LunaStudio.API.Graph.Result                 as Result
import qualified LunaStudio.API.Graph.SearchNodes            as SearchNodes
import qualified LunaStudio.API.Graph.SetNodeExpression      as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta           as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault         as SetPortDefault
import qualified LunaStudio.API.Response                     as Response
import qualified LunaStudio.Data.Graph                       as Graph
import           LunaStudio.Data.Node                        (nodeId)
import           LunaStudio.Data.NodeLoc                     (NodePath, prependPath)
import qualified LunaStudio.Data.NodeLoc                     as NodeLoc
import           NodeEditor.Action.Basic                     (localAddConnections, localMerge, localRemoveConnections, localRemoveNodes,
                                                              localSetSearcherHints, localUpdateNodeTypecheck,
                                                              localUpdateOrAddExpressionNode, localUpdateOrAddExpressionNodePreventingPorts,
                                                              localUpdateOrAddInputNode, localUpdateOrAddOutputNode, setNodeProfilingData,
                                                              setNodeValue, updateGraph, updateScene)
import           NodeEditor.Action.Basic.Revert              (revertAddConnection, revertAddNode, revertAddPort, revertAddSubgraph,
                                                              revertMovePort, revertRemoveConnection, revertRemoveNodes, revertRemovePort,
                                                              revertRenameNode, revertSetNodeExpression, revertSetNodesMeta,
                                                              revertSetPortDefault)
import           NodeEditor.Action.Basic.UpdateCollaboration (bumpTime, modifyTime, refreshTime, touchCurrentlySelected, updateClient)
import           NodeEditor.Action.Batch                     (collaborativeModify, getProgram, requestCollaborationRefresh)
import           NodeEditor.Action.Camera                    (tryLoadCamera)
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


applyResult :: Result.Result -> NodePath -> Command State ()
applyResult = applyResult' False

applyResultPreventingExpressionNodesPorts :: Result.Result -> NodePath -> Command State ()
applyResultPreventingExpressionNodesPorts = applyResult' True

applyResult' :: Bool -> Result.Result -> NodePath -> Command State ()
applyResult' preventPorts res path = do
    let exprNodeUpdateFunction = if preventPorts then localUpdateOrAddExpressionNodePreventingPorts else localUpdateOrAddExpressionNode
    void $ localRemoveNodes       . map (convert . (path,)) $ res ^. Result.removedNodes
    void $ localRemoveConnections . map (prependPath path)  $ res ^. Result.removedConnections
    mapM_ (exprNodeUpdateFunction . convert . (path,)) $ res ^. Result.graphUpdates . Graph.nodes
    void $ localAddConnections . map (\(src', dst') -> (prependPath path src', prependPath path dst')) $ res ^. Result.graphUpdates . Graph.connections
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
                    nsData      = result ^. GetProgram.nodeSearcherData
                    breadcrumb  = result ^. GetProgram.breadcrumb
                shouldCenter <- not <$> isGraphLoaded
                Global.workspace . _Just . Workspace.nodeSearcherData .= nsData
                setBreadcrumbs breadcrumb
                updateGraph nodes input output connections monads
                setGraphLoaded True
                updateScene
                when shouldCenter $ do
                    tryLoadCamera
                    requestCollaborationRefresh
        failure _ = do
            isOnTop <- fromMaybe True <$> preuses (Global.workspace . traverse) Workspace.isOnTopBreadcrumb
            if isOnTop
                then fatal "Cannot get file from backend"
                else do
                    Global.workspace . _Just %= Workspace.upperWorkspace
                    getProgram

    AddConnectionResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddConnection.location
        failure _      = whenM (isOwnRequest requestId) $ revertAddConnection request
        success result = inCurrentLocation location $ applyResult result

    AddNodeResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddNode.location
        nl             = request  ^. AddNode.nodeLoc
        failure _      = whenM (isOwnRequest requestId) $ revertAddNode request
        success result = inCurrentLocation location $ \path -> do
            applyResult result path
            whenM (isOwnRequest requestId) $ collaborativeModify [nl]

    AddPortResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddPort.location
        failure _      = whenM (isOwnRequest requestId) $ revertAddPort request
        success result = inCurrentLocation location $ applyResult result

    AddSubgraphResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddSubgraph.location
        failure _      = whenM (isOwnRequest requestId) $ revertAddSubgraph request
        success result = inCurrentLocation location $ \path -> do
            applyResult result path
            whenM (isOwnRequest requestId) $
                collaborativeModify $ map (convert . (path,) . view nodeId) $ result ^. Result.graphUpdates . Graph.nodes

    AutolayoutNodesResponse response -> handleResponse response success doNothing where
        location       = response ^. Response.request . AutolayoutNodes.location
        success result = inCurrentLocation location $ applyResult result

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
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. MovePort.location
        failure _      = whenM (isOwnRequest requestId) $ revertMovePort request
        success result = inCurrentLocation location $ applyResult result

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
        failure inverse = whenM (isOwnRequest requestId) $ revertRemoveConnection request inverse
        success result  = inCurrentLocation location $ applyResult result

    RemoveNodesResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RemoveNodes.location
        failure inverse = whenM (isOwnRequest requestId) $ revertRemoveNodes request inverse
        success result  = inCurrentLocation location $ applyResult result

    RemovePortResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RemovePort.location
        failure inverse = whenM (isOwnRequest requestId) $ revertRemovePort request inverse
        success result  = inCurrentLocation location $ applyResult result

    RenameNodeResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RenameNode.location
        failure inverse = whenM (isOwnRequest requestId) $ revertRenameNode request inverse
        success result  = inCurrentLocation location $ applyResult result

    RenamePortResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. RenamePort.location
        failure inverse = whenM (isOwnRequest requestId) $ $notImplemented
        success result  = inCurrentLocation location $ applyResult result

    SearchNodesResponse response -> handleResponse response success doNothing where
        requestId      = response ^. Response.requestId
        location       = response ^. Response.request . SearchNodes.location
        success result = inCurrentLocation location $ \_ -> do
            ownRequest <- isOwnRequest requestId
            when ownRequest $
                localSetSearcherHints $ result ^. SearchNodes.nodeSearcherData

    SetNodeExpressionResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetNodeExpression.location
        failure inverse = whenM (isOwnRequest requestId) $ revertSetNodeExpression request inverse
        success result  = inCurrentLocation location $ applyResult result


    SetNodesMetaResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetNodesMeta.location
        failure inverse = whenM (isOwnRequest requestId) $ revertSetNodesMeta request inverse
        success result  = whenM (not <$> isOwnRequest requestId) $ inCurrentLocation location $ applyResult result

    SetPortDefaultResponse response -> handleResponse response success failure where
        requestId       = response ^. Response.requestId
        request         = response ^. Response.request
        location        = request  ^. SetPortDefault.location
        failure inverse = whenM (isOwnRequest requestId) $ revertSetPortDefault request inverse
        success result  = inCurrentLocation location $ applyResultPreventingExpressionNodesPorts result

    TypeCheckResponse response -> handleResponse response doNothing doNothing

    UndoResponse response -> $notImplemented

    _ -> return ()
handle _ = Nothing
