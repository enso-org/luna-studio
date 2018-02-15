module NodeEditor.Handler.Backend.Graph
    ( handle
    ) where

import           Common.Action.Command                       (Command)
import           Common.Prelude
import           Common.Report                               (fatal)
import qualified Data.DateTime                               as DT
import qualified JS.Atom                                     as Atom
import qualified JS.Clipboard                                as JS
import qualified LunaStudio.API.Atom.MoveProject             as MoveProject
import qualified LunaStudio.API.Atom.Paste                   as AtomPaste
import qualified LunaStudio.API.Atom.Substitute              as Substitute
import qualified LunaStudio.API.Graph.AddConnection          as AddConnection
import qualified LunaStudio.API.Graph.AddImports             as AddImports
import qualified LunaStudio.API.Graph.AddNode                as AddNode
import qualified LunaStudio.API.Graph.AddPort                as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph            as AddSubgraph
import qualified LunaStudio.API.Graph.AutolayoutNodes        as AutolayoutNodes
import qualified LunaStudio.API.Graph.CollaborationUpdate    as CollaborationUpdate
import qualified LunaStudio.API.Graph.CollapseToFunction     as CollapseToFunction
import qualified LunaStudio.API.Graph.Copy                   as Copy
import qualified LunaStudio.API.Graph.GetProgram             as GetProgram
import qualified LunaStudio.API.Graph.GetSubgraphs           as GetSubgraphs
import qualified LunaStudio.API.Graph.MonadsUpdate           as MonadsUpdate
import qualified LunaStudio.API.Graph.MovePort               as MovePort
import qualified LunaStudio.API.Graph.NodeResultUpdate       as NodeResultUpdate
import qualified LunaStudio.API.Graph.NodeTypecheckerUpdate  as NodeTCUpdate
import qualified LunaStudio.API.Graph.Paste                  as Paste
import qualified LunaStudio.API.Graph.RemoveConnection       as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes            as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort             as RemovePort
import qualified LunaStudio.API.Graph.RenameNode             as RenameNode
import qualified LunaStudio.API.Graph.RenamePort             as RenamePort
import qualified LunaStudio.API.Graph.Result                 as Result
import qualified LunaStudio.API.Graph.SearchNodes            as SearchNodes
import qualified LunaStudio.API.Graph.SetCode                as SetCode
import qualified LunaStudio.API.Graph.SetNodeExpression      as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta           as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault         as SetPortDefault
import qualified LunaStudio.API.Response                     as Response
import           LunaStudio.Data.Breadcrumb                  (containsNode)
import           LunaStudio.Data.Error                       (Error (Error), GraphError (BreadcrumbDoesNotExist, OtherGraphError),
                                                              LunaError, errorType)
import qualified LunaStudio.Data.Error                       as ErrorAPI
import qualified LunaStudio.Data.Graph                       as Graph
import           LunaStudio.Data.GraphLocation               (GraphLocation)
import qualified LunaStudio.Data.GraphLocation               as GraphLocation
import           LunaStudio.Data.Node                        (nodeId)
import           LunaStudio.Data.NodeLoc                     (NodePath, prependPath)
import qualified LunaStudio.Data.NodeLoc                     as NodeLoc
import qualified LunaStudio.Data.NodeSearcher                as NS
import           NodeEditor.Action.Basic                     (centerGraph, exitBreadcrumb, localAddConnections, localAddSearcherHints,
                                                              localMerge, localMoveProject, localRemoveConnections, localRemoveNodes,
                                                              localUpdateNodeTypecheck, localUpdateOrAddExpressionNode,
                                                              localUpdateOrAddExpressionNodePreventingPorts, localUpdateOrAddInputNode,
                                                              localUpdateOrAddOutputNode, setCurrentImports, setNodeProfilingData,
                                                              updateGraph, updateNodeValueAndVisualization, updateScene)
import           NodeEditor.Action.Basic.Revert              (revertAddConnection, revertAddNode, revertAddPort, revertAddSubgraph,
                                                              revertMovePort, revertRemoveConnection, revertRemoveNodes, revertRemovePort,
                                                              revertRenameNode, revertRenamePort, revertSetNodeExpression,
                                                              revertSetNodesMeta, revertSetPortDefault)
import           NodeEditor.Action.Basic.UpdateCollaboration (bumpTime, modifyTime, refreshTime, touchCurrentlySelected, updateClient)
import           NodeEditor.Action.Batch                     (collaborativeModify, getProgram)
import           NodeEditor.Action.State.App                 (getWorkspace, modifyApp, setBreadcrumbs)
import           NodeEditor.Action.State.Graph               (inCurrentLocation, isCurrentLocation)
import           NodeEditor.Action.State.NodeEditor          (modifyExpressionNode, setGraphStatus, setScreenTransform, updateMonads)
import           NodeEditor.Action.UUID                      (isOwnRequest)
import qualified NodeEditor.Batch.Workspace                  as Workspace
import           NodeEditor.Event.Batch                      (Event (..))
import qualified NodeEditor.Event.Event                      as Event
import           NodeEditor.Handler.Backend.Common           (doNothing, doNothing2, handleResponse)
import           NodeEditor.React.Model.App                  (workspace)
import qualified NodeEditor.React.Model.Node.ExpressionNode  as Node
import           NodeEditor.React.Model.NodeEditor           (GraphStatus (GraphError, GraphLoaded))
import           NodeEditor.State.Global                     (State)
import qualified NodeEditor.State.Global                     as Global



applyResult :: GraphLocation -> Result.Result -> Command State ()
applyResult gl res = inCurrentLocation gl $ applyResult' False res

applyResultPreventingExpressionNodesPorts :: GraphLocation -> Result.Result -> Command State ()
applyResultPreventingExpressionNodesPorts gl res = inCurrentLocation gl $ applyResult' True res


applyResult' :: Bool -> Result.Result -> NodePath -> Command State ()
applyResult' preventPorts res path = unlessM (checkBreadcrumb res) $
    case res ^. Result.graphUpdates of
        Left err -> handleGraphError err
        Right graphUpdates -> do
            let exprNodeUpdateFunction = if preventPorts then localUpdateOrAddExpressionNodePreventingPorts else localUpdateOrAddExpressionNode
            void $ localRemoveNodes       . map (convert . (path,)) $ res ^. Result.removedNodes
            void $ localRemoveConnections . map (prependPath path)  $ res ^. Result.removedConnections
            mapM_ (exprNodeUpdateFunction . convert . (path,)) $ graphUpdates ^. Graph.nodes
            let inputSidebar  = graphUpdates ^. Graph.inputSidebar
                outputSidebar = graphUpdates ^. Graph.outputSidebar
            when (isJust inputSidebar)  $ forM_ inputSidebar  $ localUpdateOrAddInputNode  . convert . (path,)
            when (isJust outputSidebar) $ forM_ outputSidebar $ localUpdateOrAddOutputNode . convert . (path,)
            void $ localAddConnections . map (\(src', dst') -> (prependPath path src', prependPath path dst')) $ graphUpdates ^. Graph.connections
            setGraphStatus GraphLoaded

handleGraphError :: Error GraphError -> Command State ()
handleGraphError e = case e ^. errorType of
    BreadcrumbDoesNotExist -> do
        setGraphStatus (GraphError e)
        mayWorkspace <- getWorkspace
        let isOnTop = fromMaybe True (Workspace.isOnTopBreadcrumb <$> mayWorkspace)
        if isOnTop then fatal "Cannot get file from backend" else exitBreadcrumb
    OtherGraphError        -> setGraphStatus (GraphError e)

handleLunaError :: Error LunaError -> Command State ()
handleLunaError (Error (ErrorAPI.Graph tpe) content) = handleGraphError $ Error tpe content
handleLunaError _                                    = fatal "Cannot get file from backend"

checkBreadcrumb :: Result.Result -> Command State Bool
checkBreadcrumb res = do
    bc <- maybe def (view (Workspace.currentLocation . GraphLocation.breadcrumb)) <$> getWorkspace
    let nodeOnPathDeleted = any (containsNode bc) (res ^. Result.removedNodes)

    when nodeOnPathDeleted $ exitBreadcrumb
    return nodeOnPathDeleted

handle :: Event.Event -> Maybe (Command State ())
handle (Event.Batch ev) = Just $ case ev of
    GetProgramResponse response -> handleResponse response success failure where
        location'       = response ^. Response.request . GetProgram.location
        requestId       = response ^. Response.requestId
        success result  = whenM (isCurrentLocation location') $ do
            putStrLn "GetProgram"
            let location = result ^. GetProgram.newLocation
            unless (location' == location) $ do
                modifyApp $ workspace . _Just . Workspace.currentLocation .= location
                Atom.setActiveLocation location
            setBreadcrumbs $ result ^. GetProgram.breadcrumb
            setCurrentImports $ result ^. GetProgram.availableImports
            case result ^. GetProgram.graph of
                Left err    -> handleGraphError err
                Right graph -> do
                    let nodes       = convert . (NodeLoc.empty,) <$> graph ^. Graph.nodes
                        input       = convert . (NodeLoc.empty,) <$> graph ^. Graph.inputSidebar
                        output      = convert . (NodeLoc.empty,) <$> graph ^. Graph.outputSidebar
                        connections = graph ^. Graph.connections
                        monads      = graph ^. Graph.monads
                    updateGraph nodes input output connections monads
                    setGraphStatus GraphLoaded
                    setScreenTransform $ result ^. GetProgram.camera
                    whenM (isOwnRequest requestId) $ withJust (result ^. GetProgram.typeRepToVisMap) $ \visMap ->
                        Global.preferedVisualizers .= visMap
                    updateScene
        failure err _ = handleLunaError err

    AddConnectionResponse response -> handleResponse response success failure where
        requestId   = response ^. Response.requestId
        request     = response ^. Response.request
        location    = request  ^. AddConnection.location
        failure _ _ = whenM (isOwnRequest requestId) $ revertAddConnection request
        success     = applyResult location

    AddImportsResponse response -> handleResponse response success doNothing2 where
        request     = response ^. Response.request
        newImports  = request  ^. AddImports.modules
        location    = request  ^. AddImports.location
        success res = do
            Global.nodeSearcherData . NS.currentImports %= nub . (newImports <>)
            applyResult location res

    AddNodeResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddNode.location
        nl             = request  ^. AddNode.nodeLoc
        failure _ _    = whenM (isOwnRequest requestId) $ revertAddNode request
        success result = do
            applyResult location result
            whenM (isOwnRequest requestId) $ collaborativeModify [nl]

    AddPortResponse response -> handleResponse response success failure where
        requestId   = response ^. Response.requestId
        request     = response ^. Response.request
        location    = request  ^. AddPort.location
        failure _ _ = whenM (isOwnRequest requestId) $ revertAddPort request
        success     = applyResult location

    AddSubgraphResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddSubgraph.location
        failure _ _    = whenM (isOwnRequest requestId) $ revertAddSubgraph request
        success result = do
            applyResult location result
            inCurrentLocation location $ \path -> whenM (isOwnRequest requestId) $
                case result ^. Result.graphUpdates of
                    Right graphUpdates ->
                        collaborativeModify $ map (convert . (path,) . view nodeId) $ graphUpdates ^. Graph.nodes
                    Left _ -> return ()

    AtomPasteResponse response -> handleResponse response success doNothing2 where
        request   = response ^. Response.request
        location  = request  ^. AtomPaste.location
        success   = applyResult location

    AutolayoutNodesResponse response -> handleResponse response success doNothing2 where
        location     = response ^. Response.request . AutolayoutNodes.location
        shouldCenter = response ^. Response.request . AutolayoutNodes.centerGraph
        success res  = applyResult location res >> when shouldCenter centerGraph

    CollaborationUpdate update -> inCurrentLocation (update ^. CollaborationUpdate.location) $ \path -> do
        let clientId = update ^. CollaborationUpdate.clientId
            touchNodes nodeLocs setter = forM_ nodeLocs $ \nl ->
                modifyExpressionNode (prependPath path nl) setter
        myClientId  <- use $ Global.backend . Global.clientId
        currentTime <- liftIO DT.getCurrentTime
        when (clientId /= myClientId) $ do
            clientColor <- updateClient clientId
            case update ^. CollaborationUpdate.event of
                CollaborationUpdate.Touch       nodeLocs -> touchNodes nodeLocs $  Node.collaboration . Node.touch  . at clientId ?= (DT.addSeconds (2 * refreshTime) currentTime, clientColor)
                CollaborationUpdate.Modify      nodeLocs -> touchNodes nodeLocs $ do
                    Node.collaboration . Node.touch  . at clientId %= bumpTime (DT.addSeconds modifyTime currentTime) clientColor
                    Node.collaboration . Node.modify . at clientId ?= DT.addSeconds modifyTime currentTime
                CollaborationUpdate.CancelTouch nodeLocs -> touchNodes nodeLocs $  Node.collaboration . Node.touch  . at clientId .= Nothing
                CollaborationUpdate.Refresh             -> touchCurrentlySelected

    CollapseToFunctionResponse response -> handleResponse response success doNothing2 where
        request         = response ^. Response.request
        location        = request  ^. CollapseToFunction.location
        success         = applyResult location

    CopyResponse response -> handleResponse response success doNothing2 where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request ^. Copy.location
        success result = inCurrentLocation location $ const $
            whenM (isOwnRequest requestId) $ do
                let plain = convert $ result ^. Copy.clipboardPlain
                let meta  = convert $ result ^. Copy.clipboardMeta
                liftIO $ JS.copyStringToClipboard plain meta

    DumpGraphVizResponse response -> handleResponse response doNothing doNothing2

    --TODO[LJK, PM]: Review this Handler
    GetSubgraphsResponse response -> handleResponse response success doNothing2 where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request ^. GetSubgraphs.location
        success result = inCurrentLocation location $ \path ->
            whenM (isOwnRequest requestId) $
                localMerge path $ result ^. GetSubgraphs.graphs

    MonadsUpdate update -> do
        inCurrentLocation (update ^. MonadsUpdate.location) $ \_path ->
            updateMonads $ update ^. MonadsUpdate.monads --FIXME updateMonads in path!

    MovePortResponse response -> handleResponse response success failure where
        requestId   = response ^. Response.requestId
        request     = response ^. Response.request
        location    = request  ^. MovePort.location
        failure _ _ = whenM (isOwnRequest requestId) $ revertMovePort request
        success     = applyResult location

    NodeResultUpdate update -> do
        let location = update ^. NodeResultUpdate.location
        inCurrentLocation location $ \path -> do
            let nid = update ^. NodeResultUpdate.nodeId
            updateNodeValueAndVisualization (convert (path, nid)) $ update ^. NodeResultUpdate.value
            setNodeProfilingData            (convert (path, nid)) $ update ^. NodeResultUpdate.execTime

    NodeTypecheckerUpdate update -> do
      inCurrentLocation (update ^. NodeTCUpdate.location) $ \path ->
          void $ localUpdateNodeTypecheck path $ update ^. NodeTCUpdate.node

    PasteResponse response -> handleResponse response success doNothing2 where
        request   = response ^. Response.request
        location  = request  ^. Paste.location
        success   = applyResult location

    ProjectMoved response -> handleResponse response success doNothing2 where
        request   = response ^. Response.request
        success _ = localMoveProject (request ^. MoveProject.oldPath) (request ^. MoveProject.newPath)

    RedoResponse _response -> $notImplemented

    RemoveConnectionResponse response -> handleResponse response success failure where
        requestId         = response ^. Response.requestId
        request           = response ^. Response.request
        location          = request  ^. RemoveConnection.location
        failure _ inverse = whenM (isOwnRequest requestId) $ revertRemoveConnection request inverse
        success           = applyResult location

    RemoveNodesResponse response -> handleResponse response success failure where
        requestId         = response ^. Response.requestId
        request           = response ^. Response.request
        location          = request  ^. RemoveNodes.location
        failure _ inverse = whenM (isOwnRequest requestId) $ revertRemoveNodes request inverse
        success           = applyResult location

    RemovePortResponse response -> handleResponse response success failure where
        requestId         = response ^. Response.requestId
        request           = response ^. Response.request
        location          = request  ^. RemovePort.location
        failure _ inverse = whenM (isOwnRequest requestId) $ revertRemovePort request inverse
        success           = applyResult location

    RenameNodeResponse response -> handleResponse response success failure where
        requestId         = response ^. Response.requestId
        request           = response ^. Response.request
        location          = request  ^. RenameNode.location
        failure _ inverse = whenM (isOwnRequest requestId) $ revertRenameNode request inverse
        success           = applyResult location

    RenamePortResponse response -> handleResponse response success failure where
        requestId         = response ^. Response.requestId
        request           = response ^. Response.request
        location          = request  ^. RenamePort.location
        failure _ inverse = whenM (isOwnRequest requestId) $ revertRenamePort request inverse
        success           = applyResult location

    SearchNodesResponse response -> handleResponse response success doNothing2 where
        success = localAddSearcherHints . view SearchNodes.searcherHints

    SetCodeResponse response -> handleResponse response success doNothing2 where
        request         = response ^. Response.request
        location        = request  ^. SetCode.location
        success         = applyResult location

    SetNodeExpressionResponse response -> handleResponse response success failure where
        requestId         = response ^. Response.requestId
        request           = response ^. Response.request
        location          = request  ^. SetNodeExpression.location
        failure _ inverse = whenM (isOwnRequest requestId) $ revertSetNodeExpression request inverse
        success           = applyResult location

    SetNodesMetaResponse response -> handleResponse response success failure where
        requestId         = response ^. Response.requestId
        request           = response ^. Response.request
        location          = request  ^. SetNodesMeta.location
        failure _ inverse = whenM (isOwnRequest requestId) $ revertSetNodesMeta request inverse
        success           = whenM (not <$> isOwnRequest requestId) . applyResult location

    SetPortDefaultResponse response -> handleResponse response success failure where
        requestId         = response ^. Response.requestId
        request           = response ^. Response.request
        location          = request  ^. SetPortDefault.location
        failure _ inverse = whenM (isOwnRequest requestId) $ revertSetPortDefault request inverse
        success           = applyResultPreventingExpressionNodesPorts location

    SubstituteResponse response -> handleResponse response success doNothing2 where
        location    = response ^. Response.request . Substitute.location
        success res = do
            applyResult location (res ^. Substitute.defResult)
            withJust (res ^. Substitute.importChange) setCurrentImports

    TypeCheckResponse response -> handleResponse response doNothing doNothing2

    UndoResponse _response -> $notImplemented

    _ -> return ()
handle _ = Nothing
