module NodeEditor.Handler.Backend.Graph
    ( handle
    ) where

import Common.Prelude

import qualified Data.DateTime                               as DT
import qualified Data.Set                                    as Set
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
import qualified LunaStudio.API.Graph.SearchNodes            as SearchNodes
import qualified LunaStudio.API.Graph.SetCode                as SetCode
import qualified LunaStudio.API.Graph.SetNodeExpression      as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta           as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault         as SetPortDefault
import qualified LunaStudio.API.Graph.Transaction            as Transaction
import qualified LunaStudio.API.Response                     as Response
import qualified LunaStudio.Data.Connection                  as API
import qualified LunaStudio.Data.Diff                        as Diff
import qualified LunaStudio.Data.Error                       as ErrorAPI
import qualified LunaStudio.Data.GraphLocation               as GraphLocation
import qualified LunaStudio.Data.Node                        as API
import qualified LunaStudio.Data.NodeLoc                     as NodeLoc
import qualified NodeEditor.Action.Basic.UpdateSearcherHints as Searcher
import qualified NodeEditor.Batch.Workspace                  as Workspace
import qualified NodeEditor.Event.Event                      as Event
import qualified NodeEditor.React.Model.Node.ExpressionNode  as Node
import qualified NodeEditor.State.Global                     as Global

import Common.Action.Command                       (Command)
import Common.Report                               (fatal)
import Data.Set                                    (Set)
import LunaStudio.Data.Breadcrumb                  (containsNode)
import LunaStudio.Data.Diff                        (Diff (Diff),
                                                    Modification (..))
import LunaStudio.Data.Error                       (Error (Error), GraphError (BreadcrumbDoesNotExist),
                                                    LunaError, errorType)
import LunaStudio.Data.GraphLocation               (GraphLocation)
import LunaStudio.Data.NodeLoc                     (NodePath, prependPath)
import NodeEditor.Action.Basic                     (NodeUpdateModification (KeepNodeMeta, KeepPorts, MergePorts),
                                                    centerGraph, exitBreadcrumb,
                                                    localAddConnection,
                                                    localMerge,
                                                    localMoveProject,
                                                    localRemoveConnection,
                                                    localRemoveNode,
                                                    localRenameNode,
                                                    localSetInputSidebar,
                                                    localSetNodeExpression,
                                                    localSetNodeMeta,
                                                    localSetOutputSidebar,
                                                    localUpdateCanEnterExpressionNode,
                                                    localUpdateExpressionNodeInPorts,
                                                    localUpdateExpressionNodeOutPorts,
                                                    localUpdateIsDefinition,
                                                    localUpdateNodeCode,
                                                    localUpdateNodeTypecheck,
                                                    localUpdateOrAddExpressionNode,
                                                    setImportedLibraries,
                                                    updateNodeValueAndVisualization,
                                                    updateScene,
                                                    updateWithAPIGraph)
import NodeEditor.Action.Basic.Revert              (revertAddConnection,
                                                    revertAddNode,
                                                    revertAddPort,
                                                    revertAddSubgraph,
                                                    revertMovePort,
                                                    revertRemoveConnection,
                                                    revertRemoveNodes,
                                                    revertRemovePort,
                                                    revertRenameNode,
                                                    revertRenamePort,
                                                    revertSetNodeExpression,
                                                    revertSetNodesMeta,
                                                    revertSetPortDefault)
import NodeEditor.Action.Basic.UpdateCollaboration (bumpTime, modifyTime,
                                                    refreshTime,
                                                    touchCurrentlySelected,
                                                    updateClient)
import NodeEditor.Action.Batch                     (collaborativeModify)
import NodeEditor.Action.State.App                 (getWorkspace, modifyApp,
                                                    setBreadcrumbs)
import NodeEditor.Action.State.Graph               (inCurrentLocation,
                                                    isCurrentLocation)
import NodeEditor.Action.State.NodeEditor          (modifyExpressionNode,
                                                    setGraphStatus,
                                                    setNodeProfilingData,
                                                    setScreenTransform,
                                                    updateMonads,
                                                    updateVisualizers)
import NodeEditor.Action.UUID                      (isOwnRequest)
import NodeEditor.Event.Batch                      (Event (..))
import NodeEditor.Handler.Backend.Common           (doNothing, doNothing2,
                                                    handleResponse, logError,
                                                    resend)
import NodeEditor.React.Model.App                  (workspace)
import NodeEditor.React.Model.NodeEditor           (GraphStatus (GraphError))
import NodeEditor.State.Global                     (State)


applyModification :: NodePath -> Set NodeUpdateModification -> Modification
    -> Command State ()
applyModification p nm = \case
    AddConnection m ->
        void . localAddConnection $ API.prependPath p $ m ^. Diff.newConnection
    AddNode m ->
        localUpdateOrAddExpressionNode nm $ convert (p, m ^. Diff.newNode)
    RemoveConnection m ->
        void . localRemoveConnection $ prependPath p $ m ^. Diff.removeConnectionId
    RemoveNode m ->
        void . localRemoveNode
        $ convert (p, m ^. Diff.removeNodeLoc . NodeLoc.nodeId)
    RenameNode m ->
        void . localRenameNode
        (convert (p, m ^. Diff.renameNodeLoc . NodeLoc.nodeId))
        $ m ^. Diff.newName
    SetBreadcrumb m ->
        setBreadcrumbs $ m ^. Diff.newBreadcrumb
    SetCamera m ->
        setScreenTransform $ m ^. Diff.newCameraTransformation
    SetCanEnterNode m ->
        localUpdateCanEnterExpressionNode
        (convert (p, m ^. Diff.setCanEnterNodeLoc . NodeLoc.nodeId))
        $ m ^. Diff.newCanEnter
    SetCode _ ->
        return ()
    SetDefaultVisualizers m ->
        Global.preferedVisualizers .= m ^. Diff.newDefaultVisualizers
    SetExpression m ->
        void . localSetNodeExpression
        (convert (p, m ^. Diff.setExpressionNodeLoc . NodeLoc.nodeId))
        $ m ^. Diff.newExpression
    SetGraph m ->
        updateWithAPIGraph p $ m ^. Diff.newGraph
    SetGraphError m ->
        handleGraphError $ m ^. Diff.graphError
    SetImports m ->
        setImportedLibraries $ m ^. Diff.newImports
    SetInPorts m ->
        localUpdateExpressionNodeInPorts
        (convert (p, m ^. Diff.setInPortsNodeLoc . NodeLoc.nodeId))
        $ convert <$> m ^. Diff.newInPortTree
    SetInputSidebar m ->
        localSetInputSidebar p $ m ^. Diff.newInputSidebar
    SetIsDefinition m ->
        localUpdateIsDefinition
        (convert (p, m ^. Diff.setIsDefinitionNodeLoc . NodeLoc.nodeId))
        $ m ^. Diff.newIsDefinition
    SetMonadPath m ->
        updateMonads $ m ^. Diff.newMonadPath
    SetNodeCode m ->
        localUpdateNodeCode
        (convert (p, m ^. Diff.setNodeCodeNodeLoc . NodeLoc.nodeId))
        $ m ^. Diff.newNodeCode
    SetNodeMeta m ->
        void . localSetNodeMeta
        (convert (p, m ^. Diff.setNodeMetaNodeLoc . NodeLoc.nodeId))
        $ m ^. Diff.newNodeMeta
    SetOutPorts m ->
        localUpdateExpressionNodeOutPorts
        (convert (p, m ^. Diff.setOutPortsNodeLoc . NodeLoc.nodeId))
        $ convert <$> m ^. Diff.newOutPortTree
    SetOutputSidebar m ->
        localSetOutputSidebar p $ m ^. Diff.newOutputSidebar
    SetExternalVisPath m ->
        updateVisualizers $ m ^. Diff.newExternalVisPath


applyDiff :: GraphLocation -> Set NodeUpdateModification -> Diff
    -> Command State ()
applyDiff gl nm d@(Diff reversedMods) = inCurrentLocation gl
    $ \path -> unlessM (checkBreadcrumb d)
        $ mapM_ (applyModification path nm) $ reverse reversedMods

handleGraphError :: Error GraphError -> Command State ()
handleGraphError e = case e ^. errorType of
    BreadcrumbDoesNotExist -> do
        setGraphStatus (GraphError e)
        mayWorkspace <- getWorkspace
        let isOnTop = fromMaybe True
                $ Workspace.isOnTopBreadcrumb <$> mayWorkspace
        if isOnTop then fatal "Cannot get file from backend" else exitBreadcrumb
    _ -> setGraphStatus (GraphError e)

handleLunaError :: Error LunaError -> Command State ()
handleLunaError (Error (ErrorAPI.Graph tpe) content)
    = handleGraphError $ Error tpe content
handleLunaError _ = fatal "Cannot get file from backend"

checkBreadcrumb :: Diff -> Command State Bool
checkBreadcrumb (Diff mods) = do
    let removedNodeId (Diff.RemoveNode m)
            = Just $ m ^. Diff.removeNodeLoc . NodeLoc.nodeId
        removedNodeId _                   = Nothing
    let removedIds = catMaybes $ removedNodeId <$> mods
    bc <- maybe
        def
        (view (Workspace.currentLocation . GraphLocation.breadcrumb))
        <$> getWorkspace
    let nodeOnPathDeleted = any (containsNode bc) removedIds
    when nodeOnPathDeleted $ exitBreadcrumb
    return nodeOnPathDeleted

handle :: Event.Event -> Maybe (Command State ())
handle (Event.Batch ev) = Just $ case ev of
    GetProgramResponse response -> handleResponse response success failure where
        location'       = response ^. Response.request . GetProgram.location
        requestId       = response ^. Response.requestId
        success result  = whenM (isCurrentLocation location') $ do
            putStrLn "GetProgram"
            print result
            let location = result ^. GetProgram.currentLocation
            unless (location' == location) $ do
                modifyApp
                    $ workspace . _Just . Workspace.currentLocation .= location
                Atom.setActiveLocation location
            ownRequest <- isOwnRequest requestId
            let worksForForeignRequest (Diff.SetExternalVisPath    _) = False
                worksForForeignRequest (Diff.SetDefaultVisualizers _) = False
                worksForForeignRequest _                              = True
                diff = if ownRequest
                    then result ^. GetProgram.diff
                    else (result ^. GetProgram.diff)
                        & Diff.reversedModifications
                            %~ filter worksForForeignRequest
            applyDiff location mempty diff
            updateScene
        failure err _ = logError response err >> handleLunaError err

    AddConnectionResponse response -> handleResponse response success failure where
        requestId     = response ^. Response.requestId
        request       = response ^. Response.request
        location      = request  ^. AddConnection.location
        success       = applyDiff location (Set.singleton KeepNodeMeta)
        failure err _ = do
            logError response err
            whenM (isOwnRequest requestId) $ revertAddConnection request

    AddImportsResponse response -> handleResponse response success doNothing2 where
        request    = response ^. Response.request
        location   = request  ^. AddImports.location
        success    = applyDiff location (Set.fromList [KeepPorts, KeepNodeMeta])

    AddNodeResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddNode.location
        nl             = request  ^. AddNode.nodeLoc
        failure err _  = do
            logError response err
            whenM (isOwnRequest requestId) $ revertAddNode request
        success diff   = do
            applyDiff location (Set.fromList [KeepNodeMeta, MergePorts]) diff
            whenM (isOwnRequest requestId) $ collaborativeModify [nl]

    AddPortResponse response -> handleResponse response success failure where
        requestId     = response ^. Response.requestId
        request       = response ^. Response.request
        location      = request  ^. AddPort.location
        failure err _ = do
            logError response err
            whenM (isOwnRequest requestId) $ revertAddPort request
        success       = applyDiff location (Set.singleton KeepNodeMeta)

    AddSubgraphResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request  ^. AddSubgraph.location
        failure err _  = do
            logError response err
            whenM (isOwnRequest requestId) $ revertAddSubgraph request
        success diff   = do
            applyDiff location mempty diff
            whenM (isOwnRequest requestId)
                $ inCurrentLocation location $ \p -> do
                    let addedNodeId (Diff.AddNode m)
                            = Just $ convert (p, m ^. Diff.newNode . API.nodeId)
                        addedNodeId m
                            = (convert . (p,) . view NodeLoc.nodeId)
                                <$> Diff.getNodeModificationNodeLoc m
                        addedNodesIds = catMaybes
                            $ addedNodeId <$> diff ^. Diff.reversedModifications
                    unless (null addedNodesIds)
                        $ collaborativeModify addedNodesIds

    AtomPasteResponse response -> handleResponse response success failure where
        request       = response ^. Response.request
        location      = request  ^. AtomPaste.location
        success       = applyDiff location mempty
        failure err _ = logError response err

    AutolayoutNodesResponse response -> handleResponse response success failure where
        location = response ^. Response.request . AutolayoutNodes.location
        shouldCenter
            = response ^. Response.request . AutolayoutNodes.shouldCenterGraph
        success diff
            = applyDiff location (Set.singleton KeepPorts) diff
            >> when shouldCenter centerGraph
        failure err _ = logError response err

    CollaborationUpdate update ->
        inCurrentLocation (update ^. CollaborationUpdate.location) $ \path -> do
            let clientId = update ^. CollaborationUpdate.clientId
                touchNodes nodeLocs setter = forM_ nodeLocs $ \nl ->
                    modifyExpressionNode (prependPath path nl) setter
            myClientId  <- use $ Global.backend . Global.clientId
            currentTime <- liftIO DT.getCurrentTime
            when (clientId /= myClientId) $ do
                clientColor <- updateClient clientId
                case update ^. CollaborationUpdate.event of
                    CollaborationUpdate.Touch nodeLocs -> touchNodes nodeLocs $
                        Node.collaboration . Node.touch  . at clientId
                            ?= (DT.addSeconds (2 * refreshTime) currentTime
                               , clientColor)
                    CollaborationUpdate.Modify nodeLocs
                        -> touchNodes nodeLocs $ do
                            Node.collaboration . Node.touch  . at clientId
                                %= bumpTime
                                    (DT.addSeconds modifyTime currentTime)
                                    clientColor
                            Node.collaboration . Node.modify . at clientId
                                ?= DT.addSeconds modifyTime currentTime
                    CollaborationUpdate.CancelTouch nodeLocs ->
                        touchNodes nodeLocs
                            $ Node.collaboration . Node.touch  . at clientId
                                .= Nothing
                    CollaborationUpdate.Refresh -> touchCurrentlySelected

    CollapseToFunctionResponse response -> handleResponse response success failure where
        request       = response ^. Response.request
        location      = request  ^. CollapseToFunction.location
        success       = applyDiff location (Set.singleton KeepNodeMeta)
        failure err _ = logError response err

    CopyResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request ^. Copy.location
        failure err _  = logError response err
        success result = inCurrentLocation location $ const $
            whenM (isOwnRequest requestId) $ do
                let plain = convert $ result ^. Copy.clipboardPlain
                let meta  = convert $ result ^. Copy.clipboardMeta
                liftIO $ JS.copyStringToClipboard plain meta

    DumpGraphVizResponse response -> handleResponse response doNothing failure where
        failure err _ = logError response err

    --TODO[LJK, PM]: Review this Handler
    GetSubgraphsResponse response -> handleResponse response success failure where
        requestId      = response ^. Response.requestId
        request        = response ^. Response.request
        location       = request ^. GetSubgraphs.location
        failure err _  = logError response err
        success result = inCurrentLocation location $ \path ->
            whenM (isOwnRequest requestId) $
                localMerge path $ result ^. GetSubgraphs.graphs

    MonadsUpdate update -> do
        inCurrentLocation (update ^. MonadsUpdate.location) $ \_path ->
            updateMonads $ update ^. MonadsUpdate.monads --FIXME updateMonads in path!

    MovePortResponse response -> handleResponse response success failure where
        requestId     = response ^. Response.requestId
        request       = response ^. Response.request
        location      = request  ^. MovePort.location
        success       = applyDiff location (Set.singleton KeepNodeMeta)
        failure err _ = do
            logError response err
            whenM (isOwnRequest requestId) $ revertMovePort request

    NodeResultUpdate update -> do
        let location = update ^. NodeResultUpdate.location
        inCurrentLocation location $ \path -> do
            let nid = update ^. NodeResultUpdate.nodeId
            updateNodeValueAndVisualization
                (convert (path, nid))
                $ update ^. NodeResultUpdate.value
            setNodeProfilingData
                (convert (path, nid))
                $ update ^. NodeResultUpdate.execTime

    NodeTypecheckerUpdate update -> do
      inCurrentLocation (update ^. NodeTCUpdate.location) $ \path ->
          void $ localUpdateNodeTypecheck path $ update ^. NodeTCUpdate.node

    PasteResponse response -> handleResponse response success failure where
        request       = response ^. Response.request
        location      = request  ^. Paste.location
        success       = applyDiff location mempty
        failure err _ = logError response err

    ProjectMoved response -> handleResponse response success failure where
        request       = response ^. Response.request
        failure err _ = logError response err
        success _     = localMoveProject
            (request ^. MoveProject.oldPath)
            (request ^. MoveProject.newPath)

    RedoResponse _response -> $notImplemented

    RemoveConnectionResponse response -> handleResponse response success failure where
        requestId           = response ^. Response.requestId
        request             = response ^. Response.request
        location            = request  ^. RemoveConnection.location
        success             = applyDiff location (Set.singleton KeepNodeMeta)
        failure err inverse = do
            logError response err
            whenM (isOwnRequest requestId)
                $ revertRemoveConnection request inverse

    RemoveNodesResponse response -> handleResponse response success failure where
        requestId           = response ^. Response.requestId
        request             = response ^. Response.request
        location            = request  ^. RemoveNodes.location
        success             = applyDiff location (Set.singleton KeepNodeMeta)
        failure err inverse = do
            logError response err
            whenM (isOwnRequest requestId) $ revertRemoveNodes request inverse

    RemovePortResponse response -> handleResponse response success failure where
        requestId           = response ^. Response.requestId
        request             = response ^. Response.request
        location            = request  ^. RemovePort.location
        success             = applyDiff location (Set.singleton KeepNodeMeta)
        failure err inverse = do
            logError response err
            whenM (isOwnRequest requestId) $ revertRemovePort request inverse

    RenameNodeResponse response -> handleResponse response success failure where
        requestId           = response ^. Response.requestId
        request             = response ^. Response.request
        location            = request  ^. RenameNode.location
        success             = applyDiff location (Set.singleton KeepNodeMeta)
        failure err inverse = do
            logError response err
            whenM (isOwnRequest requestId) $ revertRenameNode request inverse

    RenamePortResponse response -> handleResponse response success failure where
        requestId           = response ^. Response.requestId
        request             = response ^. Response.request
        location            = request  ^. RenamePort.location
        success             = applyDiff location (Set.singleton KeepNodeMeta)
        failure err inverse = do
            logError response err
            whenM (isOwnRequest requestId) $ revertRenamePort request inverse

    SearchNodesResponse response -> handle where
        handle      = handleResponse response success failure
        request     = response ^. Response.request
        success     = Searcher.addDatabaseHints . view SearchNodes.searcherHints
        failure _ _ = resend request

    SetCodeResponse response -> handleResponse response success failure where
        request  = response ^. Response.request
        location = request  ^. SetCode.location
        success  = applyDiff location mempty
        failure err _ = logError response err

    SetNodeExpressionResponse response -> handleResponse response success failure where
        requestId           = response ^. Response.requestId
        request             = response ^. Response.request
        location            = request  ^. SetNodeExpression.location
        success             = applyDiff location (Set.singleton KeepNodeMeta)
        failure err inverse = do
            logError response err
            whenM (isOwnRequest requestId)
                $ revertSetNodeExpression request inverse

    SetNodesMetaResponse response -> handleResponse response success failure where
        requestId           = response ^. Response.requestId
        request             = response ^. Response.request
        location            = request  ^. SetNodesMeta.location
        success             = whenM (not <$> isOwnRequest requestId)
            . applyDiff location (Set.singleton KeepPorts)
        failure err inverse = do
            logError response err
            whenM (isOwnRequest requestId) $ revertSetNodesMeta request inverse

    SetPortDefaultResponse response -> handleResponse response success failure where
        requestId           = response ^. Response.requestId
        request             = response ^. Response.request
        location            = request  ^. SetPortDefault.location
        success             = applyDiff
            location
            (Set.fromList [KeepPorts, KeepNodeMeta])
        failure err inverse = do
            logError response err
            whenM (isOwnRequest requestId)
                $ revertSetPortDefault request inverse

    SubstituteResponse response -> handleResponse response success failure where
        location      = response ^. Response.request . Substitute.location
        success       = applyDiff location mempty
        failure err _ = logError response err

    TransactionResponse response -> handleResponse response success failure where
        location = response ^. Response.request . Transaction.location
        success  = applyDiff location mempty
        failure err inverse = logError response err

    TypeCheckResponse response -> handleResponse response doNothing failure where
        failure err _ = logError response err

    UndoResponse _response -> $notImplemented

    _ -> return ()
handle _ = Nothing
