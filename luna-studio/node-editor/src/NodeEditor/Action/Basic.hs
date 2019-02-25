module NodeEditor.Action.Basic
    ( module NodeEditor.Action.Basic
    , module X
    ) where

import Common.Prelude
import Data.Map (Map)
import LunaStudio.Data.Position                   (Position)

import NodeEditor.Action.Batch                          (addConnectionRequest, moveNodeRequest)
import NodeEditor.Action.Basic.AddConnection       as X (connect,
                                                         localAddConnection,
                                                         localAddConnections)
import NodeEditor.Action.Basic.AddNode             as X (createNode,
                                                         localAddExpressionNode,
                                                         localSetInputSidebar,
                                                         localSetOutputSidebar)
import NodeEditor.Action.Basic.AddPort             as X (addPort, localAddPort)
import NodeEditor.Action.Basic.AddSubgraph         as X (addSubgraph,
                                                         localAddSubgraph,
                                                         localUpdateSubgraph)
import NodeEditor.Action.Basic.Atom                as X (setFile, unsetFile,
                                                         updateFilePath)
import NodeEditor.Action.Basic.CollapseToFunction  as X (collapseToFunction)
import NodeEditor.Action.Basic.CreateGraph         as X (updateGraph,
                                                         updateWithAPIGraph)
import NodeEditor.Action.Basic.EnterBreadcrumb     as X (enterBreadcrumb,
                                                         enterBreadcrumbs,
                                                         enterNode,
                                                         exitBreadcrumb)
import NodeEditor.Action.Basic.FocusNode           as X (focusNode, focusNodes,
                                                         updateNodeZOrder)
import NodeEditor.Action.Basic.Merge               as X (localMerge,
                                                         localUnmerge)
import NodeEditor.Action.Basic.ModifyCamera        as X (centerGraph,
                                                         modifyCamera,
                                                         resetCamera)
import NodeEditor.Action.Basic.MovePort            as X (localMovePort,
                                                         movePort)
import NodeEditor.Action.Basic.MoveProject         as X (localMoveProject)
import NodeEditor.Action.Basic.ProjectManager      as X (loadGraph,
                                                         navigateToGraph,
                                                         saveSettings)
import NodeEditor.Action.Basic.RemoveConnection    as X (localRemoveConnection,
                                                         localRemoveConnections,
                                                         removeConnection,
                                                         removeConnections,
                                                         removeConnectionsBetweenNodes)
import NodeEditor.Action.Basic.RemoveNode          as X (localRemoveNode,
                                                         localRemoveNodes,
                                                         removeNode,
                                                         removeNodes,
                                                         removeSelectedNodes)
import NodeEditor.Action.Basic.RemovePort          as X (localRemovePort,
                                                         removePort)
import NodeEditor.Action.Basic.RenameNode          as X (localRenameNode,
                                                         renameNode)
import NodeEditor.Action.Basic.RenamePort          as X (localRenamePort,
                                                         renamePort)
import NodeEditor.Action.Basic.Scene               as X (getScene, updateScene)
import NodeEditor.Action.Basic.SelectNode          as X (dropSelectionHistory,
                                                         modifySelectionHistory,
                                                         selectAll, selectNode,
                                                         selectNodes,
                                                         selectPreviousNodes,
                                                         toggleSelect,
                                                         unselectAll)
import NodeEditor.Action.Basic.SetNodeExpression   as X (localSetNodeExpression,
                                                         setNodeExpression)
import NodeEditor.Action.Basic.SetNodeMeta         as X (localMoveNode,
                                                         localMoveNodes,
                                                         localSetNodeMeta,
                                                         localSetNodesMeta,
                                                         moveNode, moveNodes,
                                                         setNodeMeta,
                                                         setNodesMeta,
                                                         toMetaUpdate)
import NodeEditor.Action.Basic.SetNodeMode         as X (toggleSelectedNodesMode,
                                                         toggleSelectedNodesUnfold)
import NodeEditor.Action.Basic.SetPortDefault      as X (localSetPortDefault,
                                                         setPortDefault)
import NodeEditor.Action.Basic.SetPortMode         as X (setInputSidebarPortMode,
                                                         setOutputSidebarPortMode)
import NodeEditor.Action.Basic.Undo                as X (redo, undo)
import NodeEditor.Action.Basic.UpdateCollaboration as X (updateClient,
                                                         updateCollaboration)
import NodeEditor.Action.Basic.UpdateNode          as X (NodeUpdateModification (KeepNodeMeta, KeepPorts, MergePorts),
                                                         localUpdateToEnterExpressionNode,
                                                         localUpdateExpressionNode,
                                                         localUpdateExpressionNodeInPorts,
                                                         localUpdateExpressionNodeOutPorts,
                                                         localUpdateExpressionNodes,
                                                         localUpdateInputNode,
                                                         localUpdateIsDefinition,
                                                         localUpdateNodeCode,
                                                         localUpdateNodeTypecheck,
                                                         localUpdateOrAddExpressionNode,
                                                         localUpdateOrAddInputNode,
                                                         localUpdateOrAddOutputNode,
                                                         localUpdateOutputNode)
import NodeEditor.Action.Basic.UpdateNodeValue     as X (updateNodeValueAndVisualization)
import NodeEditor.Action.Basic.UpdateSearcherHints as X (localAddSearcherHints,
                                                         localClearSearcherHints,
                                                         localUpdateSearcherHints,
                                                         selectHint,
                                                         setImportedLibraries,
                                                         updateDocumentation)
import NodeEditor.Action.State.Model               as X (isArgConstructorConnectSrc,
                                                         updateAllPortsMode,
                                                         updateArgConstructorMode,
                                                         updatePortMode,
                                                         updatePortsModeForNode)

import           Common.Action.Command                      (Command)
import           NodeEditor.React.Model.Node                (NodeLoc)
import           NodeEditor.React.Model.Connection          (Connection, src, dst)
import           NodeEditor.State.Global                    (State, backend, clientId)
import qualified Data.Binary      as Binary
import qualified LunaStudio.API.Graph.Transaction      as Transaction
import qualified LunaStudio.API.Topic     as Topic
import NodeEditor.Action.State.App       (getWorkspace)
import NodeEditor.Action.UUID            (registerRequest)
import           LunaStudio.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import           Common.Batch.Connector.Connection        (Message (Message),
                                                           sendRequest)
import           NodeEditor.Batch.Workspace               (currentLocation)

moveNodeOnConnection :: NodeLoc -> Connection -> Map NodeLoc Position -> Command State ()
moveNodeOnConnection nl conn metaUpdate = do
   leftConnect  <- addConnectionRequest (Left $ conn ^. src) $ Right nl
   rightConnect <- addConnectionRequest (Right nl) $ Left $ InPortRef' $ conn ^. dst
   move         <- moveNodeRequest =<< toMetaUpdate metaUpdate
   Just workspace <- getWorkspace

   uuid <- registerRequest
   guiID      <- use $ backend . clientId

   liftIO $ sendRequest $ Message uuid (Just guiID) $
      Transaction.Request (workspace ^. currentLocation) [
         (Topic.topic' move, Binary.encode move),
         (Topic.topic' leftConnect, Binary.encode leftConnect),
         (Topic.topic' rightConnect, Binary.encode rightConnect)
         ]
  -- connect (Left $ conn ^. src) $ Right nl
  -- connect (Right nl)           $ Left $ conn ^. dst