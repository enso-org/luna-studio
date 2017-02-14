{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Node.Drag
    ( startNodeDrag
    , nodeDrag
    , stopNodeDrag
    ) where

import           Control.Arrow
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Position                      (Position, move, toTuple, vector)
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as Node
import qualified Luna.Studio.Action.Batch           as BatchCmd
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Selection (selectNodes, selectedNodes)
import           Luna.Studio.Action.Graph.Update    (updateConnectionsForNodes)
import           Luna.Studio.Action.Node.Snap       (snap)
import           Luna.Studio.Event.Mouse            (workspacePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), NodeDrag (NodeDrag), nodeDragAction)
import qualified Luna.Studio.State.Action           as Action
import           Luna.Studio.State.Global           (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph
import           React.Flux                         (MouseEvent)


instance Action (Command State) NodeDrag where
    begin    = beginActionWithKey    nodeDragAction
    continue = continueActionWithKey nodeDragAction
    update   = updateActionWithKey   nodeDragAction
    end _    = updateMovedNodes >> removeActionFromState nodeDragAction

startNodeDrag :: MouseEvent -> NodeId -> Bool -> Command State ()
startNodeDrag evt nodeId snapped = do
    coord <- workspacePosition evt
    mayNode <- Global.getNode nodeId
    withJust mayNode $ \node -> do
        let isSelected = node ^. Model.isSelected
        unless isSelected $ selectNodes [nodeId]
        nodes <- selectedNodes
        let nodesPos = Map.fromList $ (view Model.nodeId &&& view Model.position) <$> nodes
        if snapped
            then do
                let snappedNodes = Map.map snap nodesPos
                begin $ NodeDrag coord nodeId snappedNodes
                moveNodes snappedNodes
            else begin $ NodeDrag coord nodeId nodesPos

nodeDrag :: MouseEvent -> Bool -> NodeDrag -> Command State ()
nodeDrag evt snapped state = do
    coord <- workspacePosition evt
    let mouseStartPos = view Action.nodeDragStartPos      state
        draggedNodeId = view Action.nodeDragNodeId        state
        nodesStartPos = view Action.nodeDragNodesStartPos state
        delta = coord ^. vector - mouseStartPos ^. vector
        shift' = if snapped then
                     case Map.lookup draggedNodeId nodesStartPos of
                         Just pos ->
                             snap (move pos delta) ^. vector - pos ^. vector
                         Nothing  -> delta
                 else delta
    moveNodes $ Map.map (flip move shift') nodesStartPos

moveNodes :: Map NodeId Position -> Command State ()
moveNodes nodesPos = do
    Global.modifyNodeEditor $ forM_ (Map.toList nodesPos) $ \(nodeId, pos) ->
        NodeEditor.nodes . at nodeId %= fmap (Model.position .~ pos)
    updateConnectionsForNodes $ Map.keys nodesPos

updateMovedNodes :: Command State ()
updateMovedNodes = do
    selected <- selectedNodes
    let nodesToUpdate = (view Model.nodeId &&& view Model.position) <$> selected
    updates <- forM nodesToUpdate $ \(wid, pos) -> do
        Global.graph . Graph.nodesMap . ix wid . Node.position .= toTuple (pos ^. vector)
        newMeta <- preuse $ Global.graph . Graph.nodesMap . ix wid . Node.nodeMeta
        return $ (wid, ) <$> newMeta
    BatchCmd.updateNodeMeta $ catMaybes updates
    updateConnectionsForNodes $ fst <$> nodesToUpdate


stopNodeDrag :: MouseEvent -> NodeDrag ->  Command State ()
stopNodeDrag evt state = do
    removeActionFromState nodeDragAction
    coord <- workspacePosition evt
    let startPos = view Action.nodeDragStartPos state
        nodeId   = view Action.nodeDragNodeId   state
    if startPos /= coord then
        updateMovedNodes
    else selectNodes [nodeId]
