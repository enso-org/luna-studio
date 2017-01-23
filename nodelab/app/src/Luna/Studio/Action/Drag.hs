{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Drag
    ( startDrag
    , drag
    , stopDrag
    ) where

import           Control.Arrow
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Position                      (Position, move, toTuple, vector)
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as Node
import qualified Luna.Studio.Action.Batch           as BatchCmd
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph           (selectNodes, selectedNodes, updateConnectionsForNodes)
import           Luna.Studio.Action.Node            (snap)
import           Luna.Studio.Event.Mouse            (workspacePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), Drag (Drag), dragAction)
import qualified Luna.Studio.State.Action           as Action
import           Luna.Studio.State.Global           (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph
import           React.Flux                         (MouseEvent)

instance Action (Command State) Drag where
    begin    = beginActionWithKey    dragAction
    continue = continueActionWithKey dragAction
    update   = updateActionWithKey   dragAction
    end _    = updateMovedNodes >> removeActionFromState dragAction

startDrag :: NodeId -> MouseEvent -> Bool -> Command State ()
startDrag nodeId evt snapped = do
    coord <- workspacePosition evt
    mayNode <- Global.getNode nodeId
    withJust mayNode $ \node -> do
        let isSelected = node ^. Model.isSelected
        when (not isSelected) $ selectNodes [nodeId]
        nodes <- selectedNodes
        let nodesPos = Map.fromList $ (view Model.nodeId &&& view Model.position) <$> nodes
        if snapped
            then do
                let snappedNodes = Map.map snap nodesPos
                begin $ Drag coord nodeId snappedNodes
                moveNodes snappedNodes
            else begin $ Drag coord nodeId nodesPos

drag :: MouseEvent -> Bool -> Drag -> Command State ()
drag evt snapped state = do
    coord <- workspacePosition evt
    let mouseStartPos = view Action.dragStartPos      state
        draggedNodeId = view Action.dragNodeId        state
        nodesStartPos = view Action.dragNodesStartPos state
        delta = coord ^. vector - mouseStartPos ^. vector
        shift' = if snapped then
                     case Map.lookup draggedNodeId nodesStartPos of
                         Just pos -> do
                             snap (move pos delta) ^. vector - pos ^. vector
                         Nothing  -> delta
                 else delta
    moveNodes $ Map.map (flip move shift') nodesStartPos

moveNodes :: Map NodeId Position -> Command State ()
moveNodes nodesPos = do
    Global.modifyNodeEditor $ forM_ (Map.toList nodesPos) $ \(nodeId, pos) -> do
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


stopDrag :: MouseEvent -> Drag ->  Command State ()
stopDrag evt state = do
    removeActionFromState dragAction
    coord <- workspacePosition evt
    let startPos = view Action.dragStartPos state
        nodeId   = view Action.dragNodeId   state
    if (startPos /= coord) then
        updateMovedNodes
    else selectNodes [nodeId]
