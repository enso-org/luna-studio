{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Drag
    ( startDrag
    , drag
    , endDrag
    ) where

import           Control.Arrow
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as Node
import qualified Luna.Studio.Action.Batch           as BatchCmd
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph           (selectNodes, selectedNodes, updateConnectionsForNodes)
import           Luna.Studio.Action.Node            (snap)
import           Luna.Studio.Data.Vector            (Position, move, toTuple, vector)
import           Luna.Studio.Event.Mouse            (workspacePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (Action (Drag))
import qualified Luna.Studio.State.Drag             as Drag
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph
import           Luna.Studio.State.StatefulAction   (StatefulAction (exit, matchState, pack, start))
import           React.Flux                         (MouseEvent)

instance StatefulAction Drag.State where
    pack state = Drag state
    matchState (Drag state) = Just state
    matchState _ = Nothing
    exit _ = Global.performedAction .= Nothing

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
                start $ Drag.State coord nodeId snappedNodes
                moveNodes snappedNodes
            else start $ Drag.State coord nodeId nodesPos

drag :: MouseEvent -> Bool -> Drag.State -> Command State ()
drag evt snapped state = do
    coord <- workspacePosition evt
    let mouseStartPos = view Drag.dragStartPos state
        draggedNodeId = view Drag.draggedNodeId state
        nodesStartPos = view Drag.nodesStartPos state
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

endDrag :: MouseEvent -> Drag.State ->  Command State ()
endDrag evt state = do
    coord <- workspacePosition evt
    let startPos = view Drag.dragStartPos state
        nodeId = view Drag.draggedNodeId state
    if (startPos /= coord)
        then do
            selected <- selectedNodes
            let nodesToUpdate = (\n -> (n ^. Model.nodeId, n ^. Model.position)) <$> selected
            updates <- forM nodesToUpdate $ \(wid, pos) -> do
                Global.graph . Graph.nodesMap . ix wid . Node.position .= toTuple (pos ^. vector)
                newMeta <- preuse $ Global.graph . Graph.nodesMap . ix wid . Node.nodeMeta
                return $ (wid, ) <$> newMeta
            BatchCmd.updateNodeMeta $ catMaybes updates
            updateConnectionsForNodes $ fst <$> nodesToUpdate
        else selectNodes [nodeId]
    mayPerformedAction <- use $ Global.performedAction
    withJust mayPerformedAction $ \performedAction -> case performedAction of
        Drag _ -> Global.performedAction .= Nothing
        _      -> return ()
