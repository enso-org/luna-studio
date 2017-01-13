{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
module Luna.Studio.Handler.Drag
    ( toAction
    ) where

import           Control.Arrow
import           Control.Monad.State          ()
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as Node
import           Event.Event
import           Event.UI                     (UIEvent (AppEvent, NodeEvent))
import qualified Luna.Studio.Action.Batch     as BatchCmd
import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Action.Graph     (selectNodes, selectedNodes, updateConnectionsForNodes)
import           Luna.Studio.Action.Node      (snap)
import           Luna.Studio.Data.Vector      (Position, move, toTuple, vector)
import           Luna.Studio.Event.Mouse      (workspacePosition)
import qualified Luna.Studio.Event.Mouse      as Mouse
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App  as App
import qualified Luna.Studio.React.Event.Node as Node
import qualified Luna.Studio.React.Model.Node as Model
import           Luna.Studio.State.Drag       (DragHistory (..))
import qualified Luna.Studio.State.Drag       as Drag
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.Graph      as Graph
import           React.Flux                   (MouseEvent)


toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.MouseDown evt nodeId))) = Just $ when shouldProceed $ startDrag nodeId evt shouldSnap  where
    shouldProceed = Mouse.withoutMods evt Mouse.leftButton || Mouse.withShift evt Mouse.leftButton
    shouldSnap    = Mouse.withoutMods evt Mouse.leftButton
toAction (UI (AppEvent  (App.MouseUp evt)))   = Just $ stopDrag evt
toAction (UI (AppEvent  (App.MouseMove evt))) = Just $ handleMove evt shouldSnap where
    shouldSnap = Mouse.withoutMods evt Mouse.leftButton
toAction _                                    = Nothing


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
                Global.drag . Drag.history ?= DragHistory coord nodeId snappedNodes
                moveNodes snappedNodes
            else Global.drag . Drag.history ?= DragHistory coord nodeId nodesPos


handleMove :: MouseEvent -> Bool -> Command State ()
handleMove evt snapped = do
    coord <- workspacePosition evt
    -- TODO[react]: Probably remove
    -- factor <- use $ Global.camera . Camera.camera . Camera.factor
    dragHistory <- use $ Global.drag . Drag.history
    withJust dragHistory $ \(DragHistory mousePos draggedNodeId nodesPos) -> do
        let delta = coord ^. vector - mousePos ^. vector
            --TODO[react]: Find out if we need some extra rescale here
            deltaWs = delta --Camera.scaledScreenToWorkspace factor delta
            shift' = if snapped
                        then case Map.lookup draggedNodeId nodesPos of
                            Just pos -> do
                                snap (move pos deltaWs) ^. vector - pos ^. vector
                            Nothing  -> deltaWs
                        else deltaWs
        moveNodes $ Map.map (flip move shift') nodesPos

moveNodes :: Map NodeId Position -> Command State ()
moveNodes nodesPos = do
    Global.modifyNodeEditor $ forM_ (Map.toList nodesPos) $ \(nodeId, pos) -> do
        NodeEditor.nodes . at nodeId %= fmap (Model.position .~ pos)
    updateConnectionsForNodes $ Map.keys nodesPos

stopDrag :: MouseEvent -> Command State ()
stopDrag evt = do
    coord <- workspacePosition evt
    dragHistory <- use $ Global.drag . Drag.history
    withJust dragHistory $ \(DragHistory start nodeId _) -> do
        Global.drag . Drag.history .= Nothing
        if (start /= coord)
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
