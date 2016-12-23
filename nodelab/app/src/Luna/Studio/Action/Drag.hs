{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
module Luna.Studio.Action.Drag
    ( toAction
    ) where

import           Control.Arrow
import           Control.Monad.State                  ()
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Empire.API.Data.Node                 (NodeId)
import qualified Empire.API.Data.Node                 as Node
import           Event.Event
import           Event.UI                             (UIEvent (AppEvent, NodeEvent))
import qualified Luna.Studio.Commands.Batch           as BatchCmd
import           Luna.Studio.Commands.Command         (Command)
import           Luna.Studio.Commands.Graph.Connect   (updateConnectionsForNodes)
import           Luna.Studio.Commands.Graph.Selection (selectNodes, selectedNodes)
import           Luna.Studio.Commands.Node.Snap       (snap)
import           Luna.Studio.Data.Vector              (Position, moveByVector, toTuple, vector)
import           Luna.Studio.Event.Mouse
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App          as App
import qualified Luna.Studio.React.Event.Node         as Node
import qualified Luna.Studio.React.Model.Node         as Model
import           Luna.Studio.React.Store              (widget, _widget)
import qualified Luna.Studio.React.Store              as Store
import           Luna.Studio.State.Drag               (DragHistory (..))
import qualified Luna.Studio.State.Drag               as Drag
import           Luna.Studio.State.Global             (State)
import qualified Luna.Studio.State.Global             as Global
import qualified Luna.Studio.State.Graph              as Graph


toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.MouseDown evt nodeId))) = Just $ do
    when (withoutMods evt || withShift evt) $ startDrag nodeId (mousePosition evt) (not $ withShift evt)
toAction (UI (AppEvent  (App.MouseUp evt)))   = Just $ stopDrag $ mousePosition evt
toAction (UI (AppEvent  (App.MouseMove evt))) = Just $ handleMove (mousePosition evt) (not $ withShift evt)
toAction _                                    = Nothing


startDrag :: NodeId -> Position -> Bool -> Command State ()
startDrag nodeId coord snapped = do
    mayDraggedNodeRef <- Global.getNode nodeId
    withJust mayDraggedNodeRef $ \draggedNodeRef -> do
        isSelected <- view Model.isSelected <$> Store.get draggedNodeRef
        when (not isSelected) $ selectNodes [nodeId]
        nodes <- map _widget <$> selectedNodes
        let nodesPos = Map.fromList $ (view Model.nodeId &&& view Model.position) <$> nodes
        if snapped
            then do
                let snappedNodes = Map.map snap nodesPos
                Global.drag . Drag.history ?= DragHistory coord nodeId snappedNodes
                moveNodes snappedNodes
            else Global.drag . Drag.history ?= DragHistory coord nodeId nodesPos


handleMove :: Position -> Bool -> Command State ()
handleMove coord snapped = do
    -- factor <- use $ Global.camera . Camera.camera . Camera.factor
    dragHistory <- use $ Global.drag . Drag.history
    withJust dragHistory $ \(DragHistory mousePos draggedNodeId nodesPos) -> do
        let delta = coord ^. vector - mousePos ^. vector
            deltaWs = delta --Camera.scaledScreenToWorkspace factor delta
            shift' = if snapped
                        then case Map.lookup draggedNodeId nodesPos of
                            Just pos -> do
                                snap (moveByVector pos deltaWs) ^. vector - pos ^. vector
                            Nothing  -> deltaWs
                        else deltaWs
        moveNodes $ Map.map (flip moveByVector shift') nodesPos

moveNodes :: Map NodeId Position -> Command State ()
moveNodes nodesPos = do
    forM_ (Map.toList nodesPos) $ \(nodeId, pos) -> do
        Global.withNode nodeId $ mapM_ $ Store.modify_ $
            Model.position .~ pos
    updateConnectionsForNodes $ Map.keys nodesPos

stopDrag :: Position -> Command State ()
stopDrag coord = do
    dragHistory <- use $ Global.drag . Drag.history
    withJust dragHistory $ \(DragHistory start nodeId _) -> do
        Global.drag . Drag.history .= Nothing
        if (start /= coord)
            then do
                selected <- selectedNodes
                let nodesToUpdate = (\w -> (w ^. widget . Model.nodeId, w ^. widget . Model.position)) <$> selected
                updates <- forM nodesToUpdate $ \(wid, pos) -> do
                    Global.graph . Graph.nodesMap . ix wid . Node.position .= toTuple (pos ^. vector)
                    newMeta <- preuse $ Global.graph . Graph.nodesMap . ix wid . Node.nodeMeta
                    return $ (wid, ) <$> newMeta
                BatchCmd.updateNodeMeta $ catMaybes updates
                updateConnectionsForNodes $ fst <$> nodesToUpdate
            else selectNodes [nodeId]
