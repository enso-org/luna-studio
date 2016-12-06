{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
module Reactive.Plugins.Core.Action.Drag
    ( toAction
    ) where

import           React.Flux                        (mouseScreenX, mouseScreenY, mouseShiftKey)

import           Event.Event                       (Event (UI))
import           Event.UI                          (UIEvent (NodeEvent, AppEvent))
import qualified React.Event.Node                  as Node
import qualified React.Event.App                  as App
import           React.Store                       (widget, _widget, _ref)
import qualified React.Store                       as Store
import qualified React.Store.Node                  as Model
import qualified Reactive.Commands.Node            as Node
import           Utils.PreludePlus
import           Utils.Vector
import           Data.Map                          (Map)
import qualified Data.Map                          as Map

import           Object.Widget                     (WidgetFile, objectId, parent, widgetPosition)

import           Control.Arrow
import           Control.Monad.State               ()
import           Control.Monad.Trans.Maybe

import           Event.Event
import           Event.Keyboard                    hiding (Event)
import qualified Event.Mouse                       as Mouse

import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.Node              (NodeId)

import qualified Reactive.Commands.Batch           as BatchCmd
import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.Graph           (allNodes, updateConnectionsForNodes)
import           Reactive.Commands.Graph.Selection (selectNodes, selectedNodes)
import           Reactive.Commands.Node.Snap       (snap)
import qualified Reactive.Commands.UIRegistry      as UICmd
import qualified Reactive.State.Camera             as Camera
import           Reactive.State.Drag               (DragHistory (..))
import qualified Reactive.State.Drag               as Drag
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import qualified Reactive.State.UIRegistry         as UIRegistry


import qualified Object.Widget                     as Widget
import           Object.Widget.Label               (Label)

import qualified UI.Handlers.Node                  as Node




toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.MouseDown evt nodeId))) = Just $ do
    Global.getNode nodeId >>= mapM_ (Node.selectNode (mouseShiftKey evt))
    let pos = Vector2 (mouseScreenX evt) (mouseScreenY evt)
    startDrag nodeId pos $ not $ mouseShiftKey evt
toAction (UI (AppEvent  (App.MouseUp evt))) = Just $ do
    let pos = Vector2 (mouseScreenX evt) (mouseScreenY evt)
    stopDrag pos
toAction (UI (AppEvent  (App.MouseMove evt))) = Just $ do
    let pos = Vector2 (mouseScreenX evt) (mouseScreenY evt)
    handleMove pos $ not $ mouseShiftKey evt
toAction _ = Nothing


startDrag :: NodeId -> Vector2 Int -> Bool -> Command State ()
startDrag nodeId coord snapped = do
    nodes <- map _widget <$> selectedNodes
    let nodesPos = Map.fromList $ (view Model.nodeId &&& view Model.position) <$> nodes
    if snapped
        then do
            let snappedNodes = Map.map snap nodesPos
            Global.drag . Drag.history ?= DragHistory coord nodeId snappedNodes
            moveNodes snappedNodes
        else Global.drag . Drag.history ?= DragHistory coord nodeId nodesPos


handleMove :: Vector2 Int -> Bool -> Command State ()
handleMove coord snapped = do
    factor <- use $ Global.camera . Camera.camera . Camera.factor
    dragHistory <- use $ Global.drag . Drag.history
    withJust dragHistory $ \(DragHistory mousePos draggedNodeId nodesPos) -> do
        let delta = coord - mousePos
            deltaWs = Camera.scaledScreenToWorkspace factor delta
            shift = if snapped
                        then case Map.lookup draggedNodeId nodesPos of
                            Just pos -> snap (pos + deltaWs) - pos
                            Nothing  -> deltaWs
                        else deltaWs
        moveNodes $ Map.map (+shift) nodesPos

moveNodes :: Map NodeId (Vector2 Double) -> Command State ()
moveNodes nodesPos = do
    forM_ (Map.toList nodesPos) $ \(nodeId, pos) -> do
        Global.withNode nodeId $ mapM_ $ Store.modify_ $
            Model.position .~ pos
    updateConnectionsForNodes $ Map.keys nodesPos

stopDrag :: Vector2 Int -> Command State ()
stopDrag coord = do
    dragHistory <- use $ Global.drag . Drag.history
    withJust dragHistory $ \(DragHistory start nodeId _) -> do
        Global.drag . Drag.history .= Nothing
        when (start /= coord) $ do
            selected <- selectedNodes
            let nodesToUpdate = (\w -> (w ^. widget . Model.nodeId, w ^. widget . widgetPosition)) <$> selected
            updates <- forM nodesToUpdate $ \(wid, pos) -> do
                Global.graph . Graph.nodesMap . ix wid . Node.position .= toTuple pos
                newMeta <- preuse $ Global.graph . Graph.nodesMap . ix wid . Node.nodeMeta
                return $ (wid, ) <$> newMeta
            BatchCmd.updateNodeMeta $ catMaybes updates
            updateConnectionsForNodes $ fst <$> nodesToUpdate
