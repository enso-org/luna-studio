{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
module Reactive.Plugins.Core.Action.Drag
    ( toAction
    ) where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Map                          (Map)
import qualified Data.Map                          as Map

import           Object.Widget                     (WidgetFile, parent, widget, widgetPosition)

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
import           Reactive.Commands.Graph           (allNodes, nodeIdToWidgetId, updateConnectionsForNodes)
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
import qualified Object.Widget.Node                as Model

import qualified UI.Handlers.Node                  as Node



toAction :: Event -> Maybe (Command State ())
toAction (Mouse _ (Mouse.Event Mouse.Pressed  pos Mouse.LeftButton (KeyMods True False False False)  (Just _))) = Just $ startDrag  pos False
toAction (Mouse _ (Mouse.Event Mouse.Pressed  pos Mouse.LeftButton (KeyMods False False False False) (Just _))) = Just $ startDrag  pos True
toAction (Mouse _ (Mouse.Event Mouse.Moved    pos Mouse.LeftButton (KeyMods True False False False)  _))        = Just $ handleMove pos False
toAction (Mouse _ (Mouse.Event Mouse.Moved    pos Mouse.LeftButton (KeyMods False False False False) _))        = Just $ handleMove pos True
toAction (Mouse _ (Mouse.Event Mouse.Released pos Mouse.LeftButton _ _))                                        = Just $ stopDrag   pos
toAction _                                                                                                      = Nothing


getNodeIdUnderCursorFromNode :: Command UIRegistry.State (Maybe NodeId)
getNodeIdUnderCursorFromNode = runMaybeT $ do
    Just wid <- lift $ use UIRegistry.widgetOver
    Just w   <- lift (UIRegistry.lookupTypedM wid :: Command UIRegistry.State (Maybe (WidgetFile Model.Node)))
    return $ w ^. Widget.widget . Model.nodeId

getNodeIdUnderCursorFromLabel :: Command UIRegistry.State (Maybe NodeId)
getNodeIdUnderCursorFromLabel = runMaybeT act >>= return . join where
    act = do
        Just wid <- lift $ use UIRegistry.widgetOver
        Just w   <- lift (UIRegistry.lookupTypedM wid :: Command UIRegistry.State (Maybe (WidgetFile Label)))
        Just p   <- return $ w ^. parent
        Just n   <- lift (UIRegistry.lookupTypedM p :: Command UIRegistry.State (Maybe (WidgetFile Model.Node)))
        exId <- lift $ Node.expressionId p
        return $ if wid == exId
            then Just $ n ^. Widget.widget . Model.nodeId
            else Nothing

startDrag :: Vector2 Int -> Bool -> Command State ()
startDrag coord snapped = do
    nodeId'  <- zoom Global.uiRegistry getNodeIdUnderCursorFromNode
    nodeId'' <- zoom Global.uiRegistry getNodeIdUnderCursorFromLabel
    withJust (nodeId' <|> nodeId'') $ \nodeId -> do
        nodes' <- selectedNodes
        let nodes = view Widget.widget <$> nodes'
            nodesPos = Map.fromList $ (view Model.nodeId &&& view Model.position) <$> nodes
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
        widgetIdMay <- nodeIdToWidgetId nodeId
        withJust widgetIdMay $ \widgetId ->
            zoom Global.uiRegistry $ UICmd.move widgetId pos
    updateConnectionsForNodes $ Map.keys nodesPos

stopDrag :: Vector2 Int -> Command State ()
stopDrag coord = do
    dragHistory <- use $ Global.drag . Drag.history
    withJust dragHistory $ \(DragHistory start nodeId _) -> do
        Global.drag . Drag.history .= Nothing
        if start /= coord
            then do
                widgets <- allNodes

                let selected = filter (^. widget . Model.isSelected) widgets
                    nodesToUpdate = (\w -> (w ^. widget . Model.nodeId, w ^. widget . widgetPosition)) <$> selected

                updates <- forM nodesToUpdate $ \(wid, pos) -> do
                    Global.graph . Graph.nodesMap . ix wid . Node.position .= toTuple pos
                    newMeta <- preuse $ Global.graph . Graph.nodesMap . ix wid . Node.nodeMeta
                    return $ (wid, ) <$> newMeta
                BatchCmd.updateNodeMeta $ catMaybes updates
                updateConnectionsForNodes $ fst <$> nodesToUpdate
            else selectNodes [nodeId]
