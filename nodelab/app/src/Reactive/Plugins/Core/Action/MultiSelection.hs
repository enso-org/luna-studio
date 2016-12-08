{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Plugins.Core.Action.MultiSelection where

import           React.Flux                        (mousePageX, mousePageY)
import           Utils.PreludePlus
import           Utils.Vector                      (Vector2 (..), fromTuple, x, y)

import           Empire.API.Data.Node              (Node)
import qualified Empire.API.Data.Node              as Node
import           Event.Event                       (Event (UI))
import           Event.UI                          (UIEvent (AppEvent, NodeEditorEvent))
import qualified Object.Widget.Node                as NodeModel
import qualified React.Event.App                   as App
import qualified React.Event.NodeEditor            as NodeEditor
import           React.Store                       (widget)
import qualified React.Store                       as Store
import           React.Store.SelectionBox          (SelectionBox (SelectionBox))
import qualified React.Store.SelectionBox          as SelectionBox
import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.Graph.Selection (focusSelectedNode, modifySelectionHistory, selectNodes, selectedNodes, unselectAll)
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import           Reactive.State.MultiSelection     (DragHistory (..))
import qualified Reactive.State.MultiSelection     as MultiSelection



toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEditorEvent (NodeEditor.MouseDown evt))) = Just $ do
    let pos = Vector2 (mousePageX evt) (mousePageY evt)
    startDrag pos
toAction (UI (AppEvent  (App.MouseUp   _  ))) = Just $ stopDrag
toAction (UI (AppEvent  (App.MouseMove evt))) = Just $ do
    let pos = Vector2 (mousePageX evt) (mousePageY evt)
    handleMove pos
--TODO[react] implement
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Press 'A'   _)) = Just selectAll
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Down  '\27' _)) = Just unselectAll
toAction _ = Nothing


startDrag :: Vector2 Int -> Command State ()
startDrag coord = do
    Global.multiSelection . MultiSelection.history ?= DragHistory coord coord
    unselectAll

handleMove :: Vector2 Int -> Command State ()
handleMove coord = do
    dragHistory <- use $ Global.multiSelection . MultiSelection.history
    case dragHistory of
        Nothing                          -> return ()
        Just (DragHistory start _current) -> do
            Global.multiSelection . MultiSelection.history ?= DragHistory start coord
            updateSelection start coord
            drawSelectionBox start coord

inRect :: Vector2 Int -> Vector2 Int -> Node -> Bool
inRect leftTop rightBottom node = pos ^. x >= leftTop ^. x
                               && pos ^. x <= rightBottom ^. x
                               && pos ^. y <= leftTop ^. y
                               && pos ^. y >= rightBottom ^. y
    where pos = fmap round $ fromTuple $ node ^. Node.position

updateSelection :: Vector2 Int -> Vector2 Int -> Command State ()
updateSelection start end = do
    let leftTop     = Vector2 (min (start ^. x) (end ^. x)) (max (start ^. y) (end ^. y))
        rightBottom = Vector2 (max (start ^. x) (end ^. x)) (min (start ^. y) (end ^. y))
    nodeIds <- map Node._nodeId . filter (inRect leftTop rightBottom) <$> use (Global.graph . Graph.nodes)
    selectNodes nodeIds

drawSelectionBox :: Vector2 Int -> Vector2 Int -> Command State ()
drawSelectionBox start end = do
    Global.withSelectionBox $ Store.modify_ $ const $ SelectionBox True start end

hideSelectionBox :: Command State ()
hideSelectionBox = Global.withSelectionBox $ Store.modify_ $ SelectionBox.visible .~ False

stopDrag :: Command State ()
stopDrag = do
    wasSelecting <- uses (Global.multiSelection . MultiSelection.history) isJust
    when wasSelecting $ do
        Global.multiSelection . MultiSelection.history .= Nothing
        hideSelectionBox
        focusSelectedNode
        selectedWidgets <- selectedNodes
        let selectedNodesIds = map (^. widget . NodeModel.nodeId) selectedWidgets
        modifySelectionHistory selectedNodesIds
