{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Studio.Action.MultiSelection where

import           Luna.Studio.Data.Vector              (Position (Position), Vector2 (Vector2), fromTuple, x, y)
import           Luna.Studio.Prelude

import           Empire.API.Data.Node                 (Node)
import qualified Empire.API.Data.Node                 as Node
import           Event.Event                          (Event (UI))
import           Event.UI                             (UIEvent (AppEvent, NodeEditorEvent))
import           Luna.Studio.Commands.Command         (Command)
import           Luna.Studio.Commands.Graph.Selection (focusSelectedNode, modifySelectionHistory, selectNodes, selectedNodes, unselectAll)
import           Luna.Studio.Event.Mouse              (workspacePosition)
import qualified Luna.Studio.Event.Mouse              as Mouse
import qualified Luna.Studio.React.Event.App          as App
import qualified Luna.Studio.React.Event.NodeEditor   as NodeEditor
import qualified Luna.Studio.React.Model.Node         as NodeModel
import           Luna.Studio.React.Model.SelectionBox (SelectionBox (SelectionBox))
import qualified Luna.Studio.React.Model.SelectionBox as SelectionBox
import           Luna.Studio.React.Store              (widget)
import qualified Luna.Studio.React.Store              as Store
import           Luna.Studio.State.Global             (State)
import qualified Luna.Studio.State.Global             as Global
import qualified Luna.Studio.State.Graph              as Graph
import           Luna.Studio.State.MultiSelection     (DragHistory (..))
import qualified Luna.Studio.State.MultiSelection     as MultiSelection
import           React.Flux                           (MouseEvent)


toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEditorEvent (NodeEditor.MouseDown evt))) = Just $ when shouldProceed $ startDrag evt where
    shouldProceed = Mouse.withoutMods evt Mouse.leftButton
toAction (UI (AppEvent  (App.MouseUp   _  )))              = Just $ stopDrag
toAction (UI (AppEvent  (App.MouseMove evt)))              = Just $ handleMove evt
toAction _                                                 = Nothing


startDrag :: MouseEvent -> Command State ()
startDrag evt = do
    coord <- workspacePosition evt
    Global.multiSelection . MultiSelection.history ?= DragHistory coord coord
    unselectAll

handleMove :: MouseEvent -> Command State ()
handleMove evt = do
    dragHistory <- use $ Global.multiSelection . MultiSelection.history
    case dragHistory of
        Nothing                          -> return ()
        Just (DragHistory start _current) -> do
            coord <- workspacePosition evt
            Global.multiSelection . MultiSelection.history ?= DragHistory start coord
            updateSelection start coord
            drawSelectionBox start coord

inRect :: Position -> Position -> Node -> Bool
inRect leftTop rightBottom node = pos ^. x >= leftTop ^. x
                               && pos ^. x <= rightBottom ^. x
                               && pos ^. y <= leftTop ^. y
                               && pos ^. y >= rightBottom ^. y
    where pos = Position (fromTuple $ node ^. Node.position)

updateSelection :: Position -> Position -> Command State ()
updateSelection start end = do
    let leftTop     = Position (Vector2 (min (start ^. x) (end ^. x)) (max (start ^. y) (end ^. y)))
        rightBottom = Position (Vector2 (max (start ^. x) (end ^. x)) (min (start ^. y) (end ^. y)))
    nodeIds <- map Node._nodeId . filter (inRect leftTop rightBottom) <$> use (Global.graph . Graph.nodes)
    selectNodes nodeIds

drawSelectionBox :: Position -> Position -> Command State ()
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
