{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.MultiSelection
    ( startMultiSelection
    , updateMultiSelection
    , endMultiSelection
    ) where

import           Empire.API.Data.Node                 (Node)
import qualified Empire.API.Data.Node                 as Node
import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Action.Graph             (focusSelectedNode, modifySelectionHistory, selectNodes, selectedNodes, unselectAll)
import           Luna.Studio.Data.Vector              (Position (Position), Vector2 (Vector2), fromTuple, x, y)
import           Luna.Studio.Event.Mouse              (workspacePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node         as NodeModel
import qualified Luna.Studio.React.Model.NodeEditor   as NodeEditor
import           Luna.Studio.React.Model.SelectionBox (SelectionBox (SelectionBox))
import qualified Luna.Studio.React.Model.SelectionBox as SelectionBox
import           Luna.Studio.State.Action             (Action (MultiSelection))
import           Luna.Studio.State.Global             (State)
import qualified Luna.Studio.State.Global             as Global
import qualified Luna.Studio.State.Graph              as Graph
import qualified Luna.Studio.State.MultiSelection     as MultiSelection
import           Luna.Studio.State.StatefulAction     (StatefulAction (exit, matchState, pack, start, update))
import           React.Flux                           (MouseEvent)


instance StatefulAction MultiSelection.State where
    pack = MultiSelection
    matchState (MultiSelection state) = Just state
    matchState _ = Nothing
    exit _ = endMultiSelection

startMultiSelection :: MouseEvent -> Command State ()
startMultiSelection evt = do
    unselectAll
    coord <- workspacePosition evt
    start $ MultiSelection.State coord coord

updateMultiSelection :: MouseEvent -> MultiSelection.State -> Command State ()
updateMultiSelection evt state = do
    let startPos = view MultiSelection.dragStartPos state
    coord <- workspacePosition evt
    update $ MultiSelection.State startPos coord
    updateSelection startPos coord
    drawSelectionBox startPos coord

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
drawSelectionBox start end =
    Global.modifyNodeEditor $
        NodeEditor.selectionBox .= SelectionBox True start end


hideSelectionBox :: Command State ()
hideSelectionBox = Global.modifySelectionBox $ SelectionBox.visible .= False

endMultiSelection :: Command State ()
endMultiSelection = do
    mayPerformedAction <- use $ Global.performedAction
    withJust mayPerformedAction $ \performedAction -> case performedAction of
        MultiSelection _ -> Global.performedAction .= Nothing
        _                -> return ()
    hideSelectionBox
    focusSelectedNode
    selectedNodesIds <- map (^. NodeModel.nodeId) <$> selectedNodes
    modifySelectionHistory selectedNodesIds
