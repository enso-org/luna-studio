{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.MultiSelection
    ( startMultiSelection
    , updateMultiSelection
    , stopMultiSelection
    ) where

import           Data.Position                        (Position (Position), Vector2 (Vector2), fromTuple, x, y)
import           React.Flux                           (MouseEvent)

import           Empire.API.Data.Node                 (Node)
import qualified Empire.API.Data.Node                 as Node
import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Action.Graph             (modifySelectionHistory, selectNodes, selectedNodeIds, unselectAll)
import           Luna.Studio.Event.Mouse              (workspacePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor   as NodeEditor
import           Luna.Studio.React.Model.SelectionBox (SelectionBox (SelectionBox))
import           Luna.Studio.State.Action             (Action (begin, continue, end, update), MultiSelection (MultiSelection),
                                                       multiSelectionAction)
import qualified Luna.Studio.State.Action             as Action
import           Luna.Studio.State.Global             (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                       updateActionWithKey)
import qualified Luna.Studio.State.Global             as Global
import qualified Luna.Studio.State.Graph              as Graph


instance Action (Command State) MultiSelection where
    begin    = beginActionWithKey    multiSelectionAction
    continue = continueActionWithKey multiSelectionAction
    update   = updateActionWithKey   multiSelectionAction
    end      = stopMultiSelection

startMultiSelection :: MouseEvent -> Command State ()
startMultiSelection evt = do
    unselectAll
    coord <- workspacePosition evt
    begin $ MultiSelection coord

updateMultiSelection :: MouseEvent -> MultiSelection -> Command State ()
updateMultiSelection evt state = do
    let startPos = view Action.multiSelecectionStartPos state
    coord <- workspacePosition evt
    Global.modifyNodeEditor $ NodeEditor.selectionBox .= Just (SelectionBox startPos coord)
    updateSelection startPos coord

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

stopMultiSelection :: MultiSelection -> Command State ()
stopMultiSelection _ = do
    removeActionFromState multiSelectionAction
    Global.modifyNodeEditor $ NodeEditor.selectionBox .= Nothing
    nodeIds <- selectedNodeIds
    modifySelectionHistory nodeIds
