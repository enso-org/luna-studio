{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.MultiSelection
    ( startMultiSelection
    , updateMultiSelection
    , stopMultiSelection
    ) where

import           Data.Position                        (Position (Position), Vector2 (Vector2), x, y)
import           React.Flux                           (MouseEvent)

import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Action.Graph             (allNodes, modifySelectionHistory, selectNodes, selectedNodeIds, unselectAll)
import           Luna.Studio.Event.Mouse              (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node         (Node)
import qualified Luna.Studio.React.Model.Node         as Node
import qualified Luna.Studio.React.Model.NodeEditor   as NodeEditor
import           Luna.Studio.React.Model.SelectionBox (SelectionBox (SelectionBox))
import           Luna.Studio.State.Action             (Action (begin, continue, end, update), MultiSelection (MultiSelection),
                                                       multiSelectionAction)
import qualified Luna.Studio.State.Action             as Action
import           Luna.Studio.State.Global             (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                       updateActionWithKey)
import qualified Luna.Studio.State.Global             as Global


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
    where pos = node ^. Node.position

updateSelection :: Position -> Position -> Command State ()
updateSelection start act = do
    let leftTop     = Position (Vector2 (min (start ^. x) (act ^. x)) (max (start ^. y) (act ^. y)))
        rightBottom = Position (Vector2 (max (start ^. x) (act ^. x)) (min (start ^. y) (act ^. y)))
    nodeIds <- map Node._nodeId . filter (inRect leftTop rightBottom) <$> allNodes
    selectNodes nodeIds

stopMultiSelection :: MultiSelection -> Command State ()
stopMultiSelection _ = do
    removeActionFromState multiSelectionAction
    Global.modifyNodeEditor $ NodeEditor.selectionBox .= Nothing
    nodeIds <- selectedNodeIds
    modifySelectionHistory nodeIds
