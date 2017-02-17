{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Edge where

import           Empire.API.Data.Node       (NodeId)
import qualified Luna.Studio.Action.Batch   as Batch
import           Luna.Studio.Action.Command (Command)
import qualified Luna.Studio.Action.Connect as Connect
import           Luna.Studio.Prelude
import           Luna.Studio.State.Action   (Action (begin, continue, end, update), Connect, PortDrag (PortDrag), portDragAction)
import qualified Luna.Studio.State.Action   as Action
import           Luna.Studio.State.Global   (State, beginActionWithKey, continueActionWithKey, removeActionFromState, updateActionWithKey)
import           React.Flux                 (MouseEvent)

instance Action (Command State) PortDrag where
    begin    = beginActionWithKey    portDragAction
    continue = continueActionWithKey portDragAction
    update   = updateActionWithKey   portDragAction
    end _    = removeActionFromState portDragAction

connectOrPortDrag :: MouseEvent -> Connect -> Command State ()
connectOrPortDrag evt connect = if (connect ^. Action.connectIsConnModified) then
         continue $ Connect.handleMove evt
    else begin $ PortDrag (connect ^. Action.connectStartPos) (connect ^. Action.connectSourcePort) (connect ^. Action.connectMode)

restoreConnect :: MouseEvent -> PortDrag -> Command State ()
restoreConnect evt portDrag = do
    Connect.startConnecting (portDrag ^. Action.portDragStartPos) (portDrag ^. Action.portDragPortRef) Nothing (portDrag ^. Action.portDragMode)
    continue $ Connect.handleMove evt

removePort :: PortDrag -> Command State ()
removePort portDrag = Batch.removePort (portDrag ^. Action.portDragPortRef) >> end portDrag

addPort :: NodeId -> Command State ()
addPort n = Batch.addPort n
