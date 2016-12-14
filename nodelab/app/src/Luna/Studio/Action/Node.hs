module Luna.Studio.Action.Node where

import           React.Flux                        (KeyboardEvent, mouseCtrlKey, mouseMetaKey)

import           Event.Event                       (Event (UI))
import qualified Event.Keys                        as Keys
import           Event.UI                          (UIEvent (AppEvent, NodeEvent))
import qualified Luna.Studio.React.Event.App                   as App
import qualified Luna.Studio.React.Event.Node                  as Node
import           Luna.Studio.Commands.Command         (Command)
import           Luna.Studio.Commands.Graph.Selection (selectAll, toggleSelect, unselectAll)
import qualified Luna.Studio.Commands.Node            as Node
import           Luna.Studio.Commands.Node.Remove     as Node
import           Luna.Studio.State.Global             (State)
import qualified Luna.Studio.State.Global             as Global
import qualified Luna.Studio.State.Graph              as Graph
import           Luna.Studio.Prelude



toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.Enter          nodeId))) = Just $ mapM_ Node.tryEnter =<< preuse (Global.graph . Graph.nodesMap . ix nodeId)
toAction (UI (NodeEvent (Node.EditExpression nodeId))) = Just $ Node.editExpression nodeId
toAction (UI (NodeEvent (Node.Select     evt nodeId))) = Just $ when (mouseCtrlKey evt || mouseMetaKey evt) $ toggleSelect nodeId
toAction (UI (AppEvent (App.KeyDown e))) = Just $ handleKey e
toAction _   = Nothing



handleKey :: KeyboardEvent -> Command State ()
handleKey evt
    | Keys.withCtrl    evt Keys.a     = selectAll
    | Keys.withoutMods evt Keys.del   = Node.removeSelectedNodes
    | Keys.withoutMods evt Keys.esc   = unselectAll
    | Keys.withoutMods evt Keys.enter = Node.expandSelectedNodes
    | otherwise                       = return ()
