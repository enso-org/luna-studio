module Reactive.Plugins.Core.Action.Node where

import           React.Flux                        (KeyboardEvent, mouseCtrlKey, mouseMetaKey)

import           Event.Event                       (Event (UI))
import qualified Event.Keys                        as Keys
import           Event.UI                          (UIEvent (AppEvent, NodeEvent))
import qualified React.Event.App                   as App
import qualified React.Store.Node                  as Node
import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.Graph.Selection (selectAll, toggleSelect, unselectAll)
import qualified Reactive.Commands.Node            as Node
import           Reactive.Commands.Node.Remove     as Node
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import           Utils.PreludePlus



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
