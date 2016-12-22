module Luna.Studio.Action.Node where

import           React.Flux                           (KeyboardEvent, mouseCtrlKey, mouseMetaKey)

import           Empire.API.Data.Node                 (NodeId)
import           Event.Event                          (Event (UI))
import           Event.UI                             (UIEvent (AppEvent, NodeEvent))
import           Luna.Studio.Commands.Command         (Command)
import           Luna.Studio.Commands.Graph.Selection (selectAll, toggleSelect, unselectAll)
import qualified Luna.Studio.Commands.Node            as Node
import           Luna.Studio.Commands.Node.Remove     as Node
import qualified Luna.Studio.Event.Keys               as Keys
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App          as App
import qualified Luna.Studio.React.Event.Node         as Node
import           Luna.Studio.State.Global             (State)
import qualified Luna.Studio.State.Global             as Global
import qualified Luna.Studio.State.Graph              as Graph



toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.Enter            nodeId))) = Just $ mapM_ Node.tryEnter =<< preuse (Global.graph . Graph.nodesMap . ix nodeId)
toAction (UI (NodeEvent (Node.EditExpression   nodeId))) = Just $ Node.editExpression nodeId
toAction (UI (NodeEvent (Node.Select      kevt nodeId))) = Just $ when (mouseCtrlKey kevt || mouseMetaKey kevt) $ toggleSelect nodeId
toAction (UI (NodeEvent (Node.DisplayResultChanged flag nodeId))) = Just $ Node.visualizationsToggled nodeId flag
toAction (UI (NodeEvent (Node.NameEditStart    nodeId))) = Just $ Node.startEditName nodeId
toAction (UI (NodeEvent (Node.NameKeyDown kevt nodeId))) = Just $ handleKeyNode kevt nodeId
toAction (UI (NodeEvent (Node.NameChange   val nodeId))) = Just $ Node.editName nodeId val
toAction (UI (AppEvent (App.KeyDown e))) = Just $ handleKeyApp e
toAction _   = Nothing


handleKeyNode :: KeyboardEvent -> NodeId -> Command State ()
handleKeyNode kevt nodeId
    | Keys.withoutMods kevt Keys.enter = Node.applyName   nodeId
    | Keys.withoutMods kevt Keys.esc   = Node.discardName nodeId
    | otherwise                        = return ()


handleKeyApp :: KeyboardEvent -> Command State ()
handleKeyApp kevt
    | Keys.withCtrl    kevt Keys.a     = selectAll
    | Keys.withoutMods kevt Keys.del   = Node.removeSelectedNodes
    | Keys.withoutMods kevt Keys.esc   = unselectAll
    | Keys.withoutMods kevt Keys.enter = Node.expandSelectedNodes
    | otherwise                        = return ()
