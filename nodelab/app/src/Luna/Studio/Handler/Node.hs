module Luna.Studio.Handler.Node where

import           React.Flux                     (KeyboardEvent, mouseCtrlKey, mouseMetaKey)

import           Empire.API.Data.Node           (NodeId)
import           Event.Event                    (Event (UI))
import           Event.UI                       (UIEvent (AppEvent, NodeEvent))
import qualified Luna.Studio.Action.Batch       as Batch
import           Luna.Studio.Action.Command     (Command)
import           Luna.Studio.Action.Graph       (selectAll, toggleSelect, unselectAll)
import qualified Luna.Studio.Action.Node        as Node
import qualified Luna.Studio.Action.PortControl as PortControl
import qualified Luna.Studio.Event.Keys         as Keys
import           Luna.Studio.Event.Mouse        (mousePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App    as App
import qualified Luna.Studio.React.Event.Node   as Node
import           Luna.Studio.State.Action       (Action (continue))
import           Luna.Studio.State.Global       (State)
import qualified Luna.Studio.State.Global       as Global
import qualified Luna.Studio.State.Graph        as Graph



toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.Enter            nodeId))) = Just $ mapM_ Node.tryEnter =<< preuse (Global.graph . Graph.nodesMap . ix nodeId)
toAction (UI (NodeEvent (Node.EditExpression   nodeId))) = Just $ Node.editExpression nodeId
toAction (UI (NodeEvent (Node.Select      kevt nodeId))) = Just $ when (mouseCtrlKey kevt || mouseMetaKey kevt) $ toggleSelect nodeId
toAction (UI (NodeEvent (Node.DisplayResultChanged flag nodeId))) = Just $ Node.visualizationsToggled nodeId flag
toAction (UI (NodeEvent (Node.NameEditStart    nodeId))) = Just $ Node.startEditName nodeId
toAction (UI (NodeEvent (Node.NameKeyDown kevt nodeId))) = Just $ handleKeyNode kevt nodeId
toAction (UI (NodeEvent (Node.NameChange   val nodeId))) = Just $ Node.editName nodeId val
toAction (UI (NodeEvent (Node.PortEditString       portRef defaultValue))) = Just $ PortControl.setPortDefault portRef defaultValue
toAction (UI (NodeEvent (Node.PortApplyString kevt portRef defaultValue))) = Just $ when (Keys.withoutMods kevt Keys.enter) $
                                                                                        Batch.setDefaultValue portRef defaultValue
toAction (UI (NodeEvent (Node.PortSetDefaultValue portRef defaultValue))) = Just $ Batch.setDefaultValue portRef defaultValue
--TODO[react]: Findout if we need workspacePosition here
toAction (UI (NodeEvent (Node.PortInitSlider mevt portRef sliderInit)))   = Just $ PortControl.startMoveSlider portRef (mousePosition mevt) sliderInit
toAction (UI (AppEvent  (App.KeyDown   kevt))) = Just $ handleKeyApp kevt
--TODO[react]: Findout if we need workspacePosition here
toAction (UI (AppEvent  (App.MouseMove mevt))) = Just $ continue $ PortControl.moveSlider     $ mousePosition mevt
--TODO[react]: Findout if we need workspacePosition here
toAction (UI (AppEvent  (App.MouseUp   mevt))) = Just $ continue $ PortControl.stopMoveSlider $ mousePosition mevt
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
