module Luna.Studio.Handler.Node where

import           React.Flux                     (KeyboardEvent, mouseCtrlKey, mouseMetaKey)

import           Empire.API.Data.Node           (NodeId)
import qualified Luna.Studio.Action.Batch       as Batch
import           Luna.Studio.Action.Command     (Command)
import           Luna.Studio.Action.Graph       (selectAll, toggleSelect, unselectAll)
import qualified Luna.Studio.Action.Node        as Node
import qualified Luna.Studio.Action.PortControl as PortControl
import           Luna.Studio.Event.Event        (Event (Shortcut, UI))
import qualified Luna.Studio.Event.Keys         as Keys
import           Luna.Studio.Event.Mouse        (mousePosition)
import qualified Luna.Studio.Event.Mouse        as Mouse
import           Luna.Studio.Event.Shortcut     (ShortcutEvent (..))
import           Luna.Studio.Event.UI           (UIEvent (AppEvent, NodeEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App    as App
import qualified Luna.Studio.React.Event.Node   as Node
import           Luna.Studio.State.Action       (Action (continue))
import           Luna.Studio.State.Global       (State)
import qualified Luna.Studio.State.Global       as Global
import qualified Luna.Studio.State.Graph        as Graph



handle :: Event -> Maybe (Command State ())
handle (Shortcut shortcut)                             = Just $ handleShortcut shortcut
handle (UI (NodeEvent (Node.MouseDown evt nodeId))) = Just $ when shouldProceed $ Node.startNodeDrag nodeId evt shouldSnap  where
    shouldProceed = Mouse.withoutMods evt Mouse.leftButton || Mouse.withShift evt Mouse.leftButton
    shouldSnap    = Mouse.withoutMods evt Mouse.leftButton
handle (UI (NodeEvent (Node.Enter            nodeId))) = Just $ mapM_ Node.tryEnter =<< preuse (Global.graph . Graph.nodesMap . ix nodeId)
handle (UI (NodeEvent (Node.EditExpression   nodeId))) = Just $ Node.editExpression nodeId
handle (UI (NodeEvent (Node.Select      kevt nodeId))) = Just $ when (mouseCtrlKey kevt || mouseMetaKey kevt) $ toggleSelect nodeId
handle (UI (NodeEvent (Node.DisplayResultChanged flag nodeId))) = Just $ Node.visualizationsToggled nodeId flag
handle (UI (NodeEvent (Node.NameEditStart    nodeId))) = Just $ Node.startEditName nodeId
handle (UI (NodeEvent (Node.NameKeyDown kevt nodeId))) = Just $ handleKeyNode kevt nodeId
handle (UI (NodeEvent (Node.NameChange   val nodeId))) = Just $ Node.editName nodeId val
handle (UI (NodeEvent (Node.PortEditString       portRef defaultValue))) = Just $ PortControl.setPortDefault portRef defaultValue
handle (UI (NodeEvent (Node.PortApplyString kevt portRef defaultValue))) = Just $ when (Keys.withoutMods kevt Keys.enter) $
                                                                                        Batch.setDefaultValue portRef defaultValue
handle (UI (NodeEvent (Node.PortSetDefaultValue portRef defaultValue))) = Just $ Batch.setDefaultValue portRef defaultValue
--TODO[react]: Findout if we need workspacePosition here
handle (UI (NodeEvent (Node.PortInitSlider mevt portRef sliderInit)))   = Just $ PortControl.startMoveSlider portRef (mousePosition mevt) sliderInit
--TODO[react]: Findout if we need workspacePosition here
handle (UI (AppEvent  (App.MouseMove mevt))) = Just $ (continue $ PortControl.moveSlider $ mousePosition mevt) >> (continue $ Node.nodeDrag mevt shouldSnap) where
    shouldSnap = Mouse.withoutMods mevt Mouse.leftButton
--TODO[react]: Findout if we need workspacePosition here
handle (UI (AppEvent  (App.MouseUp   mevt))) = Just $ (continue $ PortControl.stopMoveSlider $ mousePosition mevt) >> (continue $ Node.stopNodeDrag mevt)
handle _   = Nothing


handleKeyNode :: KeyboardEvent -> NodeId -> Command State ()
handleKeyNode kevt nodeId
    | Keys.withoutMods kevt Keys.enter = Node.applyName   nodeId
    | Keys.withoutMods kevt Keys.esc   = Node.discardName nodeId
    | otherwise                        = return ()


handleShortcut :: ShortcutEvent -> Command State ()
handleShortcut = \case
    SelectAll           -> selectAll
    RemoveSelectedNodes -> Node.removeSelectedNodes
    UnselectAll         -> unselectAll
    ExpandSelectedNodes -> Node.expandSelectedNodes
    _                   -> return ()
