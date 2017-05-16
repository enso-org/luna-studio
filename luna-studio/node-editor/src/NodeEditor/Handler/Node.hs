module NodeEditor.Handler.Node where

import           Common.Prelude
import           NodeEditor.Action.Basic                    (enterNode, localSetPortDefault, localToggleVisualizations, removeSelectedNodes,
                                                             selectAll, setNodeExpression, setPortDefault, toggleSelect,
                                                             toggleSelectedNodesMode, toggleSelectedNodesUnfold, unselectAll)
import           NodeEditor.Action.Batch                    (autolayoutNodes)
import           NodeEditor.Action.Command                  (Command)
import qualified NodeEditor.Action.Node                     as Node
import qualified NodeEditor.Action.Port                     as PortControl
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, getExpressionNodes, getSelectedNodes)
import           NodeEditor.Event.Event                     (Event (Shortcut, UI))
import qualified NodeEditor.Event.Keys                      as Keys
import           NodeEditor.Event.Mouse                     (mousePosition, workspacePosition)
import qualified NodeEditor.Event.Mouse                     as Mouse
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import           NodeEditor.Event.UI                        (UIEvent (AppEvent, NodeEvent))
import qualified NodeEditor.React.Event.App                 as App
import qualified NodeEditor.React.Event.Node                as Node
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.State.Action                    (Action (continue))
import           NodeEditor.State.Global                    (State)
import           React.Flux                                 (MouseEvent, mouseButton, mouseCtrlKey, mouseMetaKey)


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _)) = Just $ handleCommand command
handle (UI (NodeEvent (Node.MouseDown            mevt nl))) = Just $ handleMouseDown mevt nl
handle (UI (AppEvent  (App.MouseMove             mevt _ ))) = Just $ handleMouseMove mevt
handle (UI (AppEvent  (App.MouseUp               mevt   ))) = Just $ handleMouseUp   mevt
handle (UI (NodeEvent (Node.Enter                     nl))) = Just $ withJustM (getExpressionNode nl) enterNode
handle (UI (NodeEvent (Node.EditExpression            nl))) = Just $ Node.editExpression nl
handle (UI (NodeEvent (Node.EditName                  nl))) = Just $ Node.editName nl
handle (UI (NodeEvent (Node.Select               kevt nl))) = Just $ when (mouseCtrlKey kevt || mouseMetaKey kevt) $ toggleSelect nl
handle (UI (NodeEvent (Node.DisplayResultChanged flag nl))) = Just $ localToggleVisualizations nl flag
handle (UI (NodeEvent (Node.SetExpression             nl expr))) = Just $ setNodeExpression nl expr
handle (UI (NodeEvent (Node.PortEditString            portRef portDef)))    = Just $ void $ localSetPortDefault portRef portDef
handle (UI (NodeEvent (Node.PortApplyString      kevt portRef portDef)))    = Just $ when (Keys.withoutMods kevt Keys.enter) $ setPortDefault portRef portDef
handle (UI (NodeEvent (Node.PortSetPortDefault        portRef portDef)))    = Just $ setPortDefault portRef portDef
handle (UI (NodeEvent (Node.PortInitSlider       mevt portRef sliderInit))) = Just $ mousePosition mevt >>= \mousePos -> PortControl.startMoveSlider portRef mousePos sliderInit
handle _ = Nothing


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.SelectAll               -> selectAll
    Shortcut.RemoveSelectedNodes     -> removeSelectedNodes
    Shortcut.Cancel                  -> unselectAll
    Shortcut.ExpandSelectedNodes     -> toggleSelectedNodesMode $ Node.Expanded Node.Controls
    Shortcut.EditSelectedNodes       -> toggleSelectedNodesMode $ Node.Expanded Node.Editor
    Shortcut.UnfoldSelectedNodes     -> toggleSelectedNodesUnfold
    Shortcut.AutolayoutSelectedNodes -> map (view Node.nodeLoc) <$> getSelectedNodes   >>= autolayoutNodes
    Shortcut.AutolayoutAllNodes      -> map (view Node.nodeLoc) <$> getExpressionNodes >>= autolayoutNodes
    _                                -> return ()

handleMouseDown :: MouseEvent -> NodeLoc -> Command State ()
handleMouseDown evt nl =
    when shouldProceed $ workspacePosition evt >>= \pos -> Node.startNodeDrag pos nl shouldSnap where
        shouldProceed = Mouse.withoutMods evt Mouse.leftButton || Mouse.withShift evt Mouse.leftButton
        shouldSnap    = Mouse.withoutMods evt Mouse.leftButton

handleMouseMove :: MouseEvent -> Command State ()
handleMouseMove evt = if mouseButton evt == Mouse.leftButton
    then do
        mousePosition evt >>= continue . PortControl.moveSlider
        continue . Node.nodesDrag evt $ Mouse.withoutMods evt Mouse.leftButton
    else handleMouseUp evt

handleMouseUp :: MouseEvent -> Command State ()
handleMouseUp evt = do
    mousePosition evt >>= continue . PortControl.stopMoveSlider
    continue $ Node.handleNodeDragMouseUp evt
