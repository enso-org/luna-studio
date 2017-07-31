module NodeEditor.Handler.Node where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Data.ScreenPosition                        (ScreenPosition)
import           NodeEditor.Action.Basic                    (collapseToFunction, enterNode, localSetPortDefault, removeSelectedNodes,
                                                             selectAll, setNodeExpression, setPortDefault, toggleSelect,
                                                             toggleSelectedNodesMode, toggleSelectedNodesUnfold)
import           NodeEditor.Action.Batch                    (autolayoutNodes)
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
handle (UI (AppEvent  (App.Movement              move   ))) = Just $ handleMovement move
handle (UI (AppEvent  (App.MouseUp               mevt   ))) = Just $ handleMouseUp   mevt
handle (UI (NodeEvent (Node.Enter                     nl))) = Just $ withJustM (getExpressionNode nl) enterNode
handle (UI (NodeEvent (Node.EditExpression            nl))) = Just $ Node.editExpression nl
handle (UI (NodeEvent (Node.EditName                  nl))) = Just $ Node.editName nl
handle (UI (NodeEvent (Node.Select               kevt nl))) = Just $ when (mouseCtrlKey kevt || mouseMetaKey kevt) $ toggleSelect nl
handle (UI (NodeEvent (Node.SetExpression             nl expr))) = Just $ setNodeExpression nl expr
handle (UI (NodeEvent (Node.PortEditString            portRef portDef)))    = Just $ void $ localSetPortDefault portRef portDef
handle (UI (NodeEvent (Node.PortApplyString      kevt portRef portDef)))    = Just $ when (Keys.withoutMods kevt Keys.enter) $ setPortDefault portRef portDef
handle (UI (NodeEvent (Node.PortSetPortDefault        portRef portDef)))    = Just $ setPortDefault portRef portDef
handle (UI (NodeEvent (Node.PortInitSlider       _    portRef sliderInit))) = Just $ PortControl.startMoveSlider portRef sliderInit
handle (UI (NodeEvent (Node.MouseEnter                nl))) = Just $ Node.setMouseOver nl
handle (UI (NodeEvent (Node.MouseLeave                nl))) = Just $ Node.unsetMouseOver nl
handle (UI (NodeEvent (Node.ShowFullError             nl))) = Just $ Node.showFullError nl
handle _ = Nothing


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.AutolayoutAllNodes      -> map (view Node.nodeLoc) <$> getExpressionNodes >>= autolayoutNodes
    Shortcut.AutolayoutSelectedNodes -> map (view Node.nodeLoc) <$> getSelectedNodes   >>= autolayoutNodes
    Shortcut.CollapseToFunction      -> collapseToFunction
    -- Shortcut.EditSelectedNodes       -> toggleSelectedNodesMode $ Node.Expanded Node.Editor --TEMPORARILY DISABLED
    Shortcut.ExpandSelectedNodes     -> toggleSelectedNodesMode $ Node.Expanded Node.Controls
    Shortcut.RemoveSelectedNodes     -> removeSelectedNodes
    Shortcut.SelectAll               -> selectAll
    Shortcut.UnfoldSelectedNodes     -> toggleSelectedNodesUnfold
    _                                -> return ()

handleMouseDown :: MouseEvent -> NodeLoc -> Command State ()
handleMouseDown evt nl =
    when shouldProceed $ workspacePosition evt >>= \pos -> Node.startNodeDrag pos nl shouldSnap where
        shouldProceed = Mouse.withoutMods evt Mouse.leftButton || Mouse.withShift evt Mouse.leftButton
        shouldSnap    = Mouse.withoutMods evt Mouse.leftButton

handleMouseMove :: MouseEvent -> Command State ()
handleMouseMove evt = if mouseButton evt == Mouse.leftButton
    then continue . Node.nodesDrag evt $ Mouse.withoutMods evt Mouse.leftButton
    else handleMouseUp evt

handleMovement :: ScreenPosition -> Command State ()
handleMovement = continue . PortControl.moveSlider

handleMouseUp :: MouseEvent -> Command State ()
handleMouseUp evt = do
    mousePosition evt >>= continue . PortControl.stopMoveSlider
    continue $ Node.handleNodeDragMouseUp evt
