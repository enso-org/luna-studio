module Node.Editor.Handler.Node where

import           Node.Editor.Action.Autolayout               (autolayoutAllNodes, autolayoutSelectedNodes)
import           Node.Editor.Action.Basic                    (enterNode, localSetPortDefault, localToggleVisualizations,
                                                              removeSelectedNodes, selectAll, setNodeCode, setPortDefault, toggleSelect,
                                                              toggleSelectedNodesMode, toggleSelectedNodesUnfold, unselectAll)
import           Node.Editor.Action.Command                  (Command)
import qualified Node.Editor.Action.Node                     as Node
import qualified Node.Editor.Action.Port                     as PortControl
import           Node.Editor.Action.State.NodeEditor         (getExpressionNode)
import           Node.Editor.Event.Event                     (Event (Shortcut, UI))
import qualified Node.Editor.Event.Keys                      as Keys
import           Node.Editor.Event.Mouse                     (mousePosition, workspacePosition)
import qualified Node.Editor.Event.Mouse                     as Mouse
import qualified Node.Editor.Event.Shortcut                  as Shortcut
import           Node.Editor.Event.UI                        (UIEvent (AppEvent, NodeEvent))
import           Luna.Prelude
import qualified Node.Editor.React.Event.App                 as App
import qualified Node.Editor.React.Event.Node                as Node
import           Node.Editor.React.Model.Node.ExpressionNode (NodeLoc)
import qualified Node.Editor.React.Model.Node.ExpressionNode as Node
import           Node.Editor.State.Action                    (Action (continue))
import           Node.Editor.State.Global                    (State)
import           React.Flux                                  (MouseEvent, mouseButton, mouseCtrlKey, mouseMetaKey)


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _)) = Just $ handleCommand command
handle (UI (NodeEvent (Node.MouseDown            mevt nl))) = Just $ handleMouseDown mevt nl
handle (UI (AppEvent  (App.MouseMove             mevt _ ))) = Just $ handleMouseMove mevt
handle (UI (AppEvent  (App.MouseUp               mevt   ))) = Just $ handleMouseUp   mevt
handle (UI (NodeEvent (Node.Enter                     nl))) = Just $ withJustM (getExpressionNode nl) enterNode
handle (UI (NodeEvent (Node.EditExpression            nl))) = Just $ Node.editExpression nl
handle (UI (NodeEvent (Node.Select               kevt nl))) = Just $ when (mouseCtrlKey kevt || mouseMetaKey kevt) $ toggleSelect nl
handle (UI (NodeEvent (Node.DisplayResultChanged flag nl))) = Just $ localToggleVisualizations nl flag
handle (UI (NodeEvent (Node.NameEditStart             nl))) = Just $ Node.startEditName nl
handle (UI (NodeEvent (Node.NameEditDiscard           nl))) = Just $ Node.discardName nl
handle (UI (NodeEvent (Node.NameEditApply             nl val)))  = Just $ Node.applyName nl val
handle (UI (NodeEvent (Node.SetCode                   nl code))) = Just $ setNodeCode nl code
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
    Shortcut.AutolayoutSelectedNodes -> autolayoutSelectedNodes
    Shortcut.AutolayoutAllNodes      -> autolayoutAllNodes
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
