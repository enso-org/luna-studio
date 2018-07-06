module NodeEditor.Handler.Node where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           LunaStudio.Data.Position                   (fromTuple)
import           LunaStudio.Data.ScreenPosition             (ScreenPosition)
import           NodeEditor.Action.Basic                    (collapseToFunction, enterNode, moveNode, removeSelectedNodes, selectAll,
                                                             selectNode, setNodeExpression, toggleSelect, toggleSelectedNodesMode,
                                                             toggleSelectedNodesUnfold, unselectAll)
import           NodeEditor.Action.Batch                    (autolayoutNodes)
import qualified NodeEditor.Action.Node                     as Node
import qualified NodeEditor.Action.Port                     as PortControl
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, getExpressionNodes, getSelectedNodes)
import           NodeEditor.Event.Event                     (Event (Shortcut, UI))
import           NodeEditor.Event.Event                     (Event (View))
import qualified NodeEditor.Event.Mouse                     as Mouse
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import           NodeEditor.Event.UI                        (UIEvent (AppEvent, NodeEvent, SidebarEvent))
import           NodeEditor.Event.View                      (ViewEvent)
import qualified NodeEditor.Event.View                      as View
import qualified NodeEditor.React.Event.App                 as App
import qualified NodeEditor.React.Event.Node                as Node hiding (nodeLoc)
import qualified NodeEditor.React.Event.Sidebar             as Sidebar
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node hiding (nodeLoc')
import           NodeEditor.State.Action                    (Action (continue))
import           NodeEditor.State.Global                    (State)
import           NodeEditor.State.Mouse                     (mousePosition, workspacePosition)
import           React.Flux                                 (MouseEvent, mouseButton, mouseCtrlKey, mouseMetaKey)


nodeEventPath :: View.Path
nodeEventPath = ["NodeEditor", "ExpressionNode"]

handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _))  = handleCommand command
handle (View evt)                             = handleViewEvent evt
handle (UI (NodeEvent evt))                   = handleNodeEvent evt
handle (UI (AppEvent (App.Movement  move)))   = Just $ handleMovement move
handle (UI (AppEvent (App.MouseUp   mevt)))   = Just $ handleMouseUp  mevt
handle (UI (AppEvent (App.MouseMove mevt _))) = Just $ handleMouseMove mevt
handle (UI (SidebarEvent (Sidebar.MouseMove mevt _ _)))
    = Just $ handleMouseMove mevt
handle _ = Nothing


handleViewEvent :: ViewEvent -> Maybe (Command State ())
handleViewEvent evt = let nl = View.getNodeLoc evt
    in case evt ^. View.base of
        View.NodeMove   e -> Just . moveNode nl . fromTuple $ e ^. View.position
        View.NodeSelect e -> Just
            $ if e ^. View.select then selectNode nl else unselectAll
        View.Mouse e -> if nodeEventPath /= evt ^. View.path
            then Nothing
            else case e ^. View.type_ of
                "dblclick"
                    -> Just $ withJustM (getExpressionNode nl) enterNode
                _   -> Nothing
        _ -> Nothing


handleCommand :: Shortcut.Command -> Maybe (Command State ())
handleCommand = \case
    Shortcut.SelectAll           -> Just selectAll
    Shortcut.CollapseToFunction  -> Just collapseToFunction
    Shortcut.RemoveSelectedNodes -> Just removeSelectedNodes
    Shortcut.UnfoldSelectedNodes -> Just toggleSelectedNodesUnfold
    Shortcut.ExpandSelectedNodes -> Just
        . toggleSelectedNodesMode $ Node.Expanded Node.Controls
    Shortcut.Accept -> Just $ continue PortControl.acceptEditTextPortControl
        >> PortControl.unfocusEditTextPortControl
    Shortcut.AutolayoutAllNodes
        -> Just $ map (view Node.nodeLoc) <$> getExpressionNodes
            >>= flip autolayoutNodes True
    Shortcut.AutolayoutSelectedNodes
        -> Just $ map (view Node.nodeLoc) <$> getSelectedNodes
            >>= flip autolayoutNodes False
    _   -> Nothing
    -- Shortcut.EditSelectedNodes       -> toggleSelectedNodesMode $ Node.Expanded Node.Editor --TEMPORARILY DISABLED

handleNodeEvent :: Node.Event -> Maybe (Command State ())
handleNodeEvent evt = let nl = evt ^. Node.nodeLoc'
    in case evt ^. Node.evtType of
        Node.MouseEnter     -> Just $ Node.handleMouseEnter nl
        Node.MouseLeave     -> Just $ Node.handleMouseLeave nl
        Node.MouseDown mevt -> Just $ handleMouseDown mevt nl
        Node.Enter          -> Just $ withJustM (getExpressionNode nl) enterNode
        Node.EditName       -> Just $ Node.editName nl
        Node.EditExpression -> Just $ Node.editExpression nl
        Node.SetExpression expr -> Just $ setNodeExpression nl expr
        Node.Select    kevt -> Just
            $ when (mouseCtrlKey kevt || mouseMetaKey kevt) $ toggleSelect nl


handleMouseDown :: MouseEvent -> NodeLoc -> Command State ()
handleMouseDown evt nl = when shouldProceed startNodeDrag where
    shouldProceed
        = Mouse.withoutMods evt Mouse.leftButton
        || Mouse.withShift evt Mouse.leftButton
    shouldSnap    = Mouse.withoutMods evt Mouse.leftButton
    startNodeDrag = workspacePosition evt
        >>= \pos -> Node.startNodeDrag pos nl shouldSnap


handleMouseMove :: MouseEvent -> Command State ()
handleMouseMove evt = if mouseButton evt == Mouse.leftButton
    then continue . flip Node.nodesDrag (Mouse.withoutMods evt Mouse.leftButton)
        =<< workspacePosition evt
    else handleMouseUp evt

handleMouseUp :: MouseEvent -> Command State ()
handleMouseUp evt = stopSlider >> nodeDragHandler where
    stopSlider = mousePosition evt
        >>= continue . PortControl.stopMoveSlider
    nodeDragHandler = workspacePosition evt
        >>= continue . Node.handleNodeDragMouseUp

handleMovement :: ScreenPosition -> Command State ()
handleMovement = continue . PortControl.moveSlider
