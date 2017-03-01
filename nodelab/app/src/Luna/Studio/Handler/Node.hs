module Luna.Studio.Handler.Node where

import           React.Flux                   (mouseCtrlKey, mouseMetaKey)

import qualified Luna.Studio.Action.Batch     as Batch
import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Action.Graph     (selectAll, toggleSelect, unselectAll)
import qualified Luna.Studio.Action.Node      as Node
import qualified Luna.Studio.Action.Port      as PortControl
import           Luna.Studio.Event.Event      (Event (Shortcut, UI))
import qualified Luna.Studio.Event.Keys       as Keys
import           Luna.Studio.Event.Mouse      (mousePosition, workspacePosition)
import qualified Luna.Studio.Event.Mouse      as Mouse
import qualified Luna.Studio.Event.Shortcut   as Shortcut
import           Luna.Studio.Event.UI         (UIEvent (AppEvent, NodeEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App  as App
import qualified Luna.Studio.React.Event.Node as Node
import qualified Luna.Studio.React.Model.Node as Node
import           Luna.Studio.State.Action     (Action (continue))
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.Graph      as Graph


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _))        = Just $ handleCommand command
handle (UI (NodeEvent (Node.MouseDown evt nodeId))) = Just $ do
    let shouldProceed = Mouse.withoutMods evt Mouse.leftButton || Mouse.withShift evt Mouse.leftButton
        shouldSnap    = Mouse.withoutMods evt Mouse.leftButton
    pos <- workspacePosition evt
    when shouldProceed $ Node.startNodeDrag pos nodeId shouldSnap
handle (UI (NodeEvent (Node.Enter            nodeId))) = Just $ mapM_ Node.tryEnter =<< preuse (Global.graph . Graph.nodesMap . ix nodeId)
handle (UI (NodeEvent (Node.EditExpression   nodeId))) = Just $ Node.editExpression nodeId
handle (UI (NodeEvent (Node.Select      kevt nodeId))) = Just $ when (mouseCtrlKey kevt || mouseMetaKey kevt) $ toggleSelect nodeId
handle (UI (NodeEvent (Node.DisplayResultChanged flag nodeId))) = Just $ Node.visualizationsToggled nodeId flag
handle (UI (NodeEvent (Node.NameEditStart    nodeId))) = Just $ Node.startEditName nodeId
handle (UI (NodeEvent (Node.NameApply        nodeId))) = Just $ Node.applyName   nodeId
handle (UI (NodeEvent (Node.NameDiscard      nodeId))) = Just $ Node.discardName nodeId
handle (UI (NodeEvent (Node.NameChange   val nodeId))) = Just $ Node.editName nodeId val
handle (UI (NodeEvent (Node.PortEditString       portRef defaultValue))) = Just $ PortControl.setPortDefault portRef defaultValue
handle (UI (NodeEvent (Node.PortApplyString kevt portRef defaultValue))) = Just $ when (Keys.withoutMods kevt Keys.enter) $
                                                                                        Batch.setDefaultValue portRef defaultValue
handle (UI (NodeEvent (Node.PortSetDefaultValue portRef defaultValue))) = Just $ Batch.setDefaultValue portRef defaultValue
handle (UI (NodeEvent (Node.PortInitSlider mevt portRef sliderInit)))   = Just $ do
    mousePos <- mousePosition mevt
    PortControl.startMoveSlider portRef mousePos sliderInit
handle (UI (NodeEvent (Node.SetCode nodeId code))) = Just $ Batch.setCode nodeId code
handle (UI (AppEvent  (App.MouseMove mevt _))) = Just $ do
    mousePos <- mousePosition mevt
    continue $ PortControl.moveSlider mousePos
    let shouldSnap = Mouse.withoutMods mevt Mouse.leftButton
    continue $ Node.nodesDrag mevt shouldSnap
handle (UI (AppEvent  (App.MouseUp   mevt))) = Just $ do
    mousePos <- mousePosition mevt
    continue $ PortControl.stopMoveSlider mousePos
    continue $ Node.handleNodeDragMouseUp mevt
handle _   = Nothing


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.SelectAll           -> selectAll
    Shortcut.RemoveSelectedNodes -> Node.removeSelectedNodes
    Shortcut.Cancel              -> unselectAll
    Shortcut.ExpandSelectedNodes -> Node.selectedToggleMode Node.Expanded
    Shortcut.EditSelectedNodes   -> Node.selectedToggleMode Node.Editor
    _                            -> return ()
