module NodeEditor.Handler.App
    ( handle
    ) where

import           Common.Prelude

import           Common.Action.Command          (Command)
import           NodeEditor.Action.Basic        (setFile, unselectAll, unsetFile, updateFilePath, updateScene)
import qualified NodeEditor.Action.Batch        as Batch
import qualified NodeEditor.Action.Port         as PortControl
import           NodeEditor.Action.State.Action (checkIfActionPerfoming, endActions, endAllActions)
import qualified NodeEditor.Event.Atom          as Atom
import           NodeEditor.Event.Event         (Event (Atom, Init, Shortcut, UI))
import qualified NodeEditor.Event.Shortcut      as Shortcut
import           NodeEditor.Event.UI            (UIEvent (AppEvent, SidebarEvent))
import qualified NodeEditor.React.Event.App     as App
import qualified NodeEditor.React.Event.Sidebar as Sidebar
import           NodeEditor.State.Action        (actionsClosingOnMouseLeave)
import           NodeEditor.State.Action        (Action (continue), ActionRep, textPortControlEditAction)
import           NodeEditor.State.Global        (State)
import qualified NodeEditor.State.Global        as Global
import           NodeEditor.State.Mouse         (mousePosition)
import qualified NodeEditor.State.UI            as UI


handle :: Event -> Maybe (Command Global.State ())
handle (UI (AppEvent     (App.MouseMove evt _)))       = Just $ Global.ui . UI.mousePos <~ mousePosition evt
handle (UI (SidebarEvent (Sidebar.MouseMove evt _ _))) = Just $ Global.ui . UI.mousePos <~ mousePosition evt
handle (UI (AppEvent     App.Resize          ))        = Just   updateScene
handle (UI (AppEvent     App.MouseLeave      ))        = Just $ endActions actionsClosingOnMouseLeave
handle (Shortcut         (Shortcut.Event command _))   = Just $ handleCommand command
handle  Init                                           = Just $ Batch.getProgram def True
handle (Atom (Atom.SetFile path))                      = Just $ setFile path
handle (Atom (Atom.UpdateFilePath path))               = Just $ updateFilePath path
handle (Atom  Atom.UnsetFile)                          = Just   unsetFile
handle _                                               = Nothing


cancelAllActions :: Command State [ActionRep]
cancelAllActions = do
    tpcePerforming <- checkIfActionPerfoming textPortControlEditAction
    if not tpcePerforming then endAllActions else do
        continue PortControl.rollbackEditTextPortControl
        PortControl.unfocusEditTextPortControl
        (textPortControlEditAction :) <$> endAllActions

handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.Cancel -> whenM (null <$> cancelAllActions) unselectAll
    _               -> return ()
