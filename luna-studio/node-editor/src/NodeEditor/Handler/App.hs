module NodeEditor.Handler.App
    ( handle
    ) where

import           Common.Prelude

import           Common.Action.Command          (Command)
import           NodeEditor.Action.Basic        (setFile, unselectAll, unsetFile, updateFilePath)
import qualified NodeEditor.Action.Batch        as Batch
import qualified NodeEditor.Action.Port         as PortControl
import           NodeEditor.Action.State.Action (checkIfActionPerfoming, endActions, endAllActions)
import qualified NodeEditor.Event.Atom          as Atom
import           NodeEditor.Event.Event         (Event (Atom, Init, Shortcut, UI, View))
import qualified NodeEditor.Event.Shortcut      as Shortcut
import           NodeEditor.Event.UI            (UIEvent (AppEvent))
import           NodeEditor.Event.View          (ViewEvent (ViewEvent))
import qualified NodeEditor.Event.View          as View
import qualified NodeEditor.React.Event.App     as App
import           NodeEditor.State.Action        (actionsClosingOnMouseLeave)
import           NodeEditor.State.Action        (Action (continue), ActionRep, textPortControlEditAction)
import           NodeEditor.State.Global        (State)
import qualified NodeEditor.State.Global        as Global


handle :: Event -> Maybe (Command Global.State ())
handle (UI (AppEvent App.MouseLeave))        = Just
    $ endActions actionsClosingOnMouseLeave
handle (Shortcut (Shortcut.Event command _)) = Just $ handleCommand command
handle Init                                  = Just $ Batch.getProgram def True
handle (Atom (Atom.SetFile path))            = Just $ setFile path
handle (Atom (Atom.UpdateFilePath path))     = Just $ updateFilePath path
handle (Atom  Atom.UnsetFile)                = Just unsetFile
handle (View (ViewEvent path _ base))        = case path of
    ["NodeEditor"] -> case base ^? View._Mouse . View.type_ of
        Just "mouseout" -> Just $ endActions actionsClosingOnMouseLeave
        _               -> Nothing
    _ -> Nothing
handle _ = Nothing


cancelAllActions :: Command State [ActionRep]
cancelAllActions = do
    tpcePerforming <- checkIfActionPerfoming textPortControlEditAction
    if not tpcePerforming then endAllActions else do
        continue PortControl.rollbackEditTextPortControl
        (textPortControlEditAction :) <$> endAllActions

handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.Cancel -> whenM (null <$> cancelAllActions) unselectAll
    _               -> return ()
