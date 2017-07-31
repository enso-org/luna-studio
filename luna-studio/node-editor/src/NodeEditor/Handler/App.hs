module NodeEditor.Handler.App
    ( handle
    ) where

import           Common.Prelude

import           NodeEditor.Action.Basic        (setFile, unselectAll, unsetFile, updateScene)
import qualified NodeEditor.Action.Batch        as Batch
import           NodeEditor.Action.Command      (Command)
import           NodeEditor.Action.State.Action (endActions, endAllActions)
import qualified NodeEditor.Event.Atom          as Atom
import           NodeEditor.Event.Event         (Event (Atom, Init, Shortcut, UI))
import           NodeEditor.Event.Mouse         (mousePosition)
import qualified NodeEditor.Event.Shortcut      as Shortcut
import           NodeEditor.Event.UI            (UIEvent (AppEvent))
import qualified NodeEditor.React.Event.App     as App
import           NodeEditor.State.Action        (actionsClosingOnMouseLeave)
import           NodeEditor.State.Global        (State)
import qualified NodeEditor.State.Global        as Global
import qualified NodeEditor.State.UI            as UI


handle :: Event -> Maybe (Command Global.State ())
handle (UI (AppEvent (App.MouseMove evt _))) = Just $ Global.ui . UI.mousePos <~ mousePosition evt
handle (UI (AppEvent  App.Resize          )) = Just   updateScene
handle (UI (AppEvent  App.MouseLeave      )) = Just $ endActions actionsClosingOnMouseLeave
handle (Shortcut (Shortcut.Event command _)) = Just $ handleCommand command
handle  Init                                 = Just $ Batch.getProgram def >> Batch.searchNodes
handle (Atom (Atom.SetFile path)           ) = Just $ setFile path
handle (Atom  Atom.UnsetFile               ) = Just   unsetFile
handle _                                     = Nothing


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.Cancel -> whenM (null <$> endAllActions) unselectAll
    _               -> return ()
