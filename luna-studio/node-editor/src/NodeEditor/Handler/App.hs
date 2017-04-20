module NodeEditor.Handler.App
    ( handle
    ) where

import           Common.Prelude

import           NodeEditor.Action.Basic        (updateScene)
import qualified NodeEditor.Action.Batch        as Batch
import           NodeEditor.Action.Command      (Command)
import           NodeEditor.Action.State.Action (endAll)
import           NodeEditor.Event.Event         (Event (Init, Shortcut, UI))
import           NodeEditor.Event.Mouse         (mousePosition)
import qualified NodeEditor.Event.Shortcut      as Shortcut
import           NodeEditor.Event.UI            (UIEvent (AppEvent))
import qualified NodeEditor.React.Event.App     as App
import           NodeEditor.State.Global        (State)
import qualified NodeEditor.State.Global        as Global
import qualified NodeEditor.State.UI            as UI


handle :: Event -> Maybe (Command Global.State ())
handle (UI (AppEvent (App.MouseMove evt _))) = Just $ Global.ui . UI.mousePos <~ mousePosition evt
handle (UI (AppEvent  App.Resize          )) = Just   updateScene
handle (UI (AppEvent  App.MouseLeave      )) = Just   endAll
handle (Shortcut (Shortcut.Event command _)) = Just $ handleCommand command
handle  Init                                 = Just   Batch.getProgram
handle _                                     = Nothing


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.Cancel -> endAll
    _               -> return ()
