module Node.Editor.Handler.App
    ( handle
    ) where

import           Luna.Prelude

import           Node.Editor.Action.Basic        (updateScene)
import qualified Node.Editor.Action.Batch        as Batch
import           Node.Editor.Action.Command      (Command)
import           Node.Editor.Action.State.Action (endAll)
import           Node.Editor.Event.Event         (Event (Init, Shortcut, UI))
import           Node.Editor.Event.Mouse         (mousePosition)
import qualified Node.Editor.Event.Shortcut      as Shortcut
import           Node.Editor.Event.UI            (UIEvent (AppEvent))
import qualified Node.Editor.React.Event.App     as App
import           Node.Editor.State.Global        (State)
import qualified Node.Editor.State.Global        as Global
import qualified Node.Editor.State.UI            as UI


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
