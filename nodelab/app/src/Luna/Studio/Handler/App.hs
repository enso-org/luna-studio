module Luna.Studio.Handler.App
    ( handle
    ) where

import           Luna.Studio.Prelude

import qualified JS.Config                       as Config
import qualified Luna.Studio.Action.Batch        as Batch
import           Luna.Studio.Action.Command      (Command)
import           Luna.Studio.Action.Graph.Update (updateScene)
import           Luna.Studio.Event.Event         (Event (Init, Shortcut, UI))
import           Luna.Studio.Event.Mouse         (mousePosition)
import qualified Luna.Studio.Event.Shortcut      as Shortcut
import           Luna.Studio.Event.UI            (UIEvent (AppEvent))
import qualified Luna.Studio.React.Event.App     as App
import           Luna.Studio.State.Global        (State)
import qualified Luna.Studio.State.Global        as Global


handle :: Event -> Maybe (Command Global.State ())
handle (UI (AppEvent (App.MouseMove evt _))) = Just $ Global.mousePos <~ mousePosition evt
handle (UI (AppEvent  App.Resize          )) = Just   updateScene
handle (UI (AppEvent  App.MouseLeave      )) = Just   Global.endAll
handle (Shortcut (Shortcut.Event command _)) = Just $ handleCommand command
handle  Init                                 = Just $ maybe Batch.listProjects Batch.openProject Config.openedFile
handle _                                     = Nothing


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.Cancel -> Global.endAll
    _               -> return ()
