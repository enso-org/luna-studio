module Luna.Studio.Handler.App
    ( handle
    ) where

import           Luna.Studio.Prelude

import           Empire.API.Data.Breadcrumb      (Breadcrumb (..))
import           Empire.API.Data.GraphLocation   (GraphLocation(..))
import qualified JS.Config                       as Config
import           Luna.Studio.Action.Basic        (updateScene)
import qualified Luna.Studio.Action.Batch        as Batch
import           Luna.Studio.Action.Command      (Command)
import           Luna.Studio.Action.State.Action (endAll)
import           Luna.Studio.Event.Event         (Event (Init, Shortcut, UI))
import           Luna.Studio.Event.Mouse         (mousePosition)
import qualified Luna.Studio.Event.Shortcut      as Shortcut
import           Luna.Studio.Event.UI            (UIEvent (AppEvent))
import qualified Luna.Studio.React.Event.App     as App
import           Luna.Studio.Batch.Workspace     (currentLocation)
import           Luna.Studio.State.Global        (State)
import qualified Luna.Studio.State.Global        as Global
import qualified Luna.Studio.State.UI            as UI


handle :: Event -> Maybe (Command Global.State ())
handle (UI (AppEvent (App.MouseMove evt _))) = Just $ Global.ui . UI.mousePos <~ mousePosition evt
handle (UI (AppEvent  App.Resize          )) = Just   updateScene
handle (UI (AppEvent  App.MouseLeave      )) = Just   endAll
handle (Shortcut (Shortcut.Event command _)) = Just $ handleCommand command
handle  Init                                 = Just $ initLibrary
handle _                                     = Nothing


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.Cancel -> endAll
    _               -> return ()

initLibrary :: Command Global.State ()
initLibrary = do
    let file = "/home/mmikolajczyk/git/integration/env/projects/Main.luna"
    Global.workspace . currentLocation .= GraphLocation file (Breadcrumb [])
    Batch.openFile file
