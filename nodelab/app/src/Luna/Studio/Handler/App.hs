module Luna.Studio.Handler.App
    ( toAction
    ) where

import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command  (Command)
import           Luna.Studio.Event.Event
import           Luna.Studio.Event.Mouse     (mousePosition)
import           Luna.Studio.Event.UI        (UIEvent (AppEvent))
import qualified Luna.Studio.React.Event.App as App
import qualified Luna.Studio.State.Global    as Global



toAction :: Event -> Maybe (Command Global.State ())
toAction (UI (AppEvent (App.MouseMove evt))) = Just $ Global.mousePos .= mousePosition evt
toAction _                                   = Nothing
