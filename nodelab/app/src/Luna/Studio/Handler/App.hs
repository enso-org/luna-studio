module Luna.Studio.Handler.App
    ( handle
    ) where

import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command  (Command)
import           Luna.Studio.Event.Event
import           Luna.Studio.Event.Mouse     (mousePosition)
import           Luna.Studio.Event.UI        (UIEvent (AppEvent))
import qualified Luna.Studio.React.Event.App as App
import qualified Luna.Studio.State.Global    as Global



handle :: Event -> Maybe (Command Global.State ())
handle (UI (AppEvent (App.MouseMove evt))) = Just $ Global.mousePos <~ mousePosition evt
handle _                                   = Nothing
