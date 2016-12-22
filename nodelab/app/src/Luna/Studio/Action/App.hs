module Luna.Studio.Action.App
    ( toAction
    ) where

import           Luna.Studio.Prelude

import           Event.Event
import           Event.UI                     (UIEvent (AppEvent))
import           Luna.Studio.Commands.Command (Command)
import           Luna.Studio.Event.Mouse      (getMousePosition)
import qualified Luna.Studio.React.Event.App  as App
import qualified Luna.Studio.State.Global     as Global



toAction :: Event -> Maybe (Command Global.State ())
toAction (UI (AppEvent (App.MouseMove evt))) = Just $ Global.mousePos .= getMousePosition evt
toAction _                                   = Nothing
