module Luna.Studio.Action.App
    ( toAction
    ) where

import           Luna.Studio.Prelude

import           Event.Event
import           Event.UI                     (UIEvent (AppEvent))
import           Luna.Studio.Commands.Command (Command)
import           Luna.Studio.Event.Mouse      (mousePosition)
import qualified Luna.Studio.React.Event.App  as App
import qualified Luna.Studio.State.Global     as Global



toAction :: Event -> Maybe (Command Global.State ())
toAction (UI (AppEvent (App.MouseMove evt))) = Just $ Global.mousePos .= mousePosition evt
toAction _                                   = Nothing
