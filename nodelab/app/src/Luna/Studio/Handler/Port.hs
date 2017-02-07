module Luna.Studio.Handler.Port where

import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Action.Port      (setHighlight)
import           Luna.Studio.Event.Event      (Event (UI))
import           Luna.Studio.Event.UI         (UIEvent (PortEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Port as Port
import           Luna.Studio.State.Global     (State)


handle :: Event -> Maybe (Command State ())
handle (UI (PortEvent (Port.MouseEnter portRef))) = Just $ setHighlight portRef True
handle (UI (PortEvent (Port.MouseLeave portRef))) = Just $ setHighlight portRef False
handle _                                          = Nothing
