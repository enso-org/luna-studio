module Luna.Studio.Handler.Port where

import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Action.Port      (handleClick, handleMouseDown, handleMouseEnter, handleMouseLeave)
import           Luna.Studio.Event.Event      (Event (UI))
import           Luna.Studio.Event.UI         (UIEvent (PortEvent))
import           Luna.Prelude
import qualified Luna.Studio.React.Event.Port as Port
import           Luna.Studio.State.Global     (State)


handle :: Event -> Maybe (Command State ())
handle (UI (PortEvent (Port.MouseDown  evt portRef))) = Just $ handleMouseDown evt portRef
handle (UI (PortEvent (Port.Click      evt portRef))) = Just $ handleClick     evt portRef
handle (UI (PortEvent (Port.MouseEnter portRef)))     = Just $ handleMouseEnter portRef
handle (UI (PortEvent (Port.MouseLeave portRef)))     = Just $ handleMouseLeave portRef
handle _                                              = Nothing
