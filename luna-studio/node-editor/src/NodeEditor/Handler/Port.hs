module NodeEditor.Handler.Port where

import           Common.Action.Command       (Command)
import           Common.Prelude
import           NodeEditor.Action.Port      (handleClick, handleMouseDown, handleMouseEnter, handleMouseLeave)
import           NodeEditor.Event.Event      (Event (UI))
import           NodeEditor.Event.UI         (UIEvent (PortEvent))
import qualified NodeEditor.React.Event.Port as Port
import           NodeEditor.State.Global     (State)


handle :: Event -> Maybe (Command State ())
handle (UI (PortEvent (Port.MouseDown  evt portRef))) = Just $ handleMouseDown evt portRef
handle (UI (PortEvent (Port.Click      evt portRef))) = Just $ handleClick     evt portRef
handle (UI (PortEvent (Port.MouseEnter portRef)))     = Just $ handleMouseEnter portRef
handle (UI (PortEvent (Port.MouseLeave portRef)))     = Just $ handleMouseLeave portRef
handle _                                              = Nothing
