module Node.Editor.Handler.Port where

import           Node.Editor.Action.Command   (Command)
import           Node.Editor.Action.Port      (handleClick, handleMouseDown, handleMouseEnter, handleMouseLeave)
import           Node.Editor.Event.Event      (Event (UI))
import           Node.Editor.Event.UI         (UIEvent (PortEvent))
import           Luna.Prelude
import qualified Node.Editor.React.Event.Port as Port
import           Node.Editor.State.Global     (State)


handle :: Event -> Maybe (Command State ())
handle (UI (PortEvent (Port.MouseDown  evt portRef))) = Just $ handleMouseDown evt portRef
handle (UI (PortEvent (Port.Click      evt portRef))) = Just $ handleClick     evt portRef
handle (UI (PortEvent (Port.MouseEnter portRef)))     = Just $ handleMouseEnter portRef
handle (UI (PortEvent (Port.MouseLeave portRef)))     = Just $ handleMouseLeave portRef
handle _                                              = Nothing
