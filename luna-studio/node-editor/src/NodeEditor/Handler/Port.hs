module NodeEditor.Handler.Port where

import           Common.Action.Command       (Command)
import           Common.Prelude
import           NodeEditor.Action.Basic     (setPortDefault)
import           NodeEditor.Action.Port      (acceptEditTextPortControl, editTextPortControl, handleClick, handleMouseDown,
                                              handleMouseEnter, handleMouseLeave, startMoveSlider)
import           NodeEditor.Event.Event      (Event (UI))
import           NodeEditor.Event.UI         (UIEvent (PortEvent))
import qualified NodeEditor.React.Event.Port as Port
import           NodeEditor.State.Action     (Action (continue))
import           NodeEditor.State.Global     (State)


handle :: Event -> Maybe (Command State ())
handle (UI (PortEvent (Port.MouseDown           evt portRef)))            = Just $ handleMouseDown evt portRef
handle (UI (PortEvent (Port.Click               evt portRef)))            = Just $ handleClick     evt portRef
handle (UI (PortEvent (Port.MouseEnter              portRef)))            = Just $ handleMouseEnter portRef
handle (UI (PortEvent (Port.MouseLeave              portRef)))            = Just $ handleMouseLeave portRef
handle (UI (PortEvent (Port.EditTextPortControlBlur _)))                  = Just $ continue acceptEditTextPortControl
handle (UI (PortEvent (Port.EditTextPortControl     portRef val)))        = Just $ editTextPortControl portRef val
handle (UI (PortEvent (Port.PortSetPortDefault      portRef portDef)))    = Just $ setPortDefault portRef portDef
handle (UI (PortEvent (Port.PortInitSlider      _   portRef sliderInit))) = Just $ startMoveSlider portRef sliderInit
handle _                                                                  = Nothing
