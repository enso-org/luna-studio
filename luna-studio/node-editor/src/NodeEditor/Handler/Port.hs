module NodeEditor.Handler.Port where

import           Common.Action.Command       (Command)
import           Common.Prelude
import           LunaStudio.Data.PortRef     (AnyPortRef (InPortRef', OutPortRef'))
import           NodeEditor.Action.Basic     (setPortDefault)
import           NodeEditor.Action.Port      (acceptEditTextPortControl, editTextPortControl, handleClick, handleMouseDown,
                                              handleMouseEnter, handleMouseLeave, startMoveSlider)
import           NodeEditor.Event.Event      (Event (UI, View))
import           NodeEditor.Event.UI         (UIEvent (PortEvent), _PortEvent)
import           NodeEditor.Event.View       (BaseEvent (Mouse, PortControl), PortControlEvent (PortControlEvent), ViewEvent, mousePosition)
import qualified NodeEditor.Event.View       as View
import qualified NodeEditor.React.Event.Port as Port
import           NodeEditor.State.Action     (Action (continue))
import           NodeEditor.State.Global     (State)
import qualified NodeEditor.State.Mouse      as State


handle :: Event -> Maybe (Command State ())
handle (UI   evt) = handleUIEvent evt
handle (View evt) = handleViewEvent evt
handle _          = Nothing

handleViewEvent :: ViewEvent -> Maybe (Command State ())
handleViewEvent evt = case evt ^. View.base of
    Mouse e -> do
        let path      = evt ^. View.path
            portRef   = evt ^. View.target
            isInPort  = last path == "InPort"
            isOutPort = last path == "OutPort"
            anyPortRef = View.getAnyPortRef evt
        if isInPort || isOutPort then case e ^. View.type_ of
            "click"     -> Just
                $ handleClick     (mousePosition $ evt ^. View.base) anyPortRef
            "mousedown" -> Just
                $ handleMouseDown (mousePosition $ evt ^. View.base) anyPortRef
            _ -> Nothing
        else Nothing
    PortControl e -> do
        let inPortRef = View.getInPortRef evt
            content = e ^. View.content
        Just $ setPortDefault inPortRef $ convert content
    _ -> Nothing

handleUIEvent :: UIEvent -> Maybe (Command State ())
handleUIEvent evt = maybe Nothing handlePortEvent $ evt ^? _PortEvent where
    handlePortEvent = \case
        Port.MouseDown evt portRef
            -> Just $ State.mousePosition evt >>= flip handleMouseDown portRef
        Port.Click evt portRef
            -> Just $ State.mousePosition evt >>= flip handleClick portRef
        Port.MouseEnter portRef -> Just $ handleMouseEnter portRef
        Port.MouseLeave portRef -> Just $ handleMouseLeave portRef
        Port.EditTextPortControlBlur _
            -> Just $ continue acceptEditTextPortControl
        Port.EditTextPortControl portRef val
            -> Just $ editTextPortControl portRef val
        Port.PortSetPortDefault portRef portDef
            -> Just $ setPortDefault portRef portDef
        Port.PortInitSlider _ portRef sliderInit
            -> Just $ startMoveSlider portRef sliderInit
        _   -> Nothing
