module UI.Handlers.Toggle where

import           Luna.Studio.Prelude

import           Object.Widget                (ClickHandler, KeyUpHandler, UIHandlers, WidgetId, click, keyUp)
import qualified Object.Widget.Toggle         as Model
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global

import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Widget.Toggle             ()

isEnabled :: WidgetId -> Command Global.State Bool
isEnabled wid = inRegistry $ UICmd.get wid Model.enabled

toggleValue :: WidgetId -> Command Global.State ()
toggleValue wid = do
    enabled <- isEnabled wid
    when enabled $ do
        widget <- inRegistry $ UICmd.update wid $ Model.value %~ not
        triggerValueChanged (widget ^. Model.value) wid

clickHandler :: ClickHandler Global.State
clickHandler _ _ = toggleValue

keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'r' _ _ wid = toggleValue wid
keyUpHandler _ _ _ _ = return ()

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp  .~ keyUpHandler
                     & click  .~ clickHandler
