module UI.Handlers.Toggle where

import           Luna.Studio.Prelude

import           Luna.Studio.Commands.Command    (Command)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import           Luna.Studio.State.Global        (inRegistry)
import qualified Luna.Studio.State.Global        as Global
import           Object.Widget                   (ClickHandler, KeyUpHandler, UIHandlers, WidgetId, click, keyUp)
import qualified Object.Widget.Toggle            as Model

import           UI.Handlers.Generic             (triggerValueChanged)
import           UI.Widget.Toggle                ()


-- TODO[react]: Does not make sense anymore
-- isEnabled :: WidgetId -> Command Global.State Bool
-- isEnabled wid = inRegistry $ UICmd.get wid Model.enabled
--
-- toggleValue :: WidgetId -> Command Global.State ()
-- toggleValue wid = do
--     enabled <- isEnabled wid
--     when enabled $ do
--         widget <- inRegistry $ UICmd.update wid $ Model.value %~ not
--         triggerValueChanged (widget ^. Model.value) wid
--
-- clickHandler :: ClickHandler Global.State
-- clickHandler _ _ = toggleValue
--
-- keyUpHandler :: KeyUpHandler Global.State
-- keyUpHandler 'r' _ _ wid = toggleValue wid
-- keyUpHandler _ _ _ _ = return ()
--
-- widgetHandlers :: UIHandlers Global.State
-- widgetHandlers = def & keyUp  .~ keyUpHandler
--                      & click  .~ clickHandler
