module UI.Handlers.CodeEditor where

import           Luna.Studio.Prelude

import           Data.JSString.Text              (lazyTextFromJSString)

import           Event.Event                     (JSState)
import           Event.Widget                    (Payload (..))
import           Luna.Studio.Commands.Command    (Command)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import           Luna.Studio.State.Global        (inRegistry_)
import qualified Luna.Studio.State.Global        as Global
import           Object.Widget                   (UIHandlers, WidgetId, fromWidgetId, widgetCustom)
import qualified Object.Widget.CodeEditor        as Model

import           UI.Handlers.Generic             (triggerValueChanged)
import           UI.Widget.CodeEditor            ()


foreign import javascript safe "$1.registry[$2].getCode()" getCode' :: JSState -> Int -> JSString


customHandler :: Payload -> WidgetId -> Command Global.State ()
customHandler CodeEditorBlur widgetId = do
    jsState <- use $ Global.jsState
    let value = lazyTextFromJSString $ getCode' jsState $ fromWidgetId widgetId
    -- TODO[react]: Does not make sense anymore
    -- inRegistry_ $ UICmd.update' widgetId $ Model.value .~ value
    triggerValueChanged value widgetId

customHandler _ _ = return ()

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & widgetCustom .~ customHandler
