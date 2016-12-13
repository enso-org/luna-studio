module UI.Handlers.TextBox where

import           Luna.Studio.Prelude

import           Luna.Studio.Data.Vector

import           Data.JSString.Text           (lazyTextFromJSString)

import           Event.Event                  (JSState)
import           Object.Widget                (ClickHandler, IsDisplayObject, KeyDownHandler, UIHandlers, WidgetId, dblClick, fromWidgetId,
                                               keyDown, widgetSize)
import qualified Object.Widget.TextBox        as Model
import           Luna.Studio.Commands.Command    (Command)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import           Luna.Studio.State.Global        (inRegistry)
import qualified Luna.Studio.State.Global        as Global
import qualified Luna.Studio.State.UIRegistry    as UIRegistry

import           UI.Generic                   (defaultResize)
import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Widget.TextBox            ()


dblClickHandler :: ClickHandler Global.State
dblClickHandler _ _ widgetId = do
    UICmd.takeFocus widgetId
    inRegistry $ UICmd.update_ widgetId $ Model.isEditing .~ True

foreign import javascript safe "$1.registry[$2].input.val()" getValue' :: JSState -> Int -> JSString

keyDownHandler :: KeyDownHandler Global.State
keyDownHandler '\r'  _ _ = applyChanges
keyDownHandler '\27' _ _ = abortChanges
keyDownHandler _     _ _ = const $ return ()

applyChanges :: WidgetId -> Command Global.State ()
applyChanges widgetId = do
    jsState <- use $ Global.jsState
    wasEditing <- inRegistry $ UICmd.get widgetId Model.isEditing
    when (wasEditing) $ do
        let value = lazyTextFromJSString $ getValue' jsState $ fromWidgetId widgetId
        inRegistry $ UICmd.update_ widgetId $ (Model.isEditing .~ False)
                                      . (Model.value     .~ value)
        triggerValueChanged value widgetId

abortChanges :: WidgetId -> Command Global.State ()
abortChanges widgetId = inRegistry $ UICmd.update_ widgetId $ Model.isEditing .~ False

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyDown   .~ keyDownHandler
                     & dblClick  .~ dblClickHandler

-------------------------------------------------------------

labeledEditableResize :: IsDisplayObject a => WidgetId -> Vector2 Double -> a -> Command UIRegistry.State ()
labeledEditableResize widgetId size model = do
    defaultResize widgetId size model

    (tbId:_) <- UICmd.children widgetId
    let tx      = (model ^. widgetSize . x) / 2.0
        ty      = (model ^. widgetSize . y)
        sx      = tx - (model ^. widgetSize . y / 2.0)
    UICmd.resize tbId $ Vector2 sx ty
    UICmd.moveX tbId tx
