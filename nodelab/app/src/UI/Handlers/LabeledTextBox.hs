module UI.Handlers.LabeledTextBox where

import           Luna.Studio.Prelude

import           Data.HMap.Lazy                  (HTMap)
import           Luna.Studio.Data.Vector

import           Luna.Studio.Commands.Command    (Command)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import           Luna.Studio.State.Global        (inRegistry)
import qualified Luna.Studio.State.Global        as Global
import           Luna.Studio.State.UIRegistry    (addHandler)
import           Object.Widget                   (CompositeWidget, DblClickHandler, ResizableWidget, UIHandlers, WidgetId, createWidget,
                                                  dblClick, resizeWidget, updateWidget)
import qualified Object.Widget.LabeledTextBox    as Model
import qualified Object.Widget.TextBox           as TextBox

import           UI.Generic                      (defaultResize)
import           UI.Widget.LabeledTextBox        ()

import           UI.Handlers.Generic             (ValueChangedHandler (..), triggerValueChanged)
import qualified UI.Handlers.TextBox             as TextBox



dblClickHandler :: DblClickHandler Global.State
dblClickHandler _ _ wid = do
    (tbId:_) <- inRegistry $ UICmd.children wid
    UICmd.takeFocus tbId
    -- inRegistry $ UICmd.update_ tbId $ TextBox.isEditing .~ True

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & dblClick     .~ dblClickHandler

-- Constructors


-- TODO[react]: TextBox.applyChanges does not make sense anymore
-- textHandlers :: WidgetId -> HTMap
-- textHandlers wid = addHandler (ValueChangedHandler $ textValueChangedHandler wid)
--                 $ addHandler (UICmd.LostFocus $ lostFocusHandler wid)
--                 $ mempty where
--
-- textValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
-- textValueChangedHandler parent val _tbId = do
--     inRegistry $ UICmd.update_ parent $ Model.value .~ val
--     triggerValueChanged val parent
--
--
-- lostFocusHandler :: WidgetId -> WidgetId -> Command Global.State ()
-- lostFocusHandler _ wid = TextBox.applyChanges wid
--
-- instance CompositeWidget Model.LabeledTextBox where
--     createWidget wid model = do
--         let tx      = (model ^. Model.size . x) / 2.0
--             ty      = (model ^. Model.size . y)
--             sx      = tx - (model ^. Model.size . y / 2.0)
--             textVal = model ^. Model.value
--             textBox = TextBox.create (Vector2 sx ty) textVal TextBox.Right
--
--         tbId <- UICmd.register wid textBox $ textHandlers wid
--         UICmd.moveX tbId tx
--
--     updateWidget wid _old model = do
--         (tbId:_) <- UICmd.children wid
--         UICmd.update_ tbId $ TextBox.value .~ (model ^. Model.value)
--
-- instance ResizableWidget Model.LabeledTextBox where
--     resizeWidget wid size model = do
--         defaultResize wid size model
--
--         (tbId:_) <- UICmd.children wid
--         let tx      = (model ^. Model.size . x) / 2.0
--             ty      = (model ^. Model.size . y)
--             sx      = tx - (model ^. Model.size . y / 2.0)
--         UICmd.resize tbId $ Vector2 sx ty
--         UICmd.moveX tbId tx
