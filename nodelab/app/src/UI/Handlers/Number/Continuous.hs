module UI.Handlers.Number.Continuous where

import           Luna.Studio.Prelude

import           Data.HMap.Lazy                  (HTMap)
import qualified Data.Text.Lazy                  as Text
import           Data.Text.Lazy.Read             (rational)
import           Luna.Studio.Data.Vector

import qualified Event.Mouse                     as Mouse
import           Object.Widget                   (DblClickHandler, DragEndHandler, DragMoveHandler, KeyUpHandler,
                                                  MousePressedHandler, UIHandlers, WidgetId, currentPos, dblClick,
                                                  dragEnd, dragMove, keyMods, keyUp, mousePressed, startPos,
                                                  CompositeWidget, createWidget, updateWidget, ResizableWidget, resizeWidget)
import qualified Object.Widget.Number.Continuous as Model
import qualified Object.Widget.TextBox           as TextBox
import           Reactive.Commands.Command       (Command)
import qualified Reactive.Commands.UIRegistry    as UICmd
import           Reactive.State.Global           (inRegistry)
import qualified Reactive.State.Global           as Global
import           Reactive.State.UIRegistry       (addHandler)

import           UI.Generic                      (startDrag)
import           UI.Widget.Number                (keyModMult)
import           UI.Widget.Number.Continuous     ()

import           UI.Handlers.Generic             (triggerValueChanged, ValueChangedHandler (..))
import qualified UI.Handlers.TextBox             as TextBox

isEnabled :: WidgetId -> Command Global.State Bool
isEnabled wid = inRegistry $ UICmd.get wid Model.enabled

bumpValue :: Double -> WidgetId -> Command Global.State ()
bumpValue amount wid = do
    widget <- zoom Global.uiRegistry $ UICmd.update wid (Model.value +~ amount)
    triggerValueChanged (widget ^. Model.value) wid

keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'Q' _ _ wid = bumpValue (-1.0) wid
keyUpHandler 'W' _ _ wid = bumpValue   1.0  wid
keyUpHandler _   _ _ _  = return ()

startSliderDrag :: MousePressedHandler Global.State
startSliderDrag evt _ wid = do
    enabled <- isEnabled wid
    when (enabled && (evt ^. Mouse.button == Mouse.LeftButton)) $ do
        value <- inRegistry $ UICmd.get wid Model.value
        inRegistry $ UICmd.update_ wid $ Model.dragStartValue .~ Just value
        startDrag evt wid

dragHandler :: DragMoveHandler Global.State
dragHandler ds _ wid = do
    enabled <- isEnabled wid
    when enabled $ do
        startValue <- inRegistry $ UICmd.get wid Model.dragStartValue
        withJust startValue $ \startValue -> do
            -- width <- inRegistry $ UICmd.get wid $ Model.size . x
            let diff    = ds ^. currentPos - ds ^. startPos
                delta   = if abs (diff ^. x) > abs (diff ^. y) then  diff ^. x /  divider
                                                               else -diff ^. y / (divider * 10.0)
                divider = keyModMult $ ds ^. keyMods
            inRegistry $ UICmd.update_ wid $ Model.value .~ (startValue + delta)

dragEndHandler :: DragEndHandler Global.State
dragEndHandler _ _ wid = do
    enabled <- isEnabled wid
    when enabled $ do
        value      <- inRegistry $ UICmd.get wid Model.value
        startValue <- inRegistry $ UICmd.get wid Model.dragStartValue
        inRegistry $ UICmd.update_ wid $ Model.dragStartValue .~ Nothing
        let startVal = fromMaybe value startValue
        when (startVal /= value) $ triggerValueChanged value wid

dblClickHandler :: DblClickHandler Global.State
dblClickHandler _ _ wid = do
    (tbId:_) <- inRegistry $ UICmd.children wid
    UICmd.takeFocus tbId
    inRegistry $ UICmd.update_ tbId $ TextBox.isEditing .~ True

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp        .~ keyUpHandler
                     & dblClick     .~ dblClickHandler
                     & mousePressed .~ startSliderDrag
                     & dragMove     .~ dragHandler
                     & dragEnd      .~ dragEndHandler


-- Constructors


textHandlers :: WidgetId -> HTMap
textHandlers wid = addHandler (ValueChangedHandler $ textValueChangedHandler wid) mempty

textValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
textValueChangedHandler parent val tbId = do
    let val' = rational val
    case val' of
        Left _err       -> inRegistry $ do
            val <- UICmd.get parent Model.displayValue
            UICmd.update_ tbId $ TextBox.value .~ Text.pack val
        Right (val', _) -> do
            oldValue <- inRegistry $ UICmd.get parent Model.value
            when (oldValue /= val') $ do
                inRegistry $ UICmd.update_ parent $ Model.value .~ val'
                triggerValueChanged val' parent


instance CompositeWidget Model.ContinuousNumber where
    createWidget wid model = do
        let tx      = (model ^. Model.size . x) / 2.0
            ty      =  model ^. Model.size . y
            sx      = tx - (model ^. Model.size . y / 2.0)
            textVal = Text.pack $ show $ model ^. Model.value
            textBox = TextBox.create (Vector2 sx ty) textVal TextBox.Right

        tbId <- UICmd.register wid textBox $ textHandlers wid
        UICmd.moveX tbId tx

    updateWidget wid _old model = do
        (tbId:_) <- UICmd.children wid
        UICmd.update_ tbId $ TextBox.value .~ Text.pack (model ^. Model.displayValue)

instance ResizableWidget Model.ContinuousNumber where resizeWidget = TextBox.labeledEditableResize
