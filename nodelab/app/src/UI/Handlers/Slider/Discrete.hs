module UI.Handlers.Slider.Discrete where

import           Luna.Studio.Prelude

import           Luna.Studio.Data.Vector                  (Vector2(..), x, y)
import           Data.HMap.Lazy                (HTMap)
import qualified Data.Text.Lazy                as Text
import           Data.Text.Lazy.Read           (signed, decimal)

import           Event.Keyboard                (shift)
import qualified Event.Mouse                   as Mouse
import           Object.UITypes                (WidgetId)
import           Object.Widget                 (DblClickHandler, DragEndHandler, DragMoveHandler, KeyUpHandler,
                                                MousePressedHandler, UIHandlers, currentPos, dblClick, dragEnd,
                                                dragMove, keyMods, keyUp, mousePressed, startPos, click,
                                                CompositeWidget, createWidget, updateWidget, ResizableWidget, resizeWidget)
import qualified Object.Widget.Slider.Discrete as Model
import qualified Object.Widget.TextBox         as TextBox
import           Reactive.Commands.Command     (Command, performIO)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           Reactive.State.Global         (inRegistry)
import qualified Reactive.State.Global         as Global
import           Reactive.State.UIRegistry     (addHandler)

import           UI.Generic                    (startDrag)
import           UI.Handlers.Generic           (ValueChangedHandler (..), triggerValueChanged)
import qualified UI.Handlers.TextBox           as TextBox
import           UI.Widget.Number              (keyModMult)
import           UI.Widget.Slider.Discrete     (setTicks)
import           UI.Widget.Slider              (Slider)
import qualified UI.Registry                   as UI

isEnabled :: WidgetId -> Command Global.State Bool
isEnabled wid = inRegistry $ UICmd.get wid Model.enabled

keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'W' _ _ wid = do
    enabled <- isEnabled wid
    when enabled $ do
        widget <- inRegistry $ UICmd.update wid $ Model.boundedNormValue +~ 0.1
        triggerValueChanged (widget ^. Model.value) wid

keyUpHandler 'Q' _ _ wid = do
    enabled <- isEnabled wid
    when enabled $ do
        widget <- inRegistry $  UICmd.update wid $ Model.boundedNormValue -~ 0.1
        triggerValueChanged (widget ^. Model.value) wid

keyUpHandler _ _ _ _ = return ()

startSliderDrag :: MousePressedHandler Global.State
startSliderDrag evt _ wid = do
    enabled <- isEnabled wid
    when (enabled && (evt ^. Mouse.button == Mouse.LeftButton)) $ do
        value <- inRegistry $ UICmd.get wid Model.boundedValue
        inRegistry $ UICmd.update_ wid $ Model.dragStartValue .~ (Just value)
        startDrag evt wid

dragHandler :: DragMoveHandler Global.State
dragHandler ds _ wid = do
    enabled <- isEnabled wid
    when enabled $ do
        startValue <- inRegistry $ UICmd.get wid Model.dragStartValue
        withJust startValue $ \startValue -> do
            widget <- inRegistry $ UICmd.lookup wid
            let width     = widget ^. Model.size . x
                diff      = ds ^. currentPos - ds ^. startPos
                deltaNorm = if (abs $ diff ^. x) > (abs $ diff ^. y) then  diff ^. x /  divider
                                                                     else -diff ^. y / (divider * 10.0)
                divider   = width * (keyModMult $ ds ^. keyMods)
                delta     = round $ deltaNorm * (fromIntegral $ widget ^. Model.range)
            inRegistry $ UICmd.update_ wid $ Model.boundedValue .~ (startValue + delta)

dragEndHandler :: DragEndHandler Global.State
dragEndHandler _ _ wid = do
    enabled <- isEnabled wid
    when enabled $ do
        inRegistry $ UICmd.update_ wid $ Model.dragStartValue .~ Nothing
        value <- inRegistry $ UICmd.get wid Model.boundedValue
        triggerValueChanged value wid

dblClickHandler :: DblClickHandler Global.State
dblClickHandler evt _ wid = do
    enabled <- isEnabled wid
    let shiftDown = evt ^. Mouse.keyMods . shift
    when (enabled && not shiftDown) $ do
        (tbId:_) <- inRegistry $ UICmd.children wid
        UICmd.takeFocus tbId
        inRegistry $ UICmd.update_ tbId $ TextBox.isEditing .~ True

clickHandler :: DblClickHandler Global.State
clickHandler evt _ wid = do
    enabled <- isEnabled wid
    let shiftDown = evt ^. Mouse.keyMods . shift
    when (enabled && shiftDown) $ do
        width <- inRegistry $ UICmd.get wid $ Model.size . x
        let normValue = (evt ^. Mouse.position ^. x) / width
        widget <- inRegistry $ UICmd.update wid $ Model.boundedNormValue .~ normValue
        triggerValueChanged (widget ^. Model.value) wid

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp        .~ keyUpHandler
                     & click        .~ clickHandler
                     & dblClick     .~ dblClickHandler
                     & mousePressed .~ startSliderDrag
                     & dragMove     .~ dragHandler
                     & dragEnd      .~ dragEndHandler

-- Constructors

textHandlers :: WidgetId -> HTMap
textHandlers wid = addHandler (ValueChangedHandler $ textValueChangedHandler wid)
                $ mempty where

textValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
textValueChangedHandler parent val tbId = do
    let val' = signed decimal val
    case val' of
        Left _err       -> inRegistry $ do
            val <- UICmd.get parent Model.displayValue
            UICmd.update_ tbId $ TextBox.value .~ (Text.pack $ val)
        Right (val', _) -> do
            inRegistry $ UICmd.update_ parent $ Model.value .~ val'
            triggerValueChanged val' parent


instance CompositeWidget Model.DiscreteSlider where
    createWidget wid model = do
        let tx      = (model ^. Model.size . x) / 2.0
            ty      = (model ^. Model.size . y)
            sx      = tx - (model ^. Model.size . y / 2.0)
            textVal = Text.pack $ show $ model ^. Model.value
            textBox = TextBox.create (Vector2 sx ty) textVal TextBox.Right

        tbId <- UICmd.register wid textBox $ textHandlers wid
        UICmd.moveX tbId tx

    updateWidget wid _old model = do
        (tbId:_) <- UICmd.children wid
        UICmd.update_ tbId $ TextBox.value .~ (Text.pack $ model ^. Model.displayValue)

instance ResizableWidget Model.DiscreteSlider where
    resizeWidget wid size model = do
        performIO $ do
            slider <- UI.lookup wid :: IO Slider
            setTicks model slider
        TextBox.labeledEditableResize wid size model
