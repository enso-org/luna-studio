module UI.Handlers.Button where

import           Luna.Studio.Prelude

import           Data.HMap.Lazy               (TypeKey (..))

import           Event.Event                  (JSState)
import qualified Event.Mouse                  as Mouse
import           Object.Widget                (ClickHandler, DblClickHandler, UIHandlers, WidgetId, click, dblClick,
                                               mouseOut, mouseOver, mousePressed)
import           Luna.Studio.Commands.Command    (Command, performIO)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import           Luna.Studio.State.Global        (inRegistry)
import qualified Luna.Studio.State.Global        as Global

import           JS.Cursor                    (Cursor (Normal, Pointer), setCursor)
import           UI.Widget.Toggle             ()


newtype ClickedHandler = ClickedHandler (WidgetId -> Command Global.State ())

clickedHandler :: TypeKey ClickedHandler
clickedHandler = TypeKey

triggerClicked :: WidgetId -> Command Global.State ()
triggerClicked wid = do
    maybeHandler <- inRegistry $ UICmd.handler wid clickedHandler
    withJust maybeHandler $ \(ClickedHandler handler) -> handler wid

clickHandler :: ClickHandler Global.State
clickHandler _ _ wid = triggerClicked wid


newtype DblClickedHandler = DblClickedHandler (WidgetId -> Command Global.State ())

dblClickedHandler :: TypeKey DblClickedHandler
dblClickedHandler = TypeKey

triggerDblClicked :: WidgetId -> Command Global.State ()
triggerDblClicked wid = do
    maybeHandler <- inRegistry $ UICmd.handler wid dblClickedHandler
    withJust maybeHandler $ \(DblClickedHandler handler) -> handler wid

dblClickHandler :: DblClickHandler Global.State
dblClickHandler _ _ wid = triggerDblClicked wid


newtype MousePressedHandler = MousePressedHandler (Mouse.Event' -> WidgetId  -> Command Global.State ())

mousePressedHandler :: TypeKey MousePressedHandler
mousePressedHandler = TypeKey

triggerMousePressed :: Mouse.Event' -> JSState -> WidgetId -> Command Global.State ()
triggerMousePressed evt _ wid = do
    maybeHandler <- inRegistry $ UICmd.handler wid mousePressedHandler
    withJust maybeHandler $ \(MousePressedHandler handler) -> handler evt wid

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & click        .~ clickHandler
                     & dblClick     .~ dblClickHandler
                     & mouseOver    .~ (\_ _ -> performIO $ setCursor Pointer)
                     & mouseOut     .~ (\_ _ -> performIO $ setCursor Normal)
                     & mousePressed .~ triggerMousePressed
