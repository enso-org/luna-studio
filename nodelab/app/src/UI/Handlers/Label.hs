module UI.Handlers.Label ( widgetHandlers
                         , ClickedHandler
                         , DblClickedHandler
                         ) where

import           Luna.Studio.Prelude

import           Data.HMap.Lazy               ()

import           Object.Widget                (UIHandlers, click, dblClick, mouseOut, mouseOver, mousePressed)
import           Reactive.Commands.Command    (performIO)
import           Luna.Studio.State.Global        ()
import qualified Luna.Studio.State.Global        as Global

import           JS.Cursor                    (Cursor (Normal, Pointer), setCursor)
import           UI.Handlers.Button           (ClickedHandler, DblClickedHandler, clickHandler, dblClickHandler, triggerMousePressed)

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & click        .~ clickHandler
                     & dblClick     .~ dblClickHandler
                     & mouseOver    .~ (\_ _ -> performIO $ setCursor Pointer)
                     & mouseOut     .~ (\_ _ -> performIO $ setCursor Normal)
                     & mousePressed .~ triggerMousePressed
