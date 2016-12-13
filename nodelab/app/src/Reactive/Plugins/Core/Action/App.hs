module Reactive.Plugins.Core.Action.App
    ( toAction
    ) where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import           Event.Event
import           Event.UI                  (UIEvent (AppEvent))
import qualified Luna.Studio.React.Event.App           as App
import           React.Flux                (mousePageX, mousePageY)
import           Luna.Studio.Commands.Command (Command)
import qualified Luna.Studio.State.Global     as Global



toAction :: Event -> Maybe (Command Global.State ())
toAction (UI (AppEvent  (App.MouseMove evt))) = Just $ do
    let pos = Vector2 (mousePageX evt) (mousePageY evt)
    Global.mousePos .= pos
toAction _                                             = Nothing
