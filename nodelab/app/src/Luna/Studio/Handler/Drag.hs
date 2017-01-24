module Luna.Studio.Handler.Drag
    ( toAction
    ) where

import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Action.Drag      (drag, startDrag, stopDrag)
import           Luna.Studio.Event.Event      (Event (UI))
import qualified Luna.Studio.Event.Mouse      as Mouse
import           Luna.Studio.Event.UI         (UIEvent (AppEvent, NodeEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App  as App
import qualified Luna.Studio.React.Event.Node as Node
import           Luna.Studio.State.Action     (Action (continue))
import           Luna.Studio.State.Global     (State)


toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.MouseDown evt nodeId))) = Just $ when shouldProceed $ startDrag nodeId evt shouldSnap  where
    shouldProceed = Mouse.withoutMods evt Mouse.leftButton || Mouse.withShift evt Mouse.leftButton
    shouldSnap    = Mouse.withoutMods evt Mouse.leftButton
toAction (UI (AppEvent  (App.MouseMove evt))) = Just $ continue $ drag evt shouldSnap where
    shouldSnap = Mouse.withoutMods evt Mouse.leftButton
toAction (UI (AppEvent  (App.MouseUp evt)))   = Just $ continue $ stopDrag evt
toAction _                                    = Nothing
