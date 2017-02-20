module Luna.Studio.Handler.Edge where

import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Action.Edge      (addPort, endPortDrag, handleMove, removePort, restoreConnect)
import           Luna.Studio.Event.Event      (Event (UI))
import           Luna.Studio.Event.UI         (UIEvent (AppEvent, EdgeEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App  as App
import qualified Luna.Studio.React.Event.Edge as Edge
import           Luna.Studio.State.Action     (Action (continue))
import           Luna.Studio.State.Global     (State)


handle :: Event -> Maybe (Command State ())
handle (UI (EdgeEvent (Edge.MouseMove  evt nodeId))) = Just $ handleMove evt nodeId
handle (UI (EdgeEvent (Edge.RemovePort)))            = Just $ continue removePort
handle (UI (EdgeEvent (Edge.AddPort    nodeId)))     = Just $ addPort nodeId
handle (UI (AppEvent  (App.MouseMove   evt _)))      = Just $ continue $ restoreConnect evt
handle (UI (AppEvent  (App.MouseUp     _)))          = Just $ continue $ endPortDrag
handle _                                             = Nothing
