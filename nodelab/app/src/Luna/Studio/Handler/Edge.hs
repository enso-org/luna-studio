module Luna.Studio.Handler.Edge where

import           Luna.Studio.Action.Basic     (addPort, removePort, toggleEdgeMode)
import           Luna.Studio.Action.Command   (Command)
import qualified Luna.Studio.Action.Edge      as Edge
import           Luna.Studio.Event.Event      (Event (UI))
import           Luna.Studio.Event.UI         (UIEvent (AppEvent, EdgeEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App  as App
import qualified Luna.Studio.React.Event.Edge as Edge
import           Luna.Studio.State.Action     (Action (continue))
import           Luna.Studio.State.Global     (State)


handle :: Event -> Maybe (Command State ())
handle (UI (EdgeEvent (Edge.ToggleEdgeMode nl)))              = Just $ toggleEdgeMode nl
handle (UI (EdgeEvent (Edge.RemovePort portRef)))             = Just $ removePort portRef
handle (UI (EdgeEvent (Edge.AddPort    portRef)))             = Just $ addPort portRef
handle (UI (AppEvent  (App.MouseMove   evt _)))               = Just $ Edge.handleAppMove evt
handle (UI (EdgeEvent (Edge.MouseMove  evt nodeLoc)))         = Just $ Edge.handleEdgeMove evt nodeLoc
handle (UI (AppEvent  (App.MouseUp     evt)))                 = Just $ continue $ Edge.handleMouseUp evt
handle (UI (EdgeEvent (Edge.PortNameStartEdit portRef)))      = Just $ Edge.startPortNameEdit  portRef
handle (UI (EdgeEvent (Edge.PortNameApply     portRef name))) = Just $ Edge.finishPortNameEdit portRef name
handle (UI (EdgeEvent (Edge.PortNameDiscard   portRef)))      = Just $ Edge.cancelPortNameEdit portRef
handle _                                                      = Nothing
