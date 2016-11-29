module Reactive.Plugins.Core.Action.Node where

import Utils.PreludePlus
import           Reactive.Commands.Command       (Command)
import Event.Event (Event(UI))
import Event.UI (UIEvent(NodeEvent))
import           Reactive.State.Global           (State)
import qualified React.Store.Node as Node


toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent Node.OnClick)) = Just $ print "ONCLICK"
toAction _   = Nothing
