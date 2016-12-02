module Reactive.Plugins.Core.Action.Node where

import           Event.Event               (Event (UI))
import           Event.UI                  (UIEvent (NodeEvent))
import qualified React.Store.Node          as Node
import           Reactive.Commands.Command (Command)
import qualified Reactive.Commands.Node    as Node
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph
import           Utils.PreludePlus



toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent Node.OnClick)) = Just $ print "ONCLICK"
toAction (UI (NodeEvent (Node.Drag pos nodeId))) = Just $ print (pos, nodeId)
toAction (UI (NodeEvent (Node.Enter nodeId))) = Just $ mapM_ Node.tryEnter =<< preuse (Global.graph . Graph.nodesMap . ix nodeId)
toAction _   = Nothing
