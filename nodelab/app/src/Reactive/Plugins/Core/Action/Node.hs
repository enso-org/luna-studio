module Reactive.Plugins.Core.Action.Node where

import           React.Flux                (mouseShiftKey)

import           Event.Event               (Event (UI))
import           Event.UI                  (UIEvent (NodeEvent))
import qualified React.Store.Node          as Node
import           Reactive.Commands.Command (Command)
import qualified Reactive.Commands.Node    as Node
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph
import qualified UI.Handlers.Node          as Node
import           Utils.PreludePlus

--TODO[react] merge Drag in
toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.Click evt nodeId))) = Just $ Global.getNode nodeId >>= mapM_ (Node.selectNode (mouseShiftKey evt))
toAction (UI (NodeEvent (Node.Enter nodeId))) = Just $ mapM_ Node.tryEnter =<< preuse (Global.graph . Graph.nodesMap . ix nodeId)
toAction _   = Nothing
