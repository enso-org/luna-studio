module Reactive.Plugins.Core.Action.Node where

import           React.Flux                (mouseCtrlKey, mouseMetaKey)

import           Event.Event               (Event (UI))
import           Event.UI                  (UIEvent (NodeEvent))
import qualified React.Store.Node          as Node
import           Reactive.Commands.Command (Command)
import qualified Reactive.Commands.Node    as Node
import qualified UI.Handlers.Node          as Node
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph
import           Utils.PreludePlus



--TODO[react] merge Drag in
toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.Enter      nodeId))) = Just $ mapM_ Node.tryEnter =<< preuse (Global.graph . Graph.nodesMap . ix nodeId)
toAction (UI (NodeEvent (Node.Select evt nodeId))) = Just $ mapM_ (Node.selectNode (mouseCtrlKey evt || mouseMetaKey evt)) =<< Global.getNode nodeId
toAction _   = Nothing
