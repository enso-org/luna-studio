module Luna.Studio.Action.Graph.Disconnect
     ( localRemoveConnections
     , removeConnections
     ) where

import           Luna.Studio.Prelude

import           Empire.API.Data.Connection         (ConnectionId)
import qualified Luna.Studio.Action.Batch           as BatchCmd
import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph


localRemoveConnections :: [ConnectionId] -> Command State ()
localRemoveConnections connectionIds = do
    Global.withNodeEditor $ Store.modifyM_ $
        forM_ connectionIds $ \ connectionId ->
            NodeEditor.connections . at connectionId .= Nothing
    Global.graph %= Graph.removeConnections connectionIds

removeConnections :: [ConnectionId] -> Command State ()
removeConnections = mapM_ BatchCmd.disconnectNodes
