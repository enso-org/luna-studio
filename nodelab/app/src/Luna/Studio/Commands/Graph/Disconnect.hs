module Luna.Studio.Commands.Graph.Disconnect
     ( disconnect
     , localDisconnect
     ) where

import           Luna.Studio.Prelude

import           Empire.API.Data.Connection         (Connection, ConnectionId)
import qualified Empire.API.Data.Connection         as Connection
import           Empire.API.Data.PortRef            (InPortRef, OutPortRef)
import qualified Luna.Studio.Commands.Batch         as BatchCmd
import           Luna.Studio.Commands.Command       (Command)
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph


localDisconnect :: [ConnectionId] -> Command State ()
localDisconnect connectionIds = do
    Global.withNodeEditor $ Store.modifyM_ $
        forM_ connectionIds $ \ connectionId ->
            NodeEditor.connections . at connectionId .= Nothing
    Global.graph %= Graph.removeConnections connectionIds

disconnect :: [ConnectionId] -> Command State ()
disconnect = mapM_ BatchCmd.disconnectNodes
