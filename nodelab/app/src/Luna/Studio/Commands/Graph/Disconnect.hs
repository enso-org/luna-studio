module Luna.Studio.Commands.Graph.Disconnect
     ( disconnectAll
     , localDisconnectAll
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


-- TODO[react]: Is "All" in name necessary?
localDisconnectAll :: [ConnectionId] -> Command State ()
localDisconnectAll connectionIds = do
    Global.withNodeEditor $ Store.modifyM_ $
        forM_ connectionIds $ \ connectionId ->
            NodeEditor.connections . at connectionId .= Nothing
    Global.graph           %= Graph.removeConnections connectionIds

connectionToRefs :: Connection -> (OutPortRef, InPortRef)
connectionToRefs conn = (conn ^. Connection.src, conn ^. Connection.dst)

disconnectAll :: [ConnectionId] -> Command State ()
disconnectAll connectionIds = do
    graph     <- use Global.graph
    let conns = catMaybes $ Graph.lookUpConnection graph <$> connectionIds
        refs  = connectionToRefs <$> conns
    mapM_ BatchCmd.disconnectNodes (snd <$> refs)
    localDisconnectAll connectionIds
