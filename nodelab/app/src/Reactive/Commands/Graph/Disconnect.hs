module Reactive.Commands.Graph.Disconnect
     ( disconnectAll
     , localDisconnectAll
     ) where

import           Luna.Studio.Prelude

import           Empire.API.Data.Connection (Connection, ConnectionId)
import qualified Empire.API.Data.Connection as Connection
import           Empire.API.Data.PortRef    (InPortRef, OutPortRef)
import qualified React.Store                as Store
import qualified React.Model.NodeEditor     as NodeEditor
import qualified Reactive.Commands.Batch    as BatchCmd
import           Reactive.Commands.Command  (Command)
import           Reactive.State.Global      (State)
import qualified Reactive.State.Global      as Global
import qualified Reactive.State.Graph       as Graph



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
