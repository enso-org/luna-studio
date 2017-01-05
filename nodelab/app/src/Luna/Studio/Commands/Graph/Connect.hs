--TODO[react]:Rename module to sth more appropriate
module Luna.Studio.Commands.Graph.Connect
    ( batchConnectNodes
    , localConnectNodes
    , updateConnection
    , updateConnections
    , updateConnectionsForNodes
    ) where


import qualified Data.HashMap.Strict                as Map
import           Luna.Studio.Prelude

import           Empire.API.Data.Connection         (Connection, ConnectionId)
import qualified Empire.API.Data.Connection         as Connection
import           Empire.API.Data.Node               (NodeId)
import           Empire.API.Data.Port               (PortId (..))
import qualified Empire.API.Data.Port               as Port
import           Empire.API.Data.PortRef            (InPortRef (..), OutPortRef (..), toAnyPortRef)
import qualified Empire.API.Data.PortRef            as PortRef

import qualified Object.Widget.Connection           as ConnectionModel

import qualified Luna.Studio.Commands.Batch         as BatchCmd
import           Luna.Studio.Commands.Command       (Command)
import           Luna.Studio.Data.Color             (Color (Color))
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.React.View.Global      (getConnectionColor, getConnectionPosition)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph



batchConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
batchConnectNodes src dst = BatchCmd.connectNodes src dst

withArrow :: Getter InPortRef Bool
withArrow = to $ \ref -> case ref of
    InPortRef _ Port.Self -> False
    _                     -> True

localConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ConnectionId
localConnectNodes src dst = do
    prevConn <- preuse $ Global.graph . Graph.connectionsMap . ix dst
    connectionId <- zoom Global.graph $ Graph.addConnection src dst
    let newConnection = not $ isJust prevConn
    when newConnection $ do
        mayPos <- getConnectionPosition src dst
        withJust mayPos $ \(srcPos, dstPos) -> do
            mayColor <- getConnectionColor src
            withJust mayColor $ \color -> Global.withNodeEditor $ Store.modifyM_ $ do
                let connection = ConnectionModel.Connection connectionId srcPos dstPos color
                connectionRef <- lift $ Store.create connection
                NodeEditor.connections . at dst ?= connectionRef
    return connectionId

updateConnections :: Command Global.State ()
updateConnections = do
    connection <- uses (Global.graph . Graph.connectionsMap) Map.elems
    mapM_ updateConnection connection

updateConnectionsForNodes :: [NodeId] -> Command Global.State ()
updateConnectionsForNodes nodeIds = do
    graph <- use Global.graph
    let connectionsToUpdate = Graph.connectionsContainingNodes nodeIds graph
    mapM_ updateConnection connectionsToUpdate

updateConnection :: Connection -> Command Global.State ()
updateConnection connection = do
    mayConnectionRef <- Global.getConnection $ connection ^. Connection.dst
    withJust mayConnectionRef $ \connectionRef -> do
        let src = connection ^. Connection.src
            dst = connection ^. Connection.dst
        mayPos <- getConnectionPosition src dst
        withJust mayPos $ \(from', to') -> do
            mayColor <- getConnectionColor src
            withJust mayColor $ \color -> flip Store.modifyM_ connectionRef $ do
                ConnectionModel.from      .= from'
                ConnectionModel.to        .= to'
                ConnectionModel.color     .= color
