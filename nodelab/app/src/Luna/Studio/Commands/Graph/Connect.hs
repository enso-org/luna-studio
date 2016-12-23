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
import           Empire.API.Data.PortRef            (InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef            as PortRef

import qualified Object.Widget.Connection           as ConnectionModel

import qualified Luna.Studio.Commands.Batch         as BatchCmd
import           Luna.Studio.Commands.Command       (Command)
import           Luna.Studio.Data.Color             (Color (Color))
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.React.View.Global      (getConnectionPosition)
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
        let srcNodeId = src ^. PortRef.srcNodeId
            srcPortId = OutPortId (src ^. PortRef.srcPortId)
            dstNodeId = dst ^. PortRef.dstNodeId
            dstPortId = InPortId (dst ^. PortRef.dstPortId)
        mayPos <- getConnectionPosition srcNodeId srcPortId dstNodeId dstPortId
        withJust mayPos $ \(srcPos, dstPos) -> Global.withNodeEditor $ Store.modifyM_ $ do

            let color = Color 5 --TODO[react] get proper color
                connection = ConnectionModel.Connection connectionId True srcPos dstPos (dst ^. withArrow) color def
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
        let srcNodeId = connection ^. Connection.src . PortRef.srcNodeId
            srcPortId = OutPortId (connection ^. Connection.src . PortRef.srcPortId)
            dstNodeId = connection ^. Connection.dst . PortRef.dstNodeId
            dstPortId = InPortId (connection ^. Connection.dst . PortRef.dstPortId)
        mayPos <- getConnectionPosition srcNodeId srcPortId dstNodeId dstPortId
        withJust mayPos $ \(from', to') -> flip Store.modifyM_ connectionRef $ do
            ConnectionModel.visible   .= True
            ConnectionModel.from      .= from'
            ConnectionModel.to        .= to'
            ConnectionModel.arrow     .= (connection ^. Connection.dst) ^. withArrow
            ConnectionModel.color     .= Color 5 --TODO[react] get proper color
            ConnectionModel.highlight .= def
