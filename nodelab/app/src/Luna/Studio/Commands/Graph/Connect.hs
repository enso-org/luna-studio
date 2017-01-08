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
import           Luna.Studio.Data.Vector            (Position (Position), Vector2 (Vector2))
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
import qualified Luna.Studio.React.Store.Ref        as Ref
import           Luna.Studio.React.View.Connection  (getConnectionColor, getConnectionPosition)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph


batchConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
batchConnectNodes src dst = BatchCmd.connectNodes src dst

addConnectionRef :: ConnectionId -> Position -> Position -> Color -> Command Global.State ()
addConnectionRef connId srcPos dstPos color = Global.withNodeEditor $ Store.modifyM_ $ do
    let connection = ConnectionModel.Connection connId srcPos dstPos color
    connectionRef <- lift $ Store.create connection
    NodeEditor.connections . at connId ?= connectionRef

localConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ConnectionId
localConnectNodes src dst = do
    connectionId <- zoom Global.graph $ Graph.addConnection src dst
    prevConn <- Global.getConnection $ connectionId
    let newConnection = not $ isJust prevConn
    when newConnection $ do
        mayPos   <- getConnectionPosition src dst
        mayColor <- getConnectionColor src
        withJust ((,) <$> mayPos <*> mayColor) $ \((srcPos, dstPos), color) -> do
            addConnectionRef connectionId srcPos dstPos color
    return connectionId

updateConnections :: Command Global.State ()
updateConnections = do
    connections <- uses (Global.graph . Graph.connectionsMap) Map.elems
    print connections
    mapM_ updateConnection (map (view Connection.dst) connections)

updateConnectionsForNodes :: [NodeId] -> Command Global.State ()
updateConnectionsForNodes nodeIds = do
    graph <- use Global.graph
    let connectionsToUpdate = Graph.connectionsContainingNodes nodeIds graph
    mapM_ updateConnection (map (view Connection.dst) connectionsToUpdate)

updateConnection :: ConnectionId -> Command Global.State ()
updateConnection connId = do
    mayConnectionRef <- Global.getConnection $ connId
    mayConnection    <- preuse $ Global.graph . Graph.connectionsMap . ix connId
    case (mayConnectionRef, mayConnection) of
        (Just connRef, Just conn) -> do
            let src = conn ^. Connection.src
                dst = conn ^. Connection.dst
            mayPos <- getConnectionPosition src dst
            withJust mayPos $ \(from', to') -> do
                mayColor <- getConnectionColor src
                withJust mayColor $ \color -> flip Store.modifyM_ connRef $ do
                    ConnectionModel.from      .= from'
                    ConnectionModel.to        .= to'
                    ConnectionModel.color     .= color
        (Nothing, Just conn) -> void $ localConnectNodes (conn ^. Connection.src) (conn ^. Connection.dst)
        _ -> return ()
