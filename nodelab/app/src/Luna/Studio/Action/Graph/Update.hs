module Luna.Studio.Action.Graph.Update
    ( updateConnections
    , updateConnection
    , updateConnectionsForNodes
    ) where


import qualified Data.HashMap.Strict                    as Map
import           Empire.API.Data.Connection             (ConnectionId)
import qualified Empire.API.Data.Connection             as Connection
import           Empire.API.Data.Node                   (NodeId)
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Connect.Color       (getConnectionColor)
import           Luna.Studio.Action.Geometry.Connection (getConnectionPosition)
import           Luna.Studio.Action.Graph.Connect       (localConnectNodes)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Connection     as ConnectionModel
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph


updateConnections :: Command Global.State ()
updateConnections = do
    connections <- uses (Global.graph . Graph.connectionsMap) Map.elems
    mapM_ (updateConnection . (view Connection.dst)) connections

updateConnectionsForNodes :: [NodeId] -> Command Global.State ()
updateConnectionsForNodes nodeIds = do
    graph <- use Global.graph
    let connectionsToUpdate = Graph.connectionsContainingNodes nodeIds graph
    mapM_ (updateConnection . (view Connection.dst)) connectionsToUpdate

updateConnection :: ConnectionId -> Command Global.State ()
updateConnection connId = do
    mayConnectionModel <- Global.getConnection connId
    mayConnection    <- preuse $ Global.graph . Graph.connectionsMap . ix connId
    case (mayConnectionModel, mayConnection) of
        (Just _, Just conn) -> do
            let src = conn ^. Connection.src
                dst = conn ^. Connection.dst
            mayPos <- getConnectionPosition src dst
            withJust mayPos $ \(from', to') -> do
                mayColor <- getConnectionColor src
                withJust mayColor $ \color -> Global.modifyConnection connId $ do
                    ConnectionModel.from      .= from'
                    ConnectionModel.to        .= to'
                    ConnectionModel.color     .= color
        (Nothing, Just conn) -> void $ localConnectNodes (conn ^. Connection.src) (conn ^. Connection.dst)
        _ -> return ()
