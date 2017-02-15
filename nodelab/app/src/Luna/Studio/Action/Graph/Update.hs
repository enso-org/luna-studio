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
import           Luna.Studio.Action.Geometry.Connection (createConnectionModel)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph


updateConnections :: Command Global.State ()
updateConnections = do
    connections <- uses (Global.graph . Graph.connectionsMap) Map.elems
    mapM_ (updateConnection . view Connection.dst) connections

updateConnectionsForNodes :: [NodeId] -> Command Global.State ()
updateConnectionsForNodes nodeIds = do
    graph <- use Global.graph
    let connectionsToUpdate = Graph.connectionsContainingNodes nodeIds graph
    mapM_ (updateConnection . view Connection.dst) connectionsToUpdate

updateConnection :: ConnectionId -> Command Global.State ()
updateConnection connId = do
    mayConnection <- preuse $ Global.graph . Graph.connectionsMap . ix connId
    withJust mayConnection $ \conn -> do
        mayConnectionModel <- createConnectionModel conn
        mayConnToUpdate    <- Global.getConnection connId
        when (mayConnectionModel /= mayConnToUpdate) $
            Global.modifyNodeEditor $ NodeEditor.connections . at connId .= mayConnectionModel
