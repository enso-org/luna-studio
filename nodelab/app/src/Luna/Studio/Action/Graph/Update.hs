module Luna.Studio.Action.Graph.Update
    ( updateNodeZOrder
    , updateConnections
    , updateConnection
    , updateConnectionsForNodes
    ) where


import qualified Data.HashMap.Strict                    as Map
import           Data.Ord                               (comparing)
import           Empire.API.Data.Connection             (ConnectionId)
import qualified Empire.API.Data.Connection             as Connection
import           Empire.API.Data.Node                   (NodeId)
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Connect.Color       (getConnectionColor)
import           Luna.Studio.Action.Geometry.Connection (getConnectionPosition)
import           Luna.Studio.Action.Graph.Connect       (localConnectNodes)
import           Luna.Studio.Action.Graph.Lookup        (allNodes)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node           as Node
import qualified Luna.Studio.React.Model.Node           as Model
import           Luna.Studio.React.Store                (ref, widget)
import qualified Luna.Studio.React.Store                as Store
import           Luna.Studio.State.Global               (State)
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph
import qualified Object.Widget.Connection               as ConnectionModel


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

nats :: [Integer]
nats = [1..]

updateNodeZOrder :: Command State ()
updateNodeZOrder = do
    nodes <- mapM Store.get' =<< allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        sortedRefs  = view ref <$> sortedNodes
    forM_ (zip sortedRefs nats) $ \(nRef, idx) -> do
        let newZPos = negate $ (fromIntegral idx) / 100.0
        Store.modify_ (Node.zPos .~ newZPos) nRef
