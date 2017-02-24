module Luna.Studio.Action.Graph.Update
    ( updateConnections
    , updateConnection
    , updateConnectionsForNodes
    , updateConnectionsForEdges
    , updateMonads
    , updateScene
    ) where


import qualified Data.HashMap.Strict                    as Map
import           Empire.API.Data.Connection             (ConnectionId)
import qualified Empire.API.Data.Connection             as Connection
import           Empire.API.Data.Node                   (NodeId)
import           Empire.API.Data.TypeRep                (TypeRep)
import           Luna.Studio.Action.Camera.Screen       (updateSceneOnly)
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Geometry.Connection (createConnectionModel)
import           Luna.Studio.Action.Graph.Lookup        (edgeNodes)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node           as Node
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import           Luna.Studio.State.Global               (State)
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph


updateConnections :: Command State ()
updateConnections = do
    connections <- uses (Global.graph . Graph.connectionsMap) Map.elems
    mapM_ (updateConnection . view Connection.dst) connections

updateConnectionsForNodes :: [NodeId] -> Command State ()
updateConnectionsForNodes nodeIds = do
    graph <- use Global.graph
    let connectionsToUpdate = Graph.connectionsContainingNodes nodeIds graph
    mapM_ (updateConnection . view Connection.dst) connectionsToUpdate

updateConnection :: ConnectionId -> Command State ()
updateConnection connId = do
    mayConnection  <- preuse $ Global.graph . Graph.connectionsMap . ix connId
    mayConnModel <- case mayConnection of
        Just conn -> createConnectionModel conn
        Nothing   -> return Nothing
    Global.modifyNodeEditor $ NodeEditor.connections . at connId .= mayConnModel

updateConnectionsForEdges :: Command Global.State ()
updateConnectionsForEdges = edgeNodes >>= updateConnectionsForNodes . map (view Node.nodeId)

updateMonads :: [(TypeRep, [NodeId])] -> Command State ()
updateMonads monads =
    Global.modifyNodeEditor $ NodeEditor.monads .= monads

updateScene :: Command State ()
updateScene = updateSceneOnly >> updateConnectionsForEdges
