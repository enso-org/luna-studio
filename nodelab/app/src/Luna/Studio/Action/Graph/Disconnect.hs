module Luna.Studio.Action.Graph.Disconnect
     ( localRemoveConnections
     , removeConnections
     , removeConnectionsBetweenNodes
     ) where

import           Luna.Studio.Prelude

import qualified Data.HashMap.Strict                as HashMap
import           Empire.API.Data.Connection         (Connection, ConnectionId)
import qualified Empire.API.Data.Connection         as Connection
import           Empire.API.Data.Node               (NodeId)
import qualified Luna.Studio.Action.Batch           as BatchCmd
import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import           Luna.Studio.State.Graph            (connectionToNodeIds)
import qualified Luna.Studio.State.Graph            as Graph


localRemoveConnections :: [ConnectionId] -> Command State ()
localRemoveConnections connectionIds = do
    Global.modifyNodeEditor $
        forM_ connectionIds $ \ connectionId ->
            NodeEditor.connections . at connectionId .= Nothing
    Global.graph %= Graph.removeConnections connectionIds

removeConnections :: [ConnectionId] -> Command State ()
removeConnections = mapM_ BatchCmd.disconnectNodes

removeConnectionsBetweenNodes :: NodeId -> NodeId -> Command State ()
removeConnectionsBetweenNodes n1 n2 = do
    connMap <- use $ Global.graph . Graph.connectionsMap . to HashMap.elems
    let shouldRemove :: Connection -> Bool
        shouldRemove conn = (connectionToNodeIds conn) `elem` [(n1, n2), (n2, n1)]
    removeConnections $ map (view Connection.dst) $ filter shouldRemove connMap
