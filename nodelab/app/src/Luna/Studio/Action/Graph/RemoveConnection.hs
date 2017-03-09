module Luna.Studio.Action.Graph.RemoveConnection
     ( localRemoveConnection
     , removeConnection
     , removeConnectionsBetweenNodes
     ) where

import           Luna.Studio.Prelude

import qualified Data.HashMap.Strict                as HashMap
import           Empire.API.Data.Connection         (Connection, ConnectionId)
import qualified Empire.API.Data.Connection         as Connection
import           Empire.API.Data.Node               (NodeId)
import qualified Luna.Studio.Action.Batch           as Batch
import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import           Luna.Studio.State.Graph            (connectionToNodeIds)
import qualified Luna.Studio.State.Graph            as Graph


removeConnection :: ConnectionId -> Command State ()
removeConnection connId = whenM (localRemoveConnection connId) $
    Batch.removeConnection connId

localRemoveConnection :: ConnectionId -> Command State Bool
localRemoveConnection connectionId = do
    mayConn <- use $ Global.graph . Graph.connectionsMap . at connectionId
    withJust mayConn $ const $ do
        Global.graph %= Graph.removeConnection connectionId
        Global.modifyNodeEditor $ NodeEditor.connections . at connectionId .= Nothing
    return $ isJust mayConn

removeConnectionsBetweenNodes :: NodeId -> NodeId -> Command State ()
removeConnectionsBetweenNodes n1 n2 = do
    connMap <- use $ Global.graph . Graph.connectionsMap . to HashMap.elems
    let shouldRemove :: Connection -> Bool
        shouldRemove conn = connectionToNodeIds conn `elem` [(n1, n2), (n2, n1)]
    mapM_ removeConnection $ map (view Connection.dst) $ filter shouldRemove connMap
