--TODO[react]:Rename module to sth more appropriate
module Luna.Studio.Commands.Graph.Connect
    ( batchConnectNodes
    , localConnectNodes
    , updateConnection
    , updateConnections
    , updateConnectionsForNodes
    ) where


import qualified Data.HashMap.Strict                as Map
import           Luna.Studio.Data.Vector            (fromTuple)
import           Luna.Studio.Prelude

import           Empire.API.Data.Connection         (Connection, ConnectionId)
import qualified Empire.API.Data.Connection         as Connection
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as Node
import qualified Empire.API.Data.NodeMeta           as NodeMeta
import qualified Empire.API.Data.Port               as Port
import           Empire.API.Data.PortRef            (InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef            as PortRef

import qualified Object.Widget.Connection           as ConnectionModel

import qualified Luna.Studio.Commands.Batch         as BatchCmd
import           Luna.Studio.Commands.Command       (Command)
import qualified Luna.Studio.React.Model.Node       as NodeModel
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph

import           UI.Instances                       ()


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
        nodesMap <- use $ Global.graph . Graph.nodesMap
        --TODO[react]: Find out if we need to this like updateConnection (using Refs)
        let srcNodeId = src ^. PortRef.srcNodeId
            dstNodeId = dst ^. PortRef.dstNodeId
            --TODO[react]: decide what to do when node is not present in nodesMap
            srcNodePos = fromTuple $ maybe def (view $ Node.nodeMeta . NodeMeta.position) (Map.lookup srcNodeId nodesMap)
            dstNodePos = fromTuple $ maybe def (view $ Node.nodeMeta . NodeMeta.position) (Map.lookup dstNodeId nodesMap)
        Global.withNodeEditor $ Store.modifyM_ $ do
            let connection = ConnectionModel.Connection connectionId True srcNodePos dstNodePos (dst ^. withArrow) def def
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
            dstNodeId = connection ^. Connection.dst . PortRef.dstNodeId
        maySrcNodeRef <- Global.getNode srcNodeId
        mayDstNodeRef <- Global.getNode dstNodeId
        --TODO[react]: decide what to do when one of nodeRefs does not exist
        case (maySrcNodeRef, mayDstNodeRef) of
            (Nothing, _)                       -> return ()
            (_, Nothing)                       -> return ()
            (Just srcNodeRef, Just dstNodeRef) -> do
                --TODO[react]: Find out correct values that should be put in those lines
                from <- view NodeModel.position <$> Store.get srcNodeRef
                to   <- view NodeModel.position <$> Store.get dstNodeRef
                let visible   = True
                    arrow     = (connection ^. Connection.dst) ^. withArrow
                    color     = def
                    highlight = def

                flip Store.modifyM_ connectionRef $ do
                    ConnectionModel.visible   .= visible
                    ConnectionModel.from      .= from
                    ConnectionModel.to        .= to
                    ConnectionModel.arrow     .= arrow
                    ConnectionModel.color     .= color
                    ConnectionModel.highlight .= highlight
