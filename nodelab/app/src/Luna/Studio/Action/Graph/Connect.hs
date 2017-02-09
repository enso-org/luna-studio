--TODO[react]:Rename module to sth more appropriate
module Luna.Studio.Action.Graph.Connect
    ( connectNodes
    , localConnectNodes
    , localAddConnection
    ) where


import           Empire.API.Data.Connection             (Connection (Connection), ConnectionId)
import qualified Empire.API.Data.Connection             as Connection
import           Empire.API.Data.Port                   (InPort (Self))
import           Empire.API.Data.PortRef                (InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef                as PortRef
import qualified Luna.Studio.Action.Batch               as BatchCmd
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Geometry.Connection (createConnectionModel)
import           Luna.Studio.Action.Port.Self           (addPortSelfIfExists)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph

connectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
connectNodes = BatchCmd.connectNodes

localConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ConnectionId
localConnectNodes src dst = localAddConnection $ Connection src dst

localAddConnection :: Connection -> Command Global.State ConnectionId
localAddConnection connection = do
    connectionId <- zoom Global.graph $ Graph.addConnection connection
    when (connection ^. Connection.dst . PortRef.dstPortId == Self) $ do
        let nodeId = connectionId ^. PortRef.dstNodeId
        mayNode <- Global.getNode nodeId
        withJust mayNode $ \node -> do
            newNode <- addPortSelfIfExists node
            Global.modifyNodeEditor $ NodeEditor.nodes . at nodeId ?= newNode
    mayConn <- view (NodeEditor.connections . at connectionId) <$> Global.getNodeEditor
    mayConnectionModel <- createConnectionModel connection
    when (mayConnectionModel /= mayConn) $ do
        Global.modifyNodeEditor $ NodeEditor.connections . at connectionId .= mayConnectionModel
    return connectionId
