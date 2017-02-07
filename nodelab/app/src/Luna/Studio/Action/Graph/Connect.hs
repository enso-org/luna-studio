--TODO[react]:Rename module to sth more appropriate
module Luna.Studio.Action.Graph.Connect
    ( connectNodes
    , localConnectNodes
    ) where


import           Data.Position                          (Position)
import           Empire.API.Data.Connection             (ConnectionId)
import           Empire.API.Data.Port                   (InPort (Self))
import           Empire.API.Data.PortRef                (InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef                as PortRef
import qualified Luna.Studio.Action.Batch               as BatchCmd
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Connect.Color       (getConnectionColor)
import           Luna.Studio.Action.Geometry.Connection (getConnectionPosition)
import           Luna.Studio.Action.Port.Self           (addPortSelfIfExists)
import           Luna.Studio.Data.Color                 (Color)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Connection     as ConnectionModel
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph

connectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
connectNodes = BatchCmd.connectNodes

addConnection :: ConnectionId -> Position -> Position -> Color -> Command Global.State ()
addConnection connId srcPos dstPos color = do
    mayConn <- view (NodeEditor.connections . at connId) <$> Global.getNodeEditor
    let connection = ConnectionModel.Connection connId srcPos dstPos color
    when (isNothing mayConn || (fromJust mayConn /= connection)) $ Global.modifyNodeEditor $
        NodeEditor.connections . at connId ?= connection

localConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ConnectionId
localConnectNodes src dst = do
    connectionId <- zoom Global.graph $ Graph.addConnection src dst
    when (dst ^. PortRef.dstPortId == Self) $ do
        let nodeId = connectionId ^. PortRef.dstNodeId
        mayNode <- Global.getNode nodeId
        withJust mayNode $ \node -> do
            newNode <- addPortSelfIfExists node
            Global.modifyNodeEditor $ NodeEditor.nodes . at nodeId ?= newNode
    mayPos   <- getConnectionPosition src dst
    mayColor <- getConnectionColor src
    withJust ((,) <$> mayPos <*> mayColor) $ \((srcPos, dstPos), color) ->
        addConnection connectionId srcPos dstPos color
    return connectionId
