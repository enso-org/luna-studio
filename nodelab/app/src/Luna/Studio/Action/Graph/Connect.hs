--TODO[react]:Rename module to sth more appropriate
module Luna.Studio.Action.Graph.Connect
    ( connect
    , localConnect
    , localAddConnection
    ) where


import           Empire.API.Data.Connection             (Connection (Connection), ConnectionId)
import qualified Empire.API.Data.Connection             as Connection
import           Empire.API.Data.Node                   (NodeId)
import           Empire.API.Data.Port                   (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef                (InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef                as PortRef
import qualified Luna.Studio.Action.Batch               as BatchCmd
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Geometry.Connection (createConnectionModel)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node           as Model
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import qualified Luna.Studio.React.Model.Port           as Model
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph



connect :: Either OutPortRef NodeId -> Either InPortRef NodeId -> Command Global.State ()
connect src@(Left srcPortRef) dst@(Left dstPortRef) =
    localConnect srcPortRef dstPortRef >> BatchCmd.connect src dst
connect src dst = BatchCmd.connect src dst

localConnect :: OutPortRef -> InPortRef -> Command Global.State ()
localConnect src dst = localAddConnection $ Connection src dst

localAddConnection :: Connection -> Command Global.State ()
localAddConnection connection = do
    connectionId <- zoom Global.graph $ Graph.addConnection connection
    when (connection ^. Connection.dst . PortRef.dstPortId == Self) $ do
        let nodeId = connectionId ^. PortRef.dstNodeId
        let portId = InPortId (connection ^. Connection.dst . PortRef.dstPortId)
        Global.modifyNode nodeId $ Model.ports . at portId . _Just . Model.visible .= True
    mayConnectionModel <- createConnectionModel connection
    Global.modifyNodeEditor $ NodeEditor.connections . at connectionId .= mayConnectionModel
