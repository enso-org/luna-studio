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
    whenM (localConnect srcPortRef dstPortRef) $ BatchCmd.connect src dst
connect src dst = BatchCmd.connect src dst

localConnect :: OutPortRef -> InPortRef -> Command Global.State Bool
localConnect src dst = localAddConnection $ Connection src dst

localAddConnection :: Connection -> Command Global.State Bool
localAddConnection connection = do
    let connectionId = connection ^. Connection.dst
    mayConnectionModel <- createConnectionModel connection
    case mayConnectionModel of
        Just connModel -> do
            zoom Global.graph $ Graph.addConnection connection
            Global.modifyNodeEditor $ NodeEditor.connections . at connectionId .= mayConnectionModel
            return True
        Nothing        -> return False
