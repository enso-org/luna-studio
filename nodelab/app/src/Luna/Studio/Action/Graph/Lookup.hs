-- TODO: SROGI REFACTOR
module Luna.Studio.Action.Graph.Lookup
    ( allNodeIds
    , allNodes
    , edgeNodes
    , allConnectionModels
    , getPort
    ) where


import           Control.Monad.Trans.Maybe          (runMaybeT)
import qualified Data.HashMap.Lazy                  as HashMap
import           Empire.API.Data.Node               (NodeId)
import           Empire.API.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef            as PortRef
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection (Connection)
import           Luna.Studio.React.Model.Node       (Node)
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Model.Port       as PortModel
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global



allNodeIds :: Command State [NodeId]
allNodeIds = map (view Node.nodeId) <$> allNodes

allNodes :: Command State [Node]
allNodes = filter (not . Node.isEdge) <$> view (NodeEditor.nodes . to HashMap.elems) <$> Global.getNodeEditor

edgeNodes :: Command State [Node]
edgeNodes = filter Node.isEdge <$> view (NodeEditor.nodes . to HashMap.elems) <$> Global.getNodeEditor

allConnectionModels :: Command State [Connection]
allConnectionModels = view (NodeEditor.connections . to HashMap.elems) <$> Global.getNodeEditor

class HasPort a where
    getPort :: a -> Command State (Maybe PortModel.Port)
instance HasPort InPortRef where
    getPort portRef = getPortFromAnyPortRef $ InPortRef' portRef
instance HasPort OutPortRef where
    getPort portRef = getPortFromAnyPortRef $ OutPortRef' portRef
instance HasPort AnyPortRef where
    getPort = getPortFromAnyPortRef

getPortFromAnyPortRef :: AnyPortRef -> Command State (Maybe PortModel.Port)
getPortFromAnyPortRef portRef = runMaybeT $ do
    Just node <- lift $ Global.getNode $ portRef ^. PortRef.nodeId
    fromJustM $ node ^? Node.ports . ix (portRef ^. PortRef.portId)
