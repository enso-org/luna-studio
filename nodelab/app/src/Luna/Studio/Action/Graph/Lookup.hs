module Luna.Studio.Action.Graph.Lookup
    ( allNodes
    , allNodes'
    , getNode
    , getPort
    , getConnection
    ) where


import           Control.Monad.Trans.Maybe          (runMaybeT)
import qualified Data.HashMap.Lazy                  as HashMap
import           Empire.API.Data.Connection         (ConnectionId)
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as NodeAPI
import qualified Empire.API.Data.Port               as Port
import           Empire.API.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef            as PortRef
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node       (Node)
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Model.Port       as PortModel
import           Luna.Studio.React.Store            (Ref, WRef)
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph
import qualified Object.Widget.Connection           as ConnectionModel



allNodes :: Command State [Ref Node]
allNodes = Global.withNodeEditor $
    Store.use (NodeEditor.nodes . to HashMap.elems)

allNodes' :: Command State [WRef Node]
allNodes' = mapM Store.get' =<< allNodes

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
    Just node <- lift $ getNode $ portRef ^. PortRef.nodeId
    fromJustM $ node ^? Node.ports . ix portRef

class HasGraphPort a where
    getGraphPort :: a -> Command State (Maybe Port.Port)
instance HasGraphPort InPortRef where
    getGraphPort portRef = getGraphPortFromAnyPortRef $ InPortRef' portRef
instance HasGraphPort OutPortRef where
    getGraphPort portRef = getGraphPortFromAnyPortRef $ OutPortRef' portRef
instance HasGraphPort AnyPortRef where
    getGraphPort = getGraphPortFromAnyPortRef

getGraphPortFromAnyPortRef :: AnyPortRef -> Command State (Maybe Port.Port)
getGraphPortFromAnyPortRef portRef =
    preuse $ Global.graph . Graph.nodesMap . ix (portRef ^. PortRef.nodeId) . NodeAPI.ports . ix (portRef ^. PortRef.portId)

getNode :: NodeId -> Command State (Maybe Model.Node)
getNode nodeId = Global.withNode nodeId $ mapM Store.get

getConnection :: ConnectionId -> Command State (Maybe ConnectionModel.Connection)
getConnection connId = Global.withConnection connId $ mapM Store.get
