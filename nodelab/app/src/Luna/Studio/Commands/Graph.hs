module Luna.Studio.Commands.Graph
    ( allNodes
    , allNodes'
    , focusNode
    , getNode
    , getPort
    , getConnection
    -- , updateConnection
    -- , updateConnections
    -- , updateConnectionsForNodes
    , updateNodeZOrder
    ) where


import           Control.Monad.Trans.Maybe          (runMaybeT)
import qualified Data.HashMap.Lazy                  as HashMap
import           Data.Ord                           (comparing)
import qualified Data.Set                           as Set
import           Luna.Studio.Data.Angle
import           Luna.Studio.Data.Vector            (Vector2 (Vector2), lengthSquared)
import           Luna.Studio.Prelude

import           Luna.Studio.React.Model.Node       (Node)
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.React.Store            (Ref, WRef, ref, widget)
import qualified Luna.Studio.React.Store            as Store

import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.Port       as PortModel
import qualified Object.Widget.Connection           as ConnectionModel

import           Luna.Studio.Commands.Command       (Command)
import           Luna.Studio.Data.Color             (vtToColor)
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph

import           Empire.API.Data.Connection         (ConnectionId)
import qualified Empire.API.Data.Connection         as Connection
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as NodeAPI
import qualified Empire.API.Data.Port               as Port
import           Empire.API.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef            as PortRef



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

nats :: [Integer]
nats = [1..]

focusNode :: Ref Node -> Command State ()
focusNode nodeRef = do
    node <- Store.get' nodeRef
    nodes <- mapM Store.get' =<< allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        equalFst a b = a ^. widget == b ^. widget
        newOrderNodes = node : deleteBy equalFst node sortedNodes
        newOrderRefs  = view ref <$> newOrderNodes
    forM_ (zip newOrderRefs nats) $ \(nRef, idx) -> do
        let newZPos = negate $ (fromIntegral idx) / 100.0
        Store.modify_ (Node.zPos .~ newZPos) nRef

updateNodeZOrder :: Command State ()
updateNodeZOrder = do
    nodes <- mapM Store.get' =<< allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        sortedRefs  = view ref <$> sortedNodes
    forM_ (zip sortedRefs nats) $ \(nRef, idx) -> do
        let newZPos = negate $ (fromIntegral idx) / 100.0
        Store.modify_ (Node.zPos .~ newZPos) nRef
