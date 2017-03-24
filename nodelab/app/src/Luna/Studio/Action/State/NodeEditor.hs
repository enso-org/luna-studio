{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase     #-}
module Luna.Studio.Action.State.NodeEditor where

import qualified Control.Monad.State                         as M
import           Control.Monad.Trans.Maybe                   (MaybeT (MaybeT), runMaybeT)
import qualified Data.HashMap.Strict                         as HashMap
import qualified Data.Map.Lazy                               as Map
import qualified Data.Set                                    as Set

import           Empire.API.Data.MonadPath                   (MonadPath)
import qualified Empire.API.Data.Node                        as Node
import           Empire.API.Data.Port                        (OutPort (All), _WithDefault)
import           Empire.API.Data.PortDefault                 (PortDefault)
import           Empire.API.Data.PortRef                     (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef (OutPortRef))
import qualified Empire.API.Data.PortRef                     as PortRef
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.App                (get, modify)
import           Luna.Studio.Batch.Workspace                 (nodeSearcherData)
import           Luna.Studio.Data.Graph                      (Graph (Graph))
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App                 (nodeEditor)
import           Luna.Studio.React.Model.Connection          (Connection, ConnectionId, ConnectionsMap, CurrentConnection, connectionId,
                                                              containsNode, containsPortRef, dstNodeId, srcNodeId, toConnectionsMap)
import           Luna.Studio.React.Model.Node                (EdgeNode, EdgeNodesMap, ExpressionNode, ExpressionNodesMap,
                                                              Node (Edge, Expression), NodeId, NodesMap, nodeId, ports, toEdgeNodesMap,
                                                              toExpressionNodesMap, toNodesList, toNodesMap, _Edge, _Expression)
import           Luna.Studio.React.Model.Node.ExpressionNode (isSelected)
import           Luna.Studio.React.Model.NodeEditor
import           Luna.Studio.React.Model.Port                (Port, state, valueType)
import           Luna.Studio.React.Model.Searcher            (Searcher)
import           Luna.Studio.State.Global                    (State, workspace)
import           Text.ScopeSearcher.Item                     (Items, isElement, items)


getNodeEditor :: Command State NodeEditor
getNodeEditor = get nodeEditor

modifyNodeEditor :: M.State NodeEditor r -> Command State r
modifyNodeEditor = modify nodeEditor

resetGraph :: Command State ()
resetGraph = modifyNodeEditor $ do
    expressionNodes     .= def
    edgeNodes           .= def
    monads              .= def
    connections         .= def
    connectionPen       .= def
    selectionBox        .= def
    searcher            .= def
    visualizations      .= def

separateSubgraph :: [NodeId] -> Command State Graph
separateSubgraph nodeIds = do
    let idSet = Set.fromList nodeIds
        inSet = flip Set.member idSet
    nodes' <- HashMap.filterWithKey (inSet .: const)  <$> getExpressionNodesMap
    conns' <- HashMap.filter (inSet . view dstNodeId) <$> getConnectionsMap
    return $ Graph (HashMap.map convert nodes') (HashMap.map convert conns')

addNode :: Node -> Command State ()
addNode (Expression node) = addExpressionNode node
addNode (Edge edge)       = addEdgeNode edge

addExpressionNode :: ExpressionNode -> Command State ()
addExpressionNode node = modifyNodeEditor $ expressionNodes . at (node ^. nodeId) ?= node

addEdgeNode :: EdgeNode -> Command State ()
addEdgeNode node = modifyNodeEditor $ edgeNodes . at (node ^. nodeId) ?= node

getNode :: NodeId -> Command State (Maybe Node)
getNode nid = do
    mayExpressionNode <- fmap2 Expression $ getExpressionNode nid
    mayEdgeNode       <- fmap2 Edge       $ getEdgeNode nid
    if isJust mayExpressionNode
        then return mayExpressionNode
        else return mayEdgeNode

getNodes :: Command State [Node]
getNodes = toNodesList <$> getExpressionNodes <*> getEdgeNodes

getNodesMap :: Command State NodesMap
getNodesMap = toNodesMap <$> getExpressionNodes <*> getEdgeNodes

getEdgeNode :: NodeId -> Command State (Maybe EdgeNode)
getEdgeNode nid = HashMap.lookup nid <$> getEdgeNodesMap

getEdgeNodes :: Command State [EdgeNode]
getEdgeNodes = HashMap.elems <$> getEdgeNodesMap

getEdgeNodesMap :: Command State EdgeNodesMap
getEdgeNodesMap = view edgeNodes <$> getNodeEditor

getExpressionNode :: NodeId -> Command State (Maybe ExpressionNode)
getExpressionNode nid = HashMap.lookup nid <$> getExpressionNodesMap

getExpressionNodes :: Command State [ExpressionNode]
getExpressionNodes = HashMap.elems <$> getExpressionNodesMap

getExpressionNodesMap :: Command State ExpressionNodesMap
getExpressionNodesMap = view expressionNodes <$> getNodeEditor

modifyExpressionNode :: Monoid r => NodeId -> M.State ExpressionNode r -> Command State r
modifyExpressionNode nid = modify (nodeEditor . expressionNodes . at nid) . zoom traverse

modifyEdgeNode :: Monoid r => NodeId -> M.State EdgeNode r -> Command State r
modifyEdgeNode nid = modify (nodeEditor . edgeNodes . at nid) . zoom traverse

removeNode :: NodeId -> Command State ()
removeNode nid = modifyNodeEditor $ do
    expressionNodes . at nid .= Nothing
    edgeNodes       . at nid .= Nothing

getSelectedNodes :: Command State [ExpressionNode]
getSelectedNodes = filter (view isSelected) <$> getExpressionNodes

updateNodes :: [Node] -> Command State ()
updateNodes nodes = do
    updateExpressionNodes $ nodes ^.. traverse . _Expression
    updateEdgeNodes       $ nodes ^.. traverse . _Edge

updateEdgeNodes :: [EdgeNode] -> Command State ()
updateEdgeNodes update = modifyNodeEditor $ edgeNodes .= toEdgeNodesMap update

updateExpressionNodes :: [ExpressionNode] -> Command State ()
updateExpressionNodes update = modifyNodeEditor $ expressionNodes .= toExpressionNodesMap update

addConnection :: Connection -> Command State ()
addConnection conn = modifyNodeEditor $ connections . at connId ?= conn where
    connId = conn ^. connectionId

getConnection :: ConnectionId -> Command State (Maybe Connection)
getConnection connId = HashMap.lookup connId <$> getConnectionsMap

getConnections :: Command State [Connection]
getConnections = HashMap.elems <$> getConnectionsMap

getConnectionsMap :: Command State ConnectionsMap
getConnectionsMap = view connections <$> getNodeEditor

getConnectionsBetweenNodes :: NodeId -> NodeId -> Command State [Connection]
getConnectionsBetweenNodes nid1 nid2 =
    filter (\conn -> containsNode nid1 conn && containsNode nid2 conn) <$> getConnections

getConnectionsContainingNode :: NodeId -> Command State [Connection]
getConnectionsContainingNode nid = filter (containsNode nid) <$> getConnections

getConnectionsContainingNodes :: [NodeId] -> Command State [Connection]
getConnectionsContainingNodes nodeIds = filter containsNode' <$> getConnections where
    nodeIdsSet         = Set.fromList nodeIds
    containsNode' conn = Set.member (conn ^. srcNodeId) nodeIdsSet
                      || Set.member (conn ^. dstNodeId) nodeIdsSet

getConnectionsContainingPortRef :: AnyPortRef -> Command State [Connection]
getConnectionsContainingPortRef portRef = filter (containsPortRef portRef) <$> getConnections

getConnectionsToNode :: NodeId -> Command State [Connection]
getConnectionsToNode nid = filter (\conn -> conn ^. dstNodeId == nid) <$> getConnections

modifyConnection :: Monoid r => ConnectionId -> M.State Connection r -> Command State r
modifyConnection connId = modify (nodeEditor . connections . at connId) . zoom traverse

removeConnection :: ConnectionId -> Command State ()
removeConnection connId = modifyNodeEditor $ connections . at connId .= Nothing

updateConnections :: [Connection] -> Command State ()
updateConnections update = modifyNodeEditor $ connections .= toConnectionsMap update


getMonads :: Command State [MonadPath]
getMonads = view monads <$> getNodeEditor

updateMonads :: [MonadPath] -> Command State ()
updateMonads update = modifyNodeEditor $ monads .= update


modifyCurrentConnections :: M.State [CurrentConnection] r -> Command State r
modifyCurrentConnections = modify (nodeEditor . currentConnections)


getSearcher :: Command State (Maybe Searcher)
getSearcher = view searcher <$> getNodeEditor

modifySearcher :: Monoid r => M.State Searcher r -> Command State r
modifySearcher = modify (nodeEditor . searcher) . zoom traverse

globalFunctions :: Items a -> Items a
globalFunctions = Map.filter isElement

getNodeSearcherData :: Command State (Items Node.Node)
getNodeSearcherData = do
    completeData <- use $ workspace . nodeSearcherData
    selected     <- getSelectedNodes
    mscope       <- case selected of
        [node] -> (fmap . fmap) (convert . view valueType) $ getPort (OutPortRef (node ^. nodeId) All)
        _      -> return Nothing
    return $ case mscope of
        Nothing -> completeData
        Just tn -> Map.union (globalFunctions scope) (globalFunctions completeData) where
            scope = fromMaybe mempty $ completeData ^? ix tn . items

class NodeEditorElementId a where
    inGraph :: a -> Command State Bool
instance NodeEditorElementId NodeId where
    inGraph = fmap isJust . getNode
instance NodeEditorElementId ConnectionId where
    inGraph = fmap isJust . getConnection

class HasPort a where
    getPort :: a -> Command State (Maybe Port)
instance HasPort InPortRef where
    getPort portRef = getPortFromAnyPortRef $ InPortRef' portRef
instance HasPort OutPortRef where
    getPort portRef = getPortFromAnyPortRef $ OutPortRef' portRef
instance HasPort AnyPortRef where
    getPort = getPortFromAnyPortRef

getPortFromAnyPortRef :: AnyPortRef -> Command State (Maybe Port)
getPortFromAnyPortRef portRef = runMaybeT $ do
    node <- MaybeT . getNode $ portRef ^. PortRef.nodeId
    fromJustM $ node ^? ports . ix (portRef ^. PortRef.portId)

getPortDefault :: AnyPortRef -> Command State (Maybe PortDefault)
getPortDefault portRef = maybe Nothing (\mayPort -> mayPort ^? state . _WithDefault) <$> getPort portRef
