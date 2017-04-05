{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TypeFamilies   #-}
module Luna.Studio.Action.State.NodeEditor where

import           Control.Arrow                                ((&&&))
import qualified Control.Monad.State                          as M
import           Control.Monad.Trans.Maybe                    (MaybeT (MaybeT), runMaybeT)
import qualified Data.HashMap.Strict                          as HashMap
import qualified Data.Map.Lazy                                as Map
import           Data.Monoid                                  (First (First), getFirst)
import qualified Data.Set                                     as Set

import           Empire.API.Data.MonadPath                    (MonadPath)
import qualified Empire.API.Data.Node                         as Empire
import           Empire.API.Data.Port                         (OutPort (All), _WithDefault)
import           Empire.API.Data.PortDefault                  (PortDefault)
import           Empire.API.Data.PortRef                      (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef (OutPortRef))
import qualified Empire.API.Data.PortRef                      as PortRef
import           Luna.Studio.Action.Command                   (Command)
import           Luna.Studio.Action.State.App                 (get, modify)
import qualified Luna.Studio.Action.State.Internal.NodeEditor as Internal
import           Luna.Studio.Batch.Workspace                  (nodeSearcherData)
import           Luna.Studio.Data.Graph                       (Graph (Graph))
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App                  (nodeEditor)
import           Luna.Studio.React.Model.Connection           (Connection, ConnectionId, ConnectionsMap, CurrentConnection, connectionId,
                                                               containsNode, containsPortRef, dstNodeLoc, srcNodeLoc, toConnectionsMap)
import           Luna.Studio.React.Model.Node                 (ExpressionNode, Node (Expression, Sidebar), NodeLoc, SidebarNode, nodeLoc,
                                                               ports, toExpressionNodesMap, toNodesList, toSidebarNodesMap, _Expression,
                                                               _Sidebar)
import           Luna.Studio.React.Model.Node.ExpressionNode  (isSelected)
import qualified Luna.Studio.React.Model.Node.ExpressionNode  as ExpressionNode
import           Luna.Studio.React.Model.NodeEditor           (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor           as NE
import           Luna.Studio.React.Model.Port                 (Port, state, valueType)
import           Luna.Studio.React.Model.Searcher             (Searcher)
import           Luna.Studio.State.Global                     (State, workspace)
import           Text.ScopeSearcher.Item                      (Items, isElement, items)


getNodeEditor :: Command State NodeEditor
getNodeEditor = get nodeEditor

modifyNodeEditor :: M.State NodeEditor r -> Command State r
modifyNodeEditor = modify nodeEditor

resetGraph :: Command State ()
resetGraph = modifyNodeEditor $ do
    NE.expressionNodes     .= def
    NE.sidebarNodes        .= def
    NE.monads              .= def
    NE.connections         .= def
    NE.visualizations      .= def
    NE.connectionPen       .= def
    NE.selectionBox        .= def
    NE.searcher            .= def

separateSubgraph :: [NodeLoc] -> Command State Graph
separateSubgraph nodeLocs = do
    let idSet = Set.fromList nodeLocs
        inSet = flip Set.member idSet
        mkMap = HashMap.fromList . map (view nodeLoc &&& id)
    nodes' <- HashMap.filterWithKey (inSet .: const) . mkMap <$> getExpressionNodes
    conns' <- HashMap.filter (inSet . view dstNodeLoc) <$> getConnectionsMap
    return $ Graph (HashMap.map convert nodes') (HashMap.map convert conns')

addNode :: Node -> Command State ()
addNode (Expression node) = addExpressionNode node
addNode (Sidebar    node) = addSidebarNode node

addExpressionNode :: ExpressionNode -> Command State ()
addExpressionNode node = Internal.addNodeRec NE.expressionNodes ExpressionNode.expressionNodes (node ^. nodeLoc) node

addSidebarNode :: SidebarNode -> Command State ()
addSidebarNode node = Internal.addNodeRec NE.sidebarNodes ExpressionNode.sidebarNodes (node ^. nodeLoc) node

getNode :: NodeLoc -> Command State (Maybe Node)
getNode nl = do
    mayExpressionNode <- Expression `fmap2` getExpressionNode nl
    if isJust mayExpressionNode
        then return mayExpressionNode
        else Sidebar `fmap2` getSidebarNode nl

getNodes :: Command State [Node]
getNodes = toNodesList <$> getExpressionNodes <*> getSidebarNodes

getSidebarNode :: NodeLoc -> Command State (Maybe SidebarNode)
getSidebarNode nl = NE.getSidebarNode nl <$> getNodeEditor

getSidebarNodes :: Command State [SidebarNode]
getSidebarNodes = view NE.sidebarNodesRecursive <$> getNodeEditor

getExpressionNode :: NodeLoc -> Command State (Maybe ExpressionNode)
getExpressionNode nl = NE.getExpressionNode nl <$> getNodeEditor

getExpressionNodes :: Command State [ExpressionNode]
getExpressionNodes = view NE.expressionNodesRecursive <$> getNodeEditor

modifyExpressionNode :: Monoid r => NodeLoc -> M.State ExpressionNode r -> Command State r
modifyExpressionNode = Internal.modifyNodeRec NE.expressionNodes ExpressionNode.expressionNodes

modifyExpressionNodes_ :: M.State ExpressionNode () -> Command State ()
modifyExpressionNodes_ = void . modifyExpressionNodes

modifyExpressionNodes :: M.State ExpressionNode r -> Command State [r]
modifyExpressionNodes modifier = do
    nodeLocs <- view ExpressionNode.nodeLoc `fmap2` getExpressionNodes --FIXME it can be done faster
    catMaybes . map getFirst <$> forM nodeLocs (flip modifyExpressionNode $ (fmap (First . Just) modifier))

modifySidebarNode :: Monoid r => NodeLoc -> M.State SidebarNode r -> Command State r
modifySidebarNode = Internal.modifyNodeRec NE.sidebarNodes ExpressionNode.sidebarNodes

removeNode :: NodeLoc -> Command State ()
removeNode nl = do
    Internal.removeNodeRec NE.sidebarNodes    ExpressionNode.sidebarNodes    nl
    Internal.removeNodeRec NE.expressionNodes ExpressionNode.expressionNodes nl

getSelectedNodes :: Command State [ExpressionNode]
getSelectedNodes = filter (view isSelected) <$> getExpressionNodes

updateNodes :: [Node] -> Command State ()
updateNodes nodes = do
    updateExpressionNodes $ nodes ^.. traverse . _Expression
    updateSidebarNodes    $ nodes ^.. traverse . _Sidebar

updateSidebarNodes :: [SidebarNode] -> Command State ()
updateSidebarNodes update = modifyNodeEditor $ NE.sidebarNodes .= toSidebarNodesMap update

updateExpressionNodes :: [ExpressionNode] -> Command State ()
updateExpressionNodes update = modifyNodeEditor $ NE.expressionNodes .= toExpressionNodesMap update

addConnection :: Connection -> Command State ()
addConnection conn = modifyNodeEditor $ NE.connections . at connId ?= conn where
    connId = conn ^. connectionId

getConnection :: ConnectionId -> Command State (Maybe Connection)
getConnection connId = HashMap.lookup connId <$> getConnectionsMap

getConnections :: Command State [Connection]
getConnections = HashMap.elems <$> getConnectionsMap

getConnectionsMap :: Command State ConnectionsMap
getConnectionsMap = view NE.connections <$> getNodeEditor

getConnectionsBetweenNodes :: NodeLoc -> NodeLoc -> Command State [Connection]
getConnectionsBetweenNodes nl1 nl2 =
    filter (\conn -> containsNode nl1 conn && containsNode nl2 conn) <$> getConnections

getConnectionsContainingNode :: NodeLoc -> Command State [Connection]
getConnectionsContainingNode nl = filter (containsNode nl) <$> getConnections

getConnectionsContainingNodes :: [NodeLoc] -> Command State [Connection]
getConnectionsContainingNodes nodeLocs = filter containsNode' <$> getConnections where
    nodeLocsSet        = Set.fromList nodeLocs
    containsNode' conn = Set.member (conn ^. srcNodeLoc) nodeLocsSet
                      || Set.member (conn ^. dstNodeLoc) nodeLocsSet

getConnectionsContainingPortRef :: AnyPortRef -> Command State [Connection]
getConnectionsContainingPortRef portRef = filter (containsPortRef portRef) <$> getConnections

getConnectionsFromNode :: NodeLoc -> Command State [Connection]
getConnectionsFromNode nl = filter (\conn -> conn ^. srcNodeLoc == nl) <$> getConnections

getConnectionsToNode :: NodeLoc -> Command State [Connection]
getConnectionsToNode nl = filter (\conn -> conn ^. dstNodeLoc == nl) <$> getConnections

modifyConnection :: Monoid r => ConnectionId -> M.State Connection r -> Command State r
modifyConnection connId = modify (nodeEditor . NE.connections . at connId) . zoom traverse

removeConnection :: ConnectionId -> Command State ()
removeConnection connId = modifyNodeEditor $ NE.connections . at connId .= Nothing

updateConnections :: [Connection] -> Command State ()
updateConnections update = modifyNodeEditor $ NE.connections .= toConnectionsMap update


getMonads :: Command State [MonadPath]
getMonads = view NE.monads <$> getNodeEditor

updateMonads :: [MonadPath] -> Command State ()
updateMonads update = modifyNodeEditor $ NE.monads .= update


modifyCurrentConnections :: M.State [CurrentConnection] r -> Command State r
modifyCurrentConnections = modify (nodeEditor . NE.currentConnections)


getSearcher :: Command State (Maybe Searcher)
getSearcher = view NE.searcher <$> getNodeEditor

modifySearcher :: Monoid r => M.State Searcher r -> Command State r
modifySearcher = modify (nodeEditor . NE.searcher) . zoom traverse

globalFunctions :: Items a -> Items a
globalFunctions = Map.filter isElement

getNodeSearcherData :: Command State (Items Empire.Node)
getNodeSearcherData = do
    completeData <- use $ workspace . nodeSearcherData
    selected     <- getSelectedNodes
    mscope       <- case selected of
        [node] -> (fmap . fmap) (convert . view valueType) $ getPort (OutPortRef (node ^. nodeLoc) All)
        _      -> return Nothing
    return $ case mscope of
        Nothing -> completeData
        Just tn -> Map.union (globalFunctions scope) (globalFunctions completeData) where
            scope = fromMaybe mempty $ completeData ^? ix tn . items

class NodeEditorElementId a where
    inGraph :: a -> Command State Bool
instance NodeEditorElementId NodeLoc where
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
    node <- MaybeT . getNode $ portRef ^. PortRef.nodeLoc
    fromJustM $ node ^? ports . ix (portRef ^. PortRef.portId)

getPortDefault :: AnyPortRef -> Command State (Maybe PortDefault)
getPortDefault portRef = maybe Nothing (\mayPort -> mayPort ^? state . _WithDefault) <$> getPort portRef
