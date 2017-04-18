{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
module Luna.Studio.Action.State.NodeEditor where

import           Control.Arrow                                ((&&&))
import qualified Control.Monad.State                          as M
import           Control.Monad.Trans.Maybe                    (MaybeT (MaybeT), runMaybeT)
import qualified Data.HashMap.Strict                          as HashMap
import qualified Data.Map.Lazy                                as Map
import           Data.Monoid                                  (First (First), getFirst)
import qualified Data.Set                                     as Set
import           Luna.Studio.React.Model.Layout               (Layout, Scene)
import qualified Luna.Studio.React.Model.Layout               as Scene

import           Empire.API.Data.MonadPath                    (MonadPath)
import qualified Empire.API.Data.Node                         as Empire
import           Empire.API.Data.Port                         (_WithDefault)
import           Empire.API.Data.PortDefault                  (PortDefault)
import           Empire.API.Data.PortRef                      (AnyPortRef, InPortRef, OutPortRef (OutPortRef))
import qualified Empire.API.Data.PortRef                      as PortRef
import           Luna.Studio.Action.Command                   (Command)
import           Luna.Studio.Action.State.App                 (get, modify)
import qualified Luna.Studio.Action.State.Internal.NodeEditor as Internal
import           Luna.Studio.Batch.Workspace                  (nodeSearcherData)
import           Luna.Studio.Data.CameraTransformation        (CameraTransformation)
import           Luna.Studio.Data.Graph                       (Graph (Graph))
import           Luna.Prelude
import           Luna.Studio.React.Model.App                  (nodeEditor)
import           Luna.Studio.React.Model.Connection           (Connection, ConnectionId, ConnectionsMap, CurrentConnection, connectionId,
                                                               containsNode, containsPortRef, dstNodeLoc, srcNodeLoc, toConnectionsMap)
import           Luna.Studio.React.Model.Node                 (InputNode, Node (Expression, Input, Output), NodeLoc, OutputNode, lookupPort,
                                                               nodeLoc, toNodesMap)
import           Luna.Studio.React.Model.Node.ExpressionNode  (ExpressionNode, isSelected)
import qualified Luna.Studio.React.Model.Node.ExpressionNode  as ExpressionNode
import           Luna.Studio.React.Model.NodeEditor           (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor           as NE
import           Luna.Studio.React.Model.Port                 (AnyPort, InPort, OutPort, state, valueType)
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
    NE.inputNode          .= def
    NE.outputNode         .= def
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
addNode (Input      node) = addInputNode node
addNode (Output     node) = addOutputNode node

addExpressionNode :: ExpressionNode -> Command State ()
addExpressionNode node = Internal.addNodeRec NE.expressionNodes ExpressionNode.expressionNodes (node ^. nodeLoc) node

addInputNode :: InputNode -> Command State ()
addInputNode node = Internal.setNodeRec NE.inputNode ExpressionNode.inputNode (node ^. nodeLoc) node

addOutputNode :: OutputNode -> Command State ()
addOutputNode node = Internal.setNodeRec NE.outputNode ExpressionNode.outputNode (node ^. nodeLoc) node

getNode :: NodeLoc -> Command State (Maybe Node)
getNode nl = do
    mayExpressionNode <- Expression `fmap2` getExpressionNode nl
    if isJust mayExpressionNode
        then return mayExpressionNode
        else do
            mayInputNode <- Input `fmap2` getInputNode nl
            if isJust mayInputNode
                then return mayInputNode
                else Output `fmap2` getOutputNode nl

getInputNode :: NodeLoc -> Command State (Maybe InputNode)
getInputNode nl = NE.getInputNode nl <$> getNodeEditor

getOutputNode :: NodeLoc -> Command State (Maybe OutputNode)
getOutputNode nl = NE.getOutputNode nl <$> getNodeEditor

getInputNodes :: Command State [InputNode]
getInputNodes = view NE.inputNodesRecursive <$> getNodeEditor

getOutputNodes :: Command State [OutputNode]
getOutputNodes = view NE.outputNodesRecursive <$> getNodeEditor

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

modifyInputNode :: Monoid r => NodeLoc -> M.State InputNode r -> Command State r
modifyInputNode = Internal.modifySidebarRec NE.inputNode ExpressionNode.inputNode

modifyOutputNode :: Monoid r => NodeLoc -> M.State OutputNode r -> Command State r
modifyOutputNode = Internal.modifySidebarRec NE.outputNode ExpressionNode.outputNode

removeNode :: NodeLoc -> Command State ()
removeNode nl = do
    Internal.unsetNodeRec  NE.outputNode      ExpressionNode.outputNode     nl
    Internal.unsetNodeRec  NE.inputNode       ExpressionNode.inputNode      nl
    Internal.removeNodeRec NE.expressionNodes ExpressionNode.expressionNodes nl

getSelectedNodes :: Command State [ExpressionNode]
getSelectedNodes = filter (view isSelected) <$> getExpressionNodes

updateInputNode :: Maybe InputNode -> Command State ()
updateInputNode update = modifyNodeEditor $ NE.inputNode .= update

updateOutputNode :: Maybe OutputNode -> Command State ()
updateOutputNode update = modifyNodeEditor $ NE.outputNode .= update

updateExpressionNodes :: [ExpressionNode] -> Command State ()
updateExpressionNodes update = modifyNodeEditor $ NE.expressionNodes .= toNodesMap update

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

getLayout :: Command State Layout
getLayout = view NE.layout <$> getNodeEditor

getScene :: Command State (Maybe Scene)
getScene = view Scene.scene <$> getLayout

getScreenTranform :: Command State CameraTransformation
getScreenTranform = view Scene.screenTransform <$> getLayout

globalFunctions :: Items a -> Items a
globalFunctions = Map.filter isElement

getNodeSearcherData :: Command State (Items Empire.ExpressionNode)
getNodeSearcherData = do
    completeData <- use $ workspace . nodeSearcherData
    selected     <- getSelectedNodes
    mscope       <- case selected of
        [node] -> (fmap . fmap) (convert . view valueType) $ getPort (OutPortRef (node ^. nodeLoc) [])
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

class GetPort a b | a -> b where
    getPort :: a -> Command State (Maybe b)

instance GetPort InPortRef InPort where
    getPort portRef = runMaybeT $
        MaybeT (getNode $ portRef ^. PortRef.nodeLoc) >>= fromJustM . flip lookupPort (portRef ^. PortRef.dstPortId)
instance GetPort OutPortRef OutPort where
    getPort portRef = runMaybeT $
        MaybeT (getNode $ portRef ^. PortRef.nodeLoc) >>= fromJustM . flip lookupPort (portRef ^. PortRef.srcPortId)
instance GetPort AnyPortRef AnyPort where
    getPort portRef = runMaybeT $ do
        MaybeT (getNode $ portRef ^. PortRef.nodeLoc) >>= fromJustM . flip lookupPort (portRef ^. PortRef.portId)

getPortDefault :: InPortRef -> Command State (Maybe PortDefault)
getPortDefault portRef = maybe Nothing (\mayPort -> mayPort ^? state . _WithDefault) <$> getPort portRef
