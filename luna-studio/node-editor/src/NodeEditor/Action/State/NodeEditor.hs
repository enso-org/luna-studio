{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
module NodeEditor.Action.State.NodeEditor where

import           Control.Arrow                               ((&&&))
import qualified Control.Monad.State                         as M
import qualified Data.HashMap.Strict                         as HashMap
import qualified Data.Map.Lazy                               as Map
import           Data.Monoid                                 (First (First), getFirst)
import qualified Data.Set                                    as Set
import           NodeEditor.React.Model.Layout               (Layout, Scene)
import qualified NodeEditor.React.Model.Layout               as Scene

import           Common.Prelude
import           LunaStudio.Data.MonadPath                   (MonadPath)
import qualified LunaStudio.Data.Node                        as Empire
import           LunaStudio.Data.Port                        (_WithDefault)
import           LunaStudio.Data.PortDefault                 (PortDefault)
import           LunaStudio.Data.PortRef                     (AnyPortRef, InPortRef)
import           LunaStudio.Data.Position                    (Position)
import           NodeEditor.Action.Command                   (Command)
import           NodeEditor.Action.State.App                 (get, modify)
import qualified NodeEditor.Action.State.Internal.NodeEditor as Internal
import           NodeEditor.Batch.Workspace                  (nodeSearcherData)
import           NodeEditor.Data.CameraTransformation        (CameraTransformation)
import           NodeEditor.Data.Graph                       (Graph (Graph))
import           NodeEditor.React.Model.App                  (nodeEditor)
import           NodeEditor.React.Model.Connection           (Connection, ConnectionId, ConnectionsMap, HalfConnection, PosConnection,
                                                              connectionId, containsNode, containsPortRef, dstNodeLoc, srcNodeLoc,
                                                              toConnectionsMap)
import           NodeEditor.React.Model.Node                 (InputNode, Node (Expression, Input, Output), NodeLoc, OutputNode, nodeLoc,
                                                              toNodesMap)
import           NodeEditor.React.Model.Node.ExpressionNode  (ExpressionNode, isSelected)
import qualified NodeEditor.React.Model.Node.ExpressionNode  as ExpressionNode
import           NodeEditor.React.Model.NodeEditor           (NodeEditor)
import qualified NodeEditor.React.Model.NodeEditor           as NE
import           NodeEditor.React.Model.Port                 (state)
import           NodeEditor.React.Model.Searcher             (Searcher)
import           NodeEditor.State.Global                     (State, workspace)
import           Text.ScopeSearcher.Item                     (Items, isElement)


getNodeEditor :: Command State NodeEditor
getNodeEditor = get nodeEditor

modifyNodeEditor :: M.State NodeEditor r -> Command State r
modifyNodeEditor = modify nodeEditor

isGraphLoaded :: Command State Bool
isGraphLoaded = view NE.isGraphLoaded <$> getNodeEditor

whenGraphLoaded :: Command State () -> Command State ()
whenGraphLoaded = whenM isGraphLoaded

setGraphLoaded :: Bool -> Command State ()
setGraphLoaded flag = modifyNodeEditor $ NE.isGraphLoaded .= flag

resetGraph :: Command State ()
resetGraph = modifyNodeEditor $ M.put def

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

findPredecessorPosition :: ExpressionNode -> Command State Position
findPredecessorPosition n = ExpressionNode.findPredecessorPosition n <$> getExpressionNodes

findSuccessorPosition :: ExpressionNode -> Command State Position
findSuccessorPosition n = ExpressionNode.findSuccessorPosition n <$> getExpressionNodes

getNode :: NodeLoc -> Command State (Maybe Node)
getNode nl = NE.getNode nl <$> getNodeEditor

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
removeNode = Internal.removeNodeRec NE.expressionNodes ExpressionNode.expressionNodes

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

getPosConnection :: ConnectionId -> Command State (Maybe PosConnection)
getPosConnection connId = do
    mayConnection <- getConnection connId
    ne <- getNodeEditor
    return $ join $ NE.toPosConnection ne <$> mayConnection

getPosConnections :: Command State [PosConnection]
getPosConnections = view NE.posConnections <$> getNodeEditor

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


modifyHalfConnections :: M.State [HalfConnection] r -> Command State r
modifyHalfConnections = modify (nodeEditor . NE.halfConnections)


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
getNodeSearcherData = maybe def id <$> preuse (workspace . traverse . nodeSearcherData)

class NodeEditorElementId a where
    inGraph :: a -> Command State Bool
instance NodeEditorElementId NodeLoc where
    inGraph = fmap isJust . getNode
instance NodeEditorElementId ConnectionId where
    inGraph = fmap isJust . getConnection

getPort :: NE.GetPort a b => a -> Command State (Maybe b)
getPort portRef = NE.getPort portRef <$> getNodeEditor

getPortDefault :: InPortRef -> Command State (Maybe PortDefault)
getPortDefault portRef = maybe Nothing (\mayPort -> mayPort ^? state . _WithDefault) <$> (NE.getPort portRef <$> getNodeEditor)