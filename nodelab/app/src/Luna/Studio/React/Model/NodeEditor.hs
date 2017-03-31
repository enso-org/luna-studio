{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
module Luna.Studio.React.Model.NodeEditor where

import           Data.HashMap.Strict                         (HashMap)
import qualified Data.HashMap.Strict                         as HashMap
import           Data.Position                               (Position)
import qualified Empire.API.Data.Breadcrumb                  as B
import           Empire.API.Data.MonadPath                   (MonadPath)
import qualified Empire.API.Data.NodeLoc                     as NodeLoc
import           Luna.Studio.Data.CameraTransformation       (CameraTransformation)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection          (ConnectionsMap, CurrentConnection)
import           Luna.Studio.React.Model.ConnectionPen       (ConnectionPen)
import           Luna.Studio.React.Model.Node                (ExpressionNode, ExpressionNodesMap, NodeId, NodeLoc, SidebarNode,
                                                              SidebarNodesMap)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as ExpressionNode
import           Luna.Studio.React.Model.Searcher            (Searcher)
import           Luna.Studio.React.Model.SelectionBox        (SelectionBox)

data NodeEditor = NodeEditor { _screenTransform     :: CameraTransformation
                             , _expressionNodes     :: ExpressionNodesMap
                             , _sidebarNodes        :: SidebarNodesMap
                             , _monads              :: [MonadPath]
                             , _connections         :: ConnectionsMap
                             , _visualizations      :: [(NodeLoc, Int, Position)] --TODO move to node

                             , _currentConnections  :: [CurrentConnection]
                             , _connectionPen       :: Maybe ConnectionPen
                             , _selectionBox        :: Maybe SelectionBox
                             , _searcher            :: Maybe Searcher
                             } deriving (Default, Eq, Generic)

makeLenses ''NodeEditor

expressionNodesRecursive :: Getter NodeEditor [ExpressionNode]
expressionNodesRecursive = to (concatMap expressionNodesRecursive' . HashMap.elems . view expressionNodes) where
    expressionNodesRecursive' node = node : concatMap expressionNodesRecursive' (concatMap (HashMap.elems . view ExpressionNode.expressionNodes) (node ^. ExpressionNode.subgraphs))


sidebarNodesRecursive :: Getter NodeEditor [SidebarNode]
sidebarNodesRecursive = to sidebarNodesRecursive' where
    sidebarNodesRecursive' ne = HashMap.elems (ne ^. sidebarNodes) <> concatMap sidebarNodesRec (HashMap.elems $ ne ^. expressionNodes)
    sidebarNodesRec node = (concatMap (HashMap.elems . view ExpressionNode.sidebarNodes) (node ^. ExpressionNode.subgraphs))
                     <> concatMap sidebarNodesRec (concatMap (HashMap.elems . view ExpressionNode.expressionNodes) (node ^. ExpressionNode.subgraphs))


getExpressionNode :: NodeLoc -> NodeEditor -> Maybe ExpressionNode
getExpressionNode = _getNodeRec expressionNodes ExpressionNode.expressionNodes

getSidebarNode :: NodeLoc -> NodeEditor -> Maybe SidebarNode
getSidebarNode = _getNodeRec sidebarNodes ExpressionNode.sidebarNodes

_getNodeRec :: Lens' NodeEditor (HashMap NodeId a) -> Lens' ExpressionNode.Subgraph (HashMap NodeId a) -> NodeLoc -> NodeEditor -> Maybe a
_getNodeRec rootLens subLens nl ne = getNodeRec' (nl ^. NodeLoc.pathItems) where
    getNodeRec' []    = ne ^. rootLens . at (nl ^. NodeLoc.nodeId)
    getNodeRec' (h:t) = getSubNode t =<< getSubgraph h =<< getRootNode (h ^. B.nodeId)

    getSubNode []    = preview (subLens . at (nl ^. NodeLoc.nodeId) . traverse)
    getSubNode (h:t) = getSubNode t <=< getSubgraph h <=< getExprNode (h ^. B.nodeId)

    getSubgraph item = preview (ExpressionNode.subgraphs . at item . traverse)
    getExprNode nid  = preview (ExpressionNode.expressionNodes . at nid . traverse)

    getRootNode nid = ne ^. expressionNodes . at nid
