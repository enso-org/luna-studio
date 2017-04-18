{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
module Luna.Studio.React.Model.NodeEditor where

import qualified Data.HashMap.Strict                         as HashMap
import           Data.Position                               (Position)
import qualified Empire.API.Data.Breadcrumb                  as B
import           Empire.API.Data.MonadPath                   (MonadPath)
import qualified Empire.API.Data.NodeLoc                     as NodeLoc
import           Luna.Studio.Data.CameraTransformation       (CameraTransformation)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection          (ConnectionsMap, CurrentConnection)
import           Luna.Studio.React.Model.ConnectionPen       (ConnectionPen)
import           Luna.Studio.React.Model.Layout              (Layout)
import qualified Luna.Studio.React.Model.Layout              as Layout
import           Luna.Studio.React.Model.Node                (ExpressionNode, ExpressionNodesMap, HasNodeLoc, InputNode, NodeLoc,
                                                              OutputNode, nodeId)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as ExpressionNode
import           Luna.Studio.React.Model.Searcher            (Searcher)
import           Luna.Studio.React.Model.SelectionBox        (SelectionBox)


data NodeEditor = NodeEditor { _expressionNodes     :: ExpressionNodesMap
                             , _inputNode           :: Maybe InputNode
                             , _outputNode          :: Maybe OutputNode
                             , _monads              :: [MonadPath]
                             , _connections         :: ConnectionsMap
                             , _visualizations      :: [(NodeLoc, Int, Position)] --TODO move to node

                             , _currentConnections  :: [CurrentConnection]
                             , _connectionPen       :: Maybe ConnectionPen
                             , _selectionBox        :: Maybe SelectionBox
                             , _searcher            :: Maybe Searcher

                             , _layout              :: Layout
                             } deriving (Default, Eq, Generic)

makeLenses ''NodeEditor

screenTransform :: Lens' NodeEditor CameraTransformation
screenTransform = layout . Layout.screenTransform

expressionNodesRecursive :: Getter NodeEditor [ExpressionNode]
expressionNodesRecursive = to (concatMap expressionNodesRecursive' . HashMap.elems . view expressionNodes) where
    expressionNodesRecursive' node = node : concatMap expressionNodesRecursive' (concatMap (HashMap.elems . view ExpressionNode.expressionNodes) (node ^. ExpressionNode.subgraphs))

inputNodesRecursive :: Getter NodeEditor [InputNode]
inputNodesRecursive = to inputNodesRecursive' where
    inputNodesRecursive' ne = maybeToList (ne ^. inputNode) <> concatMap inputNodesRec (HashMap.elems $ ne ^. expressionNodes)
    inputNodesRec node = (concatMap (maybeToList . view ExpressionNode.inputNode) (node ^. ExpressionNode.subgraphs))
                       <> concatMap inputNodesRec (concatMap (HashMap.elems . view ExpressionNode.expressionNodes) (node ^. ExpressionNode.subgraphs))

outputNodesRecursive :: Getter NodeEditor [OutputNode]
outputNodesRecursive = to outputNodesRecursive' where
    outputNodesRecursive' ne = maybeToList (ne ^. outputNode) <> concatMap outputNodesRec (HashMap.elems $ ne ^. expressionNodes)
    outputNodesRec node = (concatMap (maybeToList . view ExpressionNode.outputNode) (node ^. ExpressionNode.subgraphs))
                        <> concatMap outputNodesRec (concatMap (HashMap.elems . view ExpressionNode.expressionNodes) (node ^. ExpressionNode.subgraphs))

getExpressionNode :: NodeLoc -> NodeEditor -> Maybe ExpressionNode
getExpressionNode nl ne = getNodeRec' (nl ^. NodeLoc.pathItems) where
    getNodeRec' []    = ne ^. expressionNodes . at (nl ^. NodeLoc.nodeId)
    getNodeRec' (h:t) = getSubNode t =<< getSubgraph h =<< getRootNode (h ^. B.nodeId)

    getSubNode []    = preview (ExpressionNode.expressionNodes . at (nl ^. NodeLoc.nodeId) . traverse)
    getSubNode (h:t) = getSubNode t <=< getSubgraph h <=< getExprNode (h ^. B.nodeId)

    getSubgraph item = preview (ExpressionNode.subgraphs . at item . traverse)
    getExprNode nid  = preview (ExpressionNode.expressionNodes . at nid . traverse)

    getRootNode nid = ne ^. expressionNodes . at nid

getInputNode :: NodeLoc -> NodeEditor -> Maybe InputNode
getInputNode = _getNodeRec inputNode ExpressionNode.inputNode

getOutputNode :: NodeLoc -> NodeEditor -> Maybe OutputNode
getOutputNode = _getNodeRec outputNode ExpressionNode.outputNode

_getNodeRec :: HasNodeLoc a => Lens' NodeEditor (Maybe a) -> Lens' ExpressionNode.Subgraph (Maybe a) -> NodeLoc -> NodeEditor -> Maybe a
_getNodeRec rootLens subLens nl ne = getNodeRec' (nl ^. NodeLoc.pathItems) where
    getNodeRec' []    = ne ^? rootLens . filtered ((== Just (nl ^. nodeId)) . fmap (view nodeId)) . traverse
    getNodeRec' (h:t) = getSubNode t =<< getSubgraph h =<< getRootNode (h ^. B.nodeId)

    getSubNode []    = preview (subLens . filtered ((== Just (nl ^. nodeId)) . fmap (view nodeId)) . traverse)
    getSubNode (h:t) = getSubNode t <=< getSubgraph h <=< getExprNode (h ^. B.nodeId)

    getSubgraph item = preview (ExpressionNode.subgraphs . at item . traverse)
    getExprNode nid  = preview (ExpressionNode.expressionNodes . at nid . traverse)

    getRootNode nid = ne ^. expressionNodes . at nid
