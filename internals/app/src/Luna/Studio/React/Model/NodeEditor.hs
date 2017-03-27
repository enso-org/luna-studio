{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.React.Model.NodeEditor where

import qualified Data.HashMap.Strict                   as HashMap
import           Data.Position                         (Position)
import           Empire.API.Data.MonadPath             (MonadPath)
import           Luna.Studio.Data.CameraTransformation (CameraTransformation)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection    (ConnectionsMap, CurrentConnection)
import           Luna.Studio.React.Model.ConnectionPen (ConnectionPen)
import           Luna.Studio.React.Model.EdgeNode      (EdgeNodesMap)
import           Luna.Studio.React.Model.Node          (Node, NodeId, NodesMap)
import qualified Luna.Studio.React.Model.Node          as Node
import           Luna.Studio.React.Model.Port          (DraggedPort)
import           Luna.Studio.React.Model.Searcher      (Searcher)
import           Luna.Studio.React.Model.SelectionBox  (SelectionBox)


data NodeEditor = NodeEditor { _screenTransform     :: CameraTransformation
                             , _nodes               :: NodesMap
                             , _edgeNodes           :: EdgeNodesMap
                             , _monads              :: [MonadPath]
                             , _connections         :: ConnectionsMap
                             , _visualizations      :: [(NodeId, Int, Position)] --TODO move to node

                             , _currentConnections  :: [CurrentConnection]
                             , _portDragConnections :: ConnectionsMap
                             , _connectionPen       :: Maybe ConnectionPen
                             , _selectionBox        :: Maybe SelectionBox
                             , _searcher            :: Maybe Searcher
                             , _draggedPort         :: Maybe DraggedPort
                             } deriving (Default, Eq, Generic)

makeLenses ''NodeEditor

nodesRecursive :: Getter NodeEditor [Node]
nodesRecursive = to (concatMap nodesRecursive' . HashMap.elems . view nodes) where
    nodesRecursive' node = node : concatMap nodesRecursive' (concatMap (HashMap.elems . view Node.nodes) (node ^. Node.subgraphs))