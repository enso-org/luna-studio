{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.React.Model.NodeEditor where

import           Data.HashMap.Strict                   (HashMap)
import           Data.Position                         (Position)
import           Empire.API.Data.MonadPath             (MonadPath)
import           Empire.API.Data.Node                  (NodeId)
import           Empire.API.Data.PortRef               (InPortRef)
import           Luna.Studio.Data.CameraTransformation (CameraTransformation)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection    (Connection, CurrentConnection)
import           Luna.Studio.React.Model.ConnectionPen (ConnectionPen)
import           Luna.Studio.React.Model.Node          (Node)
import           Luna.Studio.React.Model.Port          (DraggedPort)
import           Luna.Studio.React.Model.Searcher      (Searcher)
import           Luna.Studio.React.Model.SelectionBox  (SelectionBox)

type ConnectionsMap = HashMap InPortRef Connection
type NodesMap       = HashMap NodeId Node

data NodeEditor = NodeEditor { _screenTransform     :: CameraTransformation
                             , _nodes               :: NodesMap
                             , _monads              :: [MonadPath]
                             , _connections         :: HashMap InPortRef Connection
                             , _visualizations      :: [(NodeId, Int, Position)] --TODO move to node

                             , _currentConnections  :: [CurrentConnection]
                             , _portDragConnections :: ConnectionsMap
                             , _connectionPen       :: Maybe ConnectionPen
                             , _selectionBox        :: Maybe SelectionBox
                             , _searcher            :: Maybe Searcher
                             , _draggedPort         :: Maybe DraggedPort
                             } deriving (Default, Eq, Generic)

makeLenses ''NodeEditor
