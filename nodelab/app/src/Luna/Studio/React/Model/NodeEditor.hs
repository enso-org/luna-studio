{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.React.Model.NodeEditor where

import           Data.HashMap.Strict                   (HashMap)
import           Data.Position                         (Position)
import           Empire.API.Data.Node                  (NodeId)
import           Empire.API.Data.PortRef               (InPortRef)
import           Empire.API.Data.TypeRep               (TypeRep)
import           Luna.Studio.Data.CameraTransformation (CameraTransformation)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection    (Connection, CurrentConnection)
import           Luna.Studio.React.Model.ConnectionPen (ConnectionPen)
import           Luna.Studio.React.Model.Node          (Node)
import           Luna.Studio.React.Model.SelectionBox  (SelectionBox)


data NodeEditor = NodeEditor { _screenTransform   :: CameraTransformation
                             , _nodes             :: HashMap NodeId    Node
                             , _monads            :: [(TypeRep, [NodeId])]
                             , _connections       :: HashMap InPortRef Connection
                             , _currentConnection :: Maybe CurrentConnection
                             , _connectionPen     :: Maybe ConnectionPen
                             , _selectionBox      :: Maybe SelectionBox
                             , _visualizations    :: [(NodeId, Int, Position)]
                             } deriving (Default, Eq, Generic)

makeLenses ''NodeEditor
