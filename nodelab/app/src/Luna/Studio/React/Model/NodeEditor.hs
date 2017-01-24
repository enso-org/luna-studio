module Luna.Studio.React.Model.NodeEditor where

import           Data.HashMap.Strict                   (HashMap)
import qualified Data.HashMap.Strict                   as HashMap
import           Empire.API.Data.Node                  (NodeId)
import           Empire.API.Data.PortRef               (InPortRef)
import           Luna.Studio.Data.CameraTransformation (CameraTransformation)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection    (Connection, CurrentConnection)
import           Luna.Studio.React.Model.Node          (Node)
import           Luna.Studio.React.Model.SelectionBox  (SelectionBox)



data NodeEditor = NodeEditor { _screenTransform   :: CameraTransformation
                             , _nodes             :: HashMap NodeId    Node
                             , _connections       :: HashMap InPortRef Connection
                             , _currentConnection :: Maybe CurrentConnection
                             , _selectionBox      :: Maybe SelectionBox
                             } deriving (Eq)

makeLenses ''NodeEditor

instance Default NodeEditor where
    def = NodeEditor def HashMap.empty HashMap.empty Nothing def
