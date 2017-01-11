module Luna.Studio.React.Model.NodeEditor where

import           Data.HashMap.Strict                   (HashMap)
import qualified Data.HashMap.Strict                   as HashMap
import           Empire.API.Data.Node                  (NodeId)
import           Empire.API.Data.PortRef               (InPortRef)
import           Luna.Studio.Data.CameraTransformation (CameraTransformation)
import           Luna.Studio.Data.ConnectionPen        (ConnectionPen)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection    (Connection, CurrentConnection)
import           Luna.Studio.React.Model.Node          (Node)
import           Luna.Studio.React.Model.SelectionBox  (SelectionBox)
import           Luna.Studio.React.Store.Ref           (Ref)


data NodeEditor = NodeEditor { _screenTransform   :: CameraTransformation
                             , _nodes             :: HashMap NodeId    (Ref Node)
                             , _connections       :: HashMap InPortRef (Ref Connection)
                             , _currentConnection :: Maybe (Ref CurrentConnection)
                             , _connectionPen     :: Maybe (Ref ConnectionPen)
                             , _selectionBox      :: Ref SelectionBox
                             } deriving (Eq)

makeLenses ''NodeEditor

mk :: Ref SelectionBox -> NodeEditor
mk = NodeEditor def HashMap.empty HashMap.empty Nothing Nothing

reset :: NodeEditor -> NodeEditor
reset = mk . _selectionBox
