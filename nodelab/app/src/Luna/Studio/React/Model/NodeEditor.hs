module Luna.Studio.React.Model.NodeEditor where

import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude

import           Empire.API.Data.Node                 (NodeId)
import           Empire.API.Data.PortRef              (InPortRef)
import           Luna.Studio.React.Model.Connection   (Connection, CurrentConnection)
import           Luna.Studio.React.Model.Node         (Node)
import           Luna.Studio.React.Model.SelectionBox (SelectionBox)
import           Luna.Studio.React.Store.Ref          (Ref)



data NodeEditor = NodeEditor { _zoom              :: Double
                             , _pan               :: Vector2 Double
                             , _nodes             :: HashMap NodeId    (Ref Node)
                             , _connections       :: HashMap InPortRef (Ref Connection)
                             , _currentConnection :: Maybe (Ref CurrentConnection)
                             , _selectionBox      :: Ref SelectionBox

                             }

makeLenses ''NodeEditor

mk :: Ref SelectionBox -> NodeEditor
mk = NodeEditor 1 def HashMap.empty HashMap.empty Nothing

reset :: NodeEditor -> NodeEditor
reset = mk . _selectionBox
