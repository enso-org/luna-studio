module React.Model.NodeEditor where

import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Luna.Studio.Prelude

import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.PortRef  (InPortRef)
import           React.Model.Connection   (Connection)
import           React.Model.Node         (Node)
import           React.Store.Ref          (Ref)
import           React.Model.SelectionBox (SelectionBox)



data NodeEditor = NodeEditor { _nodes        :: HashMap NodeId    (Ref Node)
                             , _connections  :: HashMap InPortRef (Ref Connection)
                             , _selectionBox :: Ref SelectionBox
                             }

makeLenses ''NodeEditor

mk :: Ref SelectionBox -> NodeEditor
mk = NodeEditor HashMap.empty HashMap.empty

reset :: NodeEditor -> NodeEditor
reset = mk . _selectionBox
