module React.Stores where

import           Data.HashMap.Strict         (HashMap)
import           Data.IntMap                 (IntMap)
import           Utils.PreludePlus

import           Empire.API.Data.Node        (NodeId)
import qualified React.Store.Function.Input  as Input
import qualified React.Store.Function.Output as Output
import qualified React.Store.Node            as Node
import qualified React.Store.NodeEditor      as NodeEditor



data Stores = Stores { _nodeEditor :: NodeEditor.Ref
                     , _nodes      :: HashMap NodeId Node.Ref
                     , _inputs     :: IntMap Input.Ref
                     , _outputs    :: IntMap Output.Ref
                     }

makeLenses ''Stores


create :: IO Stores
create = Stores
    <$> NodeEditor.create
    <*> pure def
    <*> pure def
    <*> pure def


--createNode = do
--   constructNewNode
--   pingNodeEditor
