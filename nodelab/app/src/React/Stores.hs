module React.Stores where

import           Data.HashMap.Strict         (HashMap)
import           Data.IntMap                 (IntMap)
import           React.Flux
import           Utils.PreludePlus

import           Empire.API.Data.Node        (NodeId)
import qualified React.Store.Function.Input  as Input
import qualified React.Store.Function.Output as Output
import qualified React.Store.Node            as Node
import qualified React.Store.NodeEditor      as NodeEditor
import Reactive.Commands.Command (Command, performIO)


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


modify :: Typeable s => (s -> IO (s, r)) -> ReactStore s -> Command a r
modify = liftIO .: flip modifyStore

modifyIf :: Typeable s
         => (s -> Bool)
            -> (s -> IO (s, r))
            -> (s -> IO r)
         -> ReactStore s
         -> Command a r
modifyIf cond actionTrue actionFalse store = liftIO $ modifyStoreIf store cond actionTrue actionFalse

--createNode = do
--   constructNewNode
--   pingNodeEditor
