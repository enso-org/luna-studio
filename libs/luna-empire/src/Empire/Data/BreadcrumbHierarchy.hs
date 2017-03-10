module Empire.Data.BreadcrumbHierarchy where

import           Empire.Prelude

import           Empire.API.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem (..))
import           Empire.API.Data.Node       (NodeId)

import           Empire.Data.AST                   (NodeRef)

import           Data.Map                          (Map)
import qualified Data.Map                          as Map

data NodeIDTarget = MatchNode     NodeRef
                  | AnonymousNode NodeRef
                  deriving (Show, Eq)

getAnyRef :: NodeIDTarget -> NodeRef
getAnyRef (MatchNode ref)     = ref
getAnyRef (AnonymousNode ref) = ref

newtype BreadcrumbHierarchy = BC { _items :: Map NodeId BItem } deriving (Show, Eq)

data BItem = BItem { _children    :: Map NodeId BItem
                   , _portMapping :: Maybe (NodeId, NodeId)
                   , _self        :: Maybe (NodeId, NodeIDTarget)
                   , _body        :: Maybe NodeRef
                   } deriving (Show, Eq)

makeLenses ''BItem

instance Default BItem where
    def = BItem def def def def

navigateTo :: BItem -> Breadcrumb BreadcrumbItem -> Maybe BItem
navigateTo b (Breadcrumb crumbs) = go crumbs b where
    go [] b = pure b
    go (Lambda id : crumbs) b = do
        child <- b ^. children . at id
        go crumbs child

replaceAt :: Breadcrumb BreadcrumbItem -> BItem -> BItem -> Maybe BItem
replaceAt (Breadcrumb crumbs) par child = go crumbs par child where
    go [] par child = pure child
    go (Lambda id : crumbs) par child = do
        rep <- par ^. children . at id
        new <- go crumbs rep child
        return $ par & children . at id ?~ new

topLevelIDs :: BItem -> [NodeId]
topLevelIDs = Map.keys . view children
