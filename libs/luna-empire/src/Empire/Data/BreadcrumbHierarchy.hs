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

{-replaceAt :: Breadcrumb BreadcrumbItem -> BreadcrumbHierarchy -> BreadcrumbHierarchy -> Maybe BreadcrumbHierarchy-}
{-replaceAt (Breadcrumb bs) (BC new) (BC hierarchy) = BC <$> go bs new hierarchy-}
    {-where-}
      {-go :: [BreadcrumbItem] -> Map NodeId BItem -> Map NodeId BItem -> Maybe (Map NodeId BItem)-}
      {-go [] new _ = pure new-}
      {-go (Lambda b : bs) new current = do-}
          {-item <- Map.lookup b current-}
          {-n    <- go bs new $ item ^. children . items-}
          {-return $ current & children . items . at b ?~ n-}

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

{-navigateTo :: BreadcrumbHierarchy -> Breadcrumb BreadcrumbItem -> Maybe BreadcrumbHierarchy-}
{-navigateTo (BC forest) (Breadcrumb breadcrumbs) = BC <$> go forest nodeIds-}
    {-where-}
      {-nodeIds = map (\(Lambda id) -> id) breadcrumbs-}
      {-go :: Forest NodeId -> [NodeId] -> Maybe (Forest NodeId)-}
      {-go forest [] = Just forest-}
      {-go forest (b:bs) | Just f <- at b forest = go f bs-}
                       {-| otherwise = Nothing-}

{-at :: NodeId -> Forest NodeId -> Maybe (Forest NodeId)-}
{-at nid forest = snd <$> find ((==) nid . fst) topLevel-}
    {-where-}
      {-topLevel = map (\a -> (rootLabel a, subForest a)) forest-}

{-addID :: NodeId -> BreadcrumbHierarchy -> BreadcrumbHierarchy-}
{-addID nodeid (BC hierarchy) = BC $ Node nodeid [] : hierarchy-}

{-addWithLeafs :: NodeId -> [NodeId] -> BreadcrumbHierarchy -> BreadcrumbHierarchy-}
{-addWithLeafs nodeid leafs (BC hierarchy) = BC $ Node nodeid children : hierarchy-}
    {-where-}
        {-children = map (\nid -> Node nid []) leafs-}

{-removeID :: NodeId -> BreadcrumbHierarchy -> BreadcrumbHierarchy-}
{-removeID nodeid (BC hierarchy) = BC $ filter (\(Node nid _) -> nid /= nodeid) hierarchy-}

{-topLevelIDs :: BreadcrumbHierarchy -> [NodeId]-}
{-topLevelIDs (BC f) = map (\(Node nid _) -> nid) f-}
