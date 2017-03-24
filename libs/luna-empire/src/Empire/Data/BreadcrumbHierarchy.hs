{-# LANGUAGE GADTs #-}

module Empire.Data.BreadcrumbHierarchy where

import           Prologue                   hiding (children)

import           Empire.API.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem (..))
import           Empire.API.Data.Node       (NodeId)

import           Empire.Data.AST            (NodeRef)

import           Data.Map                   (Map)
import qualified Data.Map                   as Map

data NodeIDTarget = MatchNode     NodeRef
                  | AnonymousNode NodeRef
                  deriving (Show, Eq)

makePrisms ''NodeIDTarget

getAnyRef :: NodeIDTarget -> NodeRef
getAnyRef (MatchNode ref)     = ref
getAnyRef (AnonymousNode ref) = ref

anyRef :: Lens' NodeIDTarget NodeRef
anyRef = lens getAnyRef set where
    set (MatchNode     _) r = MatchNode     r
    set (AnonymousNode _) r = AnonymousNode r

data LamItem = LamItem { _portMapping :: (NodeId, NodeId)
                       , _lamRef      :: NodeIDTarget
                       , _lamChildren :: Map NodeId BChild
                       , _lamBody     :: NodeRef
                       } deriving (Show, Eq)

data ExprItem = ExprItem { _portChildren :: Map Int LamItem
                         , _selfRef      :: NodeIDTarget
                         } deriving (Show, Eq)

data TopItem = TopItem { _childNodes :: Map NodeId BChild
                       , _topBody    :: Maybe NodeRef
                       } deriving (Show, Eq)

data BChild  = ExprChild       ExprItem | LambdaChild  LamItem deriving (Show, Eq)
data BParent = ToplevelParent  TopItem  | LambdaParent LamItem deriving (Show, Eq)

makeLenses ''LamItem
makeLenses ''ExprItem
makeLenses ''TopItem
makePrisms ''BChild
makePrisms ''BParent

instance Default TopItem where
    def = TopItem def def

instance Default BParent where
    def = ToplevelParent def

class HasSelf a where
    self :: Lens' a NodeIDTarget

instance HasSelf LamItem where
    self = lamRef

instance HasSelf ExprItem where
    self = selfRef

instance HasSelf BChild where
    self = lens get set where
        get (ExprChild   a)   = a ^. self
        get (LambdaChild a)   = a ^. self
        set (ExprChild   a) s = ExprChild   $ a & self .~ s
        set (LambdaChild a) s = LambdaChild $ a & self .~ s

class HasChildren a where
    children :: Lens' a (Map NodeId BChild)

instance HasChildren LamItem where
    children = lamChildren

instance HasChildren TopItem where
    children = childNodes

instance HasChildren BParent where
    children = lens get set where
        get (ToplevelParent i) = i ^. children
        get (LambdaParent   i) = i ^. children
        set (ToplevelParent i) c = ToplevelParent $ i & children .~ c
        set (LambdaParent   i) c = LambdaParent   $ i & children .~ c

class HasBody a where
    body :: Traversal' a NodeRef

instance HasBody LamItem where
    body = lamBody

instance HasBody TopItem where
    body = topBody . _Just

instance HasBody BParent where
    body trans s = case s of
        LambdaParent   i -> LambdaParent   <$> body trans i
        ToplevelParent i -> ToplevelParent <$> body trans i

getBreadcrumbItems :: BParent -> Breadcrumb BreadcrumbItem -> [BChild]
getBreadcrumbItems b (Breadcrumb crumbs) = go crumbs b where
    go [] _ = []
    go (Lambda id : crumbs) b = case b ^? children . ix id of
        Just (LambdaChild c) -> LambdaChild c : go crumbs (LambdaParent c)
        Just (ExprChild   c) -> ExprChild   c : []
        Nothing              -> []

navigateTo :: BParent -> Breadcrumb BreadcrumbItem -> Maybe BParent
navigateTo b (Breadcrumb crumbs) = go crumbs b where
    go [] b = pure b
    go (Lambda id : crumbs) b = do
        child <- b ^? children . ix id . _LambdaChild . re _LambdaParent
        go crumbs child

replaceAt :: Breadcrumb BreadcrumbItem -> BParent -> BParent -> Maybe BParent
replaceAt (Breadcrumb crumbs) par child = go crumbs par child where
    go [] par child = pure child
    go (Lambda id : crumbs) par child = do
        lowerPar <- par ^? children . ix id . _LambdaChild . re _LambdaParent
        LambdaParent replaced <- go crumbs lowerPar child
        return $ par & children . ix id . _LambdaChild .~ replaced

topLevelIDs :: BParent -> [NodeId]
topLevelIDs = Map.keys . view children
