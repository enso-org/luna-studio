{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.Data.BreadcrumbHierarchy where

import           Empire.Prelude             hiding (id)
import qualified Empire.Prelude             as P

import           LunaStudio.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem (..))
import           LunaStudio.Data.NodeId     (NodeId)

import           Empire.Data.AST            (NodeRef, astExceptionFromException, astExceptionToException)

import           Data.Map                   (Map)
import qualified Data.Map                   as Map


data LamItem = LamItem { _portMapping :: (NodeId, NodeId)
                       , _lamRef      :: NodeRef
                       , _children    :: Map NodeId BChild
                       } deriving (Show, Eq)

data ExprItem = ExprItem { _portChildren :: Map Int LamItem
                         , _selfRef      :: NodeRef
                         } deriving (Show, Eq)

data BChild  = ExprChild ExprItem | LambdaChild  LamItem deriving (Show, Eq)

data BreadcrumbDoesNotExistException = BreadcrumbDoesNotExistException (Breadcrumb BreadcrumbItem)
    deriving (Show)

instance Exception BreadcrumbDoesNotExistException where
    toException = astExceptionToException
    fromException = astExceptionFromException

makeLenses ''LamItem
makeLenses ''ExprItem
makePrisms ''BChild

class HasSelf a where
    self :: Lens' a NodeRef

instance HasSelf LamItem where
    self = lamRef

instance HasSelf ExprItem where
    self = selfRef

instance HasSelf BChild where
    self = lens getter setter where
        getter (ExprChild   a)   = a ^. self
        getter (LambdaChild a)   = a ^. self
        setter (ExprChild   a) s = ExprChild   $ a & self .~ s
        setter (LambdaChild a) s = LambdaChild $ a & self .~ s

class HasRefs a where
    refs :: Traversal' a NodeRef

instance HasRefs NodeRef where
    refs = P.id

instance {-# OVERLAPPABLE #-} (Traversable t, HasRefs a) => HasRefs (t a) where
    refs = traverse . refs

instance HasRefs LamItem where
    refs f (LamItem pm ref children') = LamItem pm <$> refs f ref <*> refs f children'

instance HasRefs ExprItem where
    refs f (ExprItem children' ref) = ExprItem <$> refs f children' <*> refs f ref

instance HasRefs BChild where
    refs f (ExprChild   it) = ExprChild   <$> refs f it
    refs f (LambdaChild it) = LambdaChild <$> refs f it

unexpectedBreadcrumbError :: BreadcrumbItem -> String
unexpectedBreadcrumbError bc =
    "internal error: unexpected breadcrumb " <> show bc

getBreadcrumbItems :: LamItem -> Breadcrumb BreadcrumbItem -> [BChild]
getBreadcrumbItems item (Breadcrumb bc) = go bc item where
    go [] _ = []
    go (Lambda id : crumbs) b = case b ^? children . ix id of
        Just (LambdaChild c) -> LambdaChild c : go crumbs c
        Just (ExprChild   c) -> ExprChild   c : []
        Nothing              -> []
    go (a : _) _ = error $ unexpectedBreadcrumbError a

navigateTo :: LamItem -> Breadcrumb BreadcrumbItem -> Maybe LamItem
navigateTo item (Breadcrumb bc) = go bc item where
    go [] b = pure b
    go (Lambda id : crumbs) b = do
        child <- b ^? children . ix id . _LambdaChild
        go crumbs child
    go (Arg id pos : crumbs) b = do
        child <- b ^? children . ix id . _ExprChild . portChildren . ix pos
        go crumbs child
    go (a : _) _ = error $ unexpectedBreadcrumbError a

replaceAt :: Breadcrumb BreadcrumbItem -> LamItem -> LamItem -> Maybe LamItem
replaceAt (Breadcrumb bc) parent child = go bc parent child where
    go [] par child = pure child
    go (Lambda id : crumbs) par child = do
        lowerPar <- par ^? children . ix id . _LambdaChild
        replaced <- go crumbs lowerPar child
        return $ par & children . ix id . _LambdaChild .~ replaced
    go (Arg id pos : crumbs) par child = do
        lowerPar <- par ^? children . ix id . _ExprChild . portChildren . ix pos
        replaced <- go crumbs lowerPar child
        return $ par & children . ix id . _ExprChild . portChildren . ix pos .~ replaced
    go (a : _) _ _ = error $ unexpectedBreadcrumbError a

topLevelIDs :: LamItem -> [NodeId]
topLevelIDs = Map.keys . view children

getLamItems :: LamItem -> [((NodeId, Maybe Int), LamItem)]
getLamItems hierarchy = goParent hierarchy
    where
        goParent lamItem = goLamItem Nothing lamItem

        goBChild nodeId (ExprChild exprItem)  = goExprItem nodeId exprItem
        goBChild nodeId (LambdaChild lamItem) = goLamItem (Just (nodeId, Nothing)) lamItem

        goLamItem idArg lamItem = first <> concatMap (\(a, b) -> goBChild a b) (Map.assocs $ lamItem ^. children)
            where
                first = case idArg of
                    Just a -> [(a, lamItem)]
                    _      -> []

        goExprItem nodeId (ExprItem children' _) = map (\(a,b) -> ((nodeId, Just a), b)) $ Map.assocs children'
