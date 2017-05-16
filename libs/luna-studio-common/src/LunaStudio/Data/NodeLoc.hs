{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
module LunaStudio.Data.NodeLoc where

import           Data.Binary                   (Binary)
import           Data.Convert                  (Convertible (convert))
import qualified Data.Set                      as Set
import           LunaStudio.Data.Breadcrumb    (Breadcrumb (Breadcrumb), BreadcrumbItem)
import qualified LunaStudio.Data.Breadcrumb    as Breadcrumb
import           LunaStudio.Data.GraphLocation (GraphLocation)
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import           LunaStudio.Data.Node          (NodeId)
import           Prologue


data NodePath = NodePath
        { _localBc :: Breadcrumb BreadcrumbItem
        } deriving (Default, Eq, Generic, NFData, Ord, Show)

data NodeLoc = NodeLoc
        { _path   :: NodePath
        , _nodeId :: NodeId
        } deriving (Eq, Generic, NFData, Ord, Show)

makeLenses ''NodePath
makeLenses ''NodeLoc

instance Binary NodePath
instance Binary NodeLoc

instance Convertible (NodePath, NodeId) NodeLoc where
    convert = uncurry NodeLoc

instance Convertible NodeId NodeLoc where
    convert nid = NodeLoc def nid

instance Convertible NodeLoc NodeId where --FIXME this instance is only for compatibility with old API
    convert = view nodeId

class HasBreadcrumb a where breadcrumb :: Lens' a (Breadcrumb BreadcrumbItem)
class HasNodeLoc a where nodeLoc :: Lens' a NodeLoc

instance HasBreadcrumb GraphLocation               where breadcrumb = GraphLocation.breadcrumb
instance HasBreadcrumb (Breadcrumb BreadcrumbItem) where breadcrumb = id
instance HasBreadcrumb NodePath                    where breadcrumb = localBc
instance HasBreadcrumb NodeLoc                     where breadcrumb = path . breadcrumb
instance HasNodeLoc NodeLoc                        where nodeLoc = id

instance HasBreadcrumb b => Convertible b NodePath where
    convert = NodePath . view breadcrumb

instance HasBreadcrumb b => Convertible (b, NodeLoc) (b, NodeId) where
    convert (b, n) = (b & breadcrumb %~ (<> (n ^. breadcrumb)), n ^. nodeId) where


empty :: NodePath
empty = def

pathItems :: Lens' NodeLoc [BreadcrumbItem]
pathItems = path . localBc . Breadcrumb.items

appendItem :: BreadcrumbItem -> NodePath -> NodePath
appendItem item = localBc %~ (<> Breadcrumb [item])

dropItem :: NodePath -> NodePath
dropItem = localBc . Breadcrumb.items %~ init

replaceLast :: BreadcrumbItem -> NodePath -> NodePath
replaceLast item = appendItem item . dropItem

fromPath :: NodePath -> NodeLoc
fromPath path' = NodeLoc newPath $ lastItem ^. Breadcrumb.nodeId where
    newPath = path' & localBc . Breadcrumb.items %~ init
    lastItem = path' ^. localBc . Breadcrumb.items . to last

toBreadcrumb :: NodeLoc -> Breadcrumb BreadcrumbItem
toBreadcrumb nl = (nl ^. breadcrumb) <> Breadcrumb [Breadcrumb.Lambda $ nl ^. nodeId]

prependPath :: (HasBreadcrumb b, HasNodeLoc n) => b -> n -> n
prependPath b = nodeLoc . path . localBc %~ (b ^. breadcrumb <>)

normalise :: (HasBreadcrumb b, HasNodeLoc n) => b -> n -> (b, n)
normalise b n = ( b & breadcrumb %~ (<> (n ^. nodeLoc . path . localBc))
                , n & nodeLoc . path .~ def)

normalise' :: (HasBreadcrumb b, HasNodeLoc n) => b -> [n] -> (b, [n])
normalise' b ns =
    let split n = case n ^. nodeLoc . path . localBc . Breadcrumb.items of
          (h:t) -> Just (h, n & nodeLoc . path . localBc . Breadcrumb.items .~ t)
          _     -> Nothing
        splitted = sequence $ map split ns
    in case splitted of
        Nothing        -> (b, ns)
        Just splitted' -> case Set.toList $ Set.fromList $ map fst splitted' of
            [item] -> normalise' (b & breadcrumb . Breadcrumb.items %~ (<> [item])) $ map snd splitted'
            _      -> (b, ns)
