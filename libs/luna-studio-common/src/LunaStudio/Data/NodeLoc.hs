module LunaStudio.Data.NodeLoc
    ( module LunaStudio.Data.NodeLoc
    , module X
    ) where

import LunaStudio.Data.NodeId as X (NodeId)

import Prologue

import qualified Data.Set                      as Set
import qualified LunaStudio.Data.Breadcrumb    as Breadcrumb
import qualified LunaStudio.Data.GraphLocation as GraphLocation

import Control.Lens                  (to)
import Data.Aeson.Types              (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Binary                   (Binary)
import Data.Convert                  (Convertible (convert))
import LunaStudio.Data.Breadcrumb    (Breadcrumb (Breadcrumb), BreadcrumbItem)
import LunaStudio.Data.GraphLocation (GraphLocation)


data NodePath = NodePath
    { _localBc :: Breadcrumb BreadcrumbItem
    } deriving (Eq, Generic, Ord, Show)

data NodeLoc = NodeLoc
    { _path   :: NodePath
    , _nodeId :: NodeId
    } deriving (Eq, Generic, Ord, Show)

makeLenses ''NodePath
makeLenses ''NodeLoc

instance Binary      NodePath
instance Default     NodePath
instance NFData      NodePath
instance FromJSON    NodePath
instance ToJSON      NodePath
instance Binary      NodeLoc
instance NFData      NodeLoc
instance FromJSON    NodeLoc
instance FromJSONKey NodeLoc
instance ToJSON      NodeLoc
instance ToJSONKey   NodeLoc

instance Convertible (NodePath, NodeId) NodeLoc where
    convert = uncurry NodeLoc

instance Convertible NodeId NodeLoc where
    convert = NodeLoc def

instance Convertible NodeLoc NodeId where --FIXME this instance is only for compatibility with old API
    convert = view nodeId

class HasBreadcrumb a where breadcrumb :: Lens' a (Breadcrumb BreadcrumbItem)
class HasNodeLoc a where nodeLoc :: Lens' a NodeLoc

instance HasBreadcrumb GraphLocation               where
    breadcrumb = GraphLocation.breadcrumb
instance HasBreadcrumb (Breadcrumb BreadcrumbItem) where
    breadcrumb = id
instance HasBreadcrumb NodePath                    where
    breadcrumb = localBc
instance HasBreadcrumb NodeLoc                     where
    breadcrumb = path . breadcrumb
instance HasNodeLoc NodeLoc                        where
    nodeLoc = id

instance HasBreadcrumb b => Convertible b NodePath where
    convert = NodePath . view breadcrumb

empty :: NodePath
empty = def

pathItems :: Lens' NodeLoc [BreadcrumbItem]
pathItems = path . localBc . Breadcrumb.items

appendItem :: BreadcrumbItem -> NodePath -> NodePath
appendItem item = localBc %~ (<> Breadcrumb [item])

dropItem :: NodePath -> NodePath
dropItem = localBc . Breadcrumb.items %~ unsafeInit

replaceLast :: BreadcrumbItem -> NodePath -> NodePath
replaceLast item = appendItem item . dropItem

fromPath :: NodePath -> NodeLoc
fromPath path' = NodeLoc newPath $ lastItem ^. Breadcrumb.nodeId where
    newPath = path' & localBc . Breadcrumb.items %~ unsafeInit
    lastItem = path' ^. localBc . Breadcrumb.items . to unsafeLast

toNodeIdList :: NodeLoc -> [NodeId]
toNodeIdList nl = (view Breadcrumb.nodeId <$> nl ^. pathItems) <> [nl ^. nodeId]

toBreadcrumb :: NodeLoc -> Breadcrumb BreadcrumbItem
toBreadcrumb nl
    = (nl ^. breadcrumb) <> Breadcrumb [Breadcrumb.Lambda $ nl ^. nodeId]

prependPath :: (HasBreadcrumb b, HasNodeLoc n) => b -> n -> n
prependPath b = nodeLoc . path . localBc %~ (b ^. breadcrumb <>)

normalise :: (HasBreadcrumb b, HasNodeLoc n) => b -> n -> (b, n)
normalise b n = ( b & breadcrumb %~ (<> (n ^. nodeLoc . path . localBc))
                , n & nodeLoc . path .~ def)

normalise_ :: HasBreadcrumb b => b -> NodeLoc -> (b, NodeId)
normalise_ b n = (b & breadcrumb %~ (<> (n ^. breadcrumb)), n ^. nodeId)

normalise' :: (HasBreadcrumb b, HasNodeLoc n) => b -> [n] -> (b, [n])
normalise' b ns =
    let split n = case n ^. nodeLoc . path . localBc . Breadcrumb.items of
            (h:t) -> Just
                (h, n & nodeLoc . path . localBc . Breadcrumb.items .~ t)
            _     -> Nothing
        splitted = sequence $ fmap split ns
    in case splitted of
        Nothing        -> (b, ns)
        Just splitted' -> case Set.toList $ Set.fromList $ fmap fst splitted' of
            [item] -> normalise' (b & breadcrumb . Breadcrumb.items
                %~ (<> [item])) $ fmap snd splitted'
            _      -> (b, ns)
