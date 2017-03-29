{-# LANGUAGE DeriveAnyClass #-}
module Empire.API.Data.NodeLoc where

import           Data.Convert                  (Convertible (..))
import qualified Data.List                     as List
import           Empire.API.Data.Breadcrumb    (Breadcrumb (Breadcrumb), BreadcrumbItem)
import qualified Empire.API.Data.Breadcrumb    as Breadcrumb
import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Node          (NodeId)
import           Prologue


data NodePath = NodePath
        { _localBc :: Breadcrumb BreadcrumbItem
        } deriving (Eq, Default, Generic, NFData, Ord, Show)

data NodeLoc = NodeLoc
        { _path   :: NodePath
        , _nodeId :: NodeId
        } deriving (Eq, Generic, NFData, Ord, Show)

makeLenses ''NodePath
makeLenses ''NodeLoc

instance Convertible (NodePath, NodeId) NodeLoc where
    convert = uncurry NodeLoc

instance Convertible NodeId NodeLoc where
    convert nid = NodeLoc def nid

instance Convertible NodeLoc NodeId where --FIXME this instance is only for compatibility with old API
    convert = view nodeId

instance Convertible GraphLocation NodePath where
    convert = convert . view GraphLocation.breadcrumb

instance Convertible (Breadcrumb BreadcrumbItem) NodePath where
    convert = NodePath

instance Convertible (GraphLocation, NodeLoc) (Breadcrumb BreadcrumbItem, NodeId) where
    convert (graphLoc, nodeLoc) = (breadcrumb, nodeLoc ^. nodeId) where
        breadcrumb = (graphLoc ^. GraphLocation.breadcrumb) <> (nodeLoc ^. path . localBc)

instance Convertible (GraphLocation, NodeLoc) (Breadcrumb BreadcrumbItem) where
    convert (graphLoc, nodeLoc) = (graphLoc ^. GraphLocation.breadcrumb)
                               <> (nodeLoc ^. path . localBc)
                               <> Breadcrumb [Breadcrumb.Lambda $ nodeLoc ^. nodeId]

instance Convertible (GraphLocation, NodeLoc) (GraphLocation, NodeId) where
    convert (graphLoc, nodeLoc) = (newGraphLoc, nodeLoc ^. nodeId) where
        newGraphLoc = graphLoc & GraphLocation.breadcrumb %~ (<> (nodeLoc ^. path . localBc))

instance Convertible (GraphLocation, NodeId) NodeLoc where
    convert (graphLoc, nodeId) = NodeLoc (convert graphLoc) nodeId

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
fromPath path = NodeLoc path' $ lastItem ^. Breadcrumb.nodeId where
    path' = path & localBc . Breadcrumb.items %~ init
    lastItem = path ^. localBc . Breadcrumb.items . to last
