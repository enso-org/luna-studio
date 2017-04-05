module Luna.Studio.Batch.Workspace where

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Convert                  (Convertible (..))
import qualified Data.List                     as List
import           Luna.Studio.Prelude

import           Empire.API.Data.Breadcrumb    (Breadcrumb (Breadcrumb), BreadcrumbItem)
import qualified Empire.API.Data.Breadcrumb    as Breadcrumb
import           Empire.API.Data.GraphLocation (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Node          (Node, NodeId)
import           Empire.API.Data.NodeLoc       (NodeLoc (NodeLoc), NodePath (NodePath))

import           Text.ScopeSearcher.Item       (Items)



data Workspace = Workspace { _currentLocation  :: GraphLocation
                           , _lastUILocation   :: Maybe GraphLocation
                           , _isGraphLoaded    :: Bool
                           , _nodeSearcherData :: Items Node
                           } deriving (Show, Eq, Generic)

instance ToJSON Workspace

mk :: FilePath -> Workspace
mk path = Workspace (GraphLocation path def) def False def

makeLenses ''Workspace

uiGraphLocation' :: Workspace -> GraphLocation
uiGraphLocation' w = GraphLocation library breadcrumb' where
    breadcrumb' = w ^. currentLocation . GraphLocation.breadcrumb
    library     = w ^. currentLocation . GraphLocation.filePath

uiGraphLocation :: Getter Workspace GraphLocation
uiGraphLocation = to uiGraphLocation'

instance Convertible (Workspace, NodeLoc) (Breadcrumb BreadcrumbItem) where
    convert (workspace, nodeLoc) = convert (workspace ^. currentLocation, nodeLoc)

instance Convertible (Workspace, NodeLoc) (Workspace, NodeId) where
    convert (workspace, nodeLoc) = (workspace & currentLocation .~ newLocation, nodeId') where
        (newLocation, nodeId') = convert (workspace ^. currentLocation, nodeLoc)

instance Convertible (GraphLocation, Breadcrumb BreadcrumbItem, NodeId) (Maybe NodeLoc) where
    convert (graphLoc, breadcrumb', nodeId') = do
        localBc' <- Breadcrumb <$> List.stripPrefix (graphLoc ^. GraphLocation.breadcrumb . Breadcrumb.items) (breadcrumb' ^. Breadcrumb.items)
        return $ NodeLoc (NodePath localBc') nodeId'
