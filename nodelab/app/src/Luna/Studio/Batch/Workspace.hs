module Luna.Studio.Batch.Workspace where

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Convert                  (Convertible (..))
import qualified Data.IntMap.Lazy              as IntMap
import qualified Data.List                     as List
import           Data.Map.Lazy                 (Map)
import           Data.UUID.Types               (nil)
import           Luna.Studio.Prelude

import           Empire.API.Data.Breadcrumb    (Breadcrumb (..), BreadcrumbItem)
import qualified Empire.API.Data.Breadcrumb    as Breadcrumb
import           Empire.API.Data.GraphLocation (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Library       (Library)
import qualified Empire.API.Data.Library       as Library
import           Empire.API.Data.Node          (Node, NodeId)
import           Empire.API.Data.NodeLoc       (NodeLoc (NodeLoc), NodePath (NodePath))
import           Empire.API.Data.Project       (Project, ProjectId)
import qualified Empire.API.Data.Project       as Project

import           Text.ScopeSearcher.Item       (Items)

-- data UIGraphLocation = UIGraphLocation { _libraryName :: String
--                                        , _breadcrumb  :: Breadcrumb BreadcrumbItem
--                                        } deriving (Show, Eq, Generic)


data Workspace = Workspace { _currentLocation  :: GraphLocation
                           , _lastUILocation   :: Maybe GraphLocation
                           , _isGraphLoaded    :: Bool
                           , _nodeSearcherData :: Items Node
                           } deriving (Show, Eq, Generic)

instance ToJSON Workspace

instance Default Workspace where
    def = Workspace (GraphLocation "" (Breadcrumb [])) def False def

makeLenses ''Workspace

-- currentProject' :: Workspace -> Project
-- currentProject' w = fromMaybe err $ w ^? projects . ix pid where
--     pid = w ^. currentLocation . GraphLocation.projectId
--     err = error $ "Invalid project id: " <> show pid
--
-- currentProject :: Getter Workspace Project
-- currentProject = to currentProject'

-- currentProjectId :: Functor f => (ProjectId -> f ProjectId) -> Workspace -> f Workspace
-- currentProjectId = currentLocation . GraphLocation.projectId

-- currentLibrary' :: Workspace -> Library
-- currentLibrary' w = fromMaybe err $ project ^? Project.libs . ix lid where
--     lid = w ^. currentLocation . GraphLocation.libraryId
--     project = w ^. currentProject
--     err = error "Invalid library id"
--
-- currentLibrary :: Getter Workspace Library
-- currentLibrary = to currentLibrary'


-- makeLenses ''UIGraphLocation
-- instance ToJSON UIGraphLocation
-- instance FromJSON UIGraphLocation

uiGraphLocation' :: Workspace -> GraphLocation
uiGraphLocation' w = GraphLocation library breadcrumb' where
    breadcrumb' = w ^. currentLocation . GraphLocation.breadcrumb
    -- project     = w ^. currentProjectId
    library     = w ^. currentLocation . GraphLocation.filePath

uiGraphLocation :: Getter Workspace GraphLocation
uiGraphLocation = to uiGraphLocation'

-- fromUIGraphLocation :: UIGraphLocation -> GraphLocation
-- fromUIGraphLocation (UIGraphLocation lib bc) = do
--     project <- projs ^? ix projId
--     let libs  = IntMap.toList $ project ^. Project.libs
--     (libraryId, _) <- find (\(_,p) -> p ^. Library.path == lib) libs
--     return $ GraphLocation projId libraryId bc


instance Convertible (Workspace, NodeLoc) (Breadcrumb BreadcrumbItem) where
    convert (workspace, nodeLoc) = convert (workspace ^. currentLocation, nodeLoc)

instance Convertible (Workspace, NodeLoc) (Workspace, NodeId) where
    convert (workspace, nodeLoc) = (workspace & currentLocation .~ newLocation, nodeId') where
        (newLocation, nodeId') = convert (workspace ^. currentLocation, nodeLoc)

instance Convertible (GraphLocation, Breadcrumb BreadcrumbItem, NodeId) (Maybe NodeLoc) where
    convert (graphLoc, breadcrumb', nodeId') = do
        localBc' <- Breadcrumb <$> List.stripPrefix (graphLoc ^. GraphLocation.breadcrumb . Breadcrumb.items) (breadcrumb' ^. Breadcrumb.items)
        return $ NodeLoc (NodePath localBc') nodeId'
