module Luna.Studio.Batch.Workspace where

import           Data.Aeson                    (FromJSON, ToJSON)
import qualified Data.IntMap.Lazy              as IntMap
import           Data.Map.Lazy                 (Map)
import           Data.UUID.Types               (nil)
import           Luna.Studio.Prelude

import           Empire.API.Data.Breadcrumb    (Breadcrumb (Breadcrumb), BreadcrumbItem)
import qualified Empire.API.Data.Breadcrumb    as Breadcrumb
import           Empire.API.Data.GraphLocation (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Library       (Library)
import qualified Empire.API.Data.Library       as Library
import           Empire.API.Data.Node          (ExpressionNode)
import           Empire.API.Data.NodeLoc       (HasBreadcrumb (..))
import           Empire.API.Data.Project       (Project, ProjectId)
import qualified Empire.API.Data.Project       as Project
import           Text.ScopeSearcher.Item       (Items)


data UIGraphLocation = UIGraphLocation { _projectId   :: ProjectId
                                       , _libraryName :: String
                                       , _breadcrumb  :: Breadcrumb BreadcrumbItem
                                       } deriving (Show, Eq, Generic)


data Workspace = Workspace { _projects         :: Map ProjectId Project
                           , _currentLocation  :: GraphLocation
                           , _lastUILocation   :: Maybe UIGraphLocation
                           , _isGraphLoaded    :: Bool
                           , _nodeSearcherData :: Items ExpressionNode
                           } deriving (Show, Eq, Generic)

instance ToJSON Workspace

instance Default Workspace where
    def = Workspace def (GraphLocation nil 0 (Breadcrumb [])) def False def

makeLenses ''Workspace

currentProject' :: Workspace -> Project
currentProject' w = fromMaybe err $ w ^? projects . ix pid where
    pid = w ^. currentLocation . GraphLocation.projectId
    err = $(placeholder "Invalid project id")

currentProject :: Getter Workspace Project
currentProject = to currentProject'

currentProjectId :: Functor f => (ProjectId -> f ProjectId) -> Workspace -> f Workspace
currentProjectId = currentLocation . GraphLocation.projectId

currentLibrary' :: Workspace -> Library
currentLibrary' w = fromMaybe err $ project ^? Project.libs . ix lid where
    lid = w ^. currentLocation . GraphLocation.libraryId
    project = w ^. currentProject
    err = $(placeholder "Invalid library id")

currentLibrary :: Getter Workspace Library
currentLibrary = to currentLibrary'

upperWorkspace :: Workspace -> Workspace
upperWorkspace w = w & currentLocation . GraphLocation.breadcrumb . Breadcrumb.items .~ newItems where
    newItems = fromMaybe def $ mayTail $ w ^. currentLocation . GraphLocation.breadcrumb . Breadcrumb.items

isOnTopBreadcrumb :: Workspace -> Bool
isOnTopBreadcrumb = view (currentLocation . GraphLocation.breadcrumb . Breadcrumb.items . to null)

makeLenses ''UIGraphLocation
instance ToJSON UIGraphLocation
instance FromJSON UIGraphLocation

uiGraphLocation' :: Workspace -> UIGraphLocation
uiGraphLocation' w = UIGraphLocation project library breadcrumb' where
    breadcrumb' = w ^. currentLocation . GraphLocation.breadcrumb
    project     = w ^. currentProjectId
    library     = w ^. currentLibrary  . Library.path

uiGraphLocation :: Getter Workspace UIGraphLocation
uiGraphLocation = to uiGraphLocation'

fromUIGraphLocation :: Map ProjectId Project -> UIGraphLocation -> Maybe GraphLocation
fromUIGraphLocation projs (UIGraphLocation projId lib bc) = do
    project <- projs ^? ix projId
    let libs  = IntMap.toList $ project ^. Project.libs
    (libraryId, _) <- find (\(_,p) -> p ^. Library.path == lib) libs
    return $ GraphLocation projId libraryId bc

instance HasBreadcrumb Workspace where
    breadcrumb = currentLocation . GraphLocation.breadcrumb
