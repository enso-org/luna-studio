module Luna.Studio.Batch.Workspace where

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Convert                  (Convertible (..))
import qualified Data.List                     as List
import           Luna.Studio.Prelude

import           Empire.API.Data.Breadcrumb    (Breadcrumb (Breadcrumb), BreadcrumbItem)
import qualified Empire.API.Data.Breadcrumb    as Breadcrumb
import           Empire.API.Data.GraphLocation (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Node          (ExpressionNode)
import           Empire.API.Data.NodeLoc       (HasBreadcrumb (..))
import           Empire.API.Data.Project       (Project, ProjectId)
import qualified Empire.API.Data.Project       as Project
import           Text.ScopeSearcher.Item       (Items)


data Workspace = Workspace { _currentLocation  :: GraphLocation
                           , _lastUILocation   :: Maybe GraphLocation
                           , _isGraphLoaded    :: Bool
                           , _nodeSearcherData :: Items ExpressionNode
                           } deriving (Show, Eq, Generic)

instance ToJSON Workspace

mk :: FilePath -> Workspace
mk path = Workspace (GraphLocation path def) def False def

makeLenses ''Workspace

uiGraphLocation' :: Workspace -> GraphLocation
uiGraphLocation' w = GraphLocation library breadcrumb' where
    breadcrumb' = w ^. currentLocation . GraphLocation.breadcrumb
    library     = w ^. currentLocation . GraphLocation.filePath

upperWorkspace :: Workspace -> Workspace
upperWorkspace w = w & currentLocation . GraphLocation.breadcrumb . Breadcrumb.items .~ newItems where
    newItems = fromMaybe def $ mayTail $ w ^. currentLocation . GraphLocation.breadcrumb . Breadcrumb.items

isOnTopBreadcrumb :: Workspace -> Bool
isOnTopBreadcrumb = view (currentLocation . GraphLocation.breadcrumb . Breadcrumb.items . to null)

uiGraphLocation :: Getter Workspace GraphLocation
uiGraphLocation = to uiGraphLocation'


instance HasBreadcrumb Workspace where
    breadcrumb = currentLocation . GraphLocation.breadcrumb
