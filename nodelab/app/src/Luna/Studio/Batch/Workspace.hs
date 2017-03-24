module Luna.Studio.Batch.Workspace where

import           Data.Aeson                    (FromJSON, ToJSON)
import qualified Data.IntMap.Lazy              as IntMap
import           Data.Map.Lazy                 (Map)
import           Data.UUID.Types               (nil)
import           Luna.Studio.Prelude

import           Empire.API.Data.Breadcrumb    (Breadcrumb (..), BreadcrumbItem)
import           Empire.API.Data.GraphLocation (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Library       (Library)
import           Empire.API.Data.Node       (Node)
import qualified Empire.API.Data.Library       as Library
import           Empire.API.Data.Project       (Project, ProjectId)
import qualified Empire.API.Data.Project       as Project

import           Text.ScopeSearcher.Item       (Items)

data Workspace = Workspace { _currentLocation  :: GraphLocation
                           , _lastUILocation   :: Maybe GraphLocation
                           , _isGraphLoaded    :: Bool
                           , _nodeSearcherData :: Items Node
                           } deriving (Show, Eq, Generic)

instance ToJSON Workspace

instance Default Workspace where
    def = Workspace (GraphLocation "" (Breadcrumb [])) def False def

makeLenses ''Workspace


uiGraphLocation' :: Workspace -> GraphLocation
uiGraphLocation' w = GraphLocation library breadcrumb' where
    breadcrumb' = w ^. currentLocation . GraphLocation.breadcrumb
    library     = w ^. currentLocation . GraphLocation.filePath

uiGraphLocation :: Getter Workspace GraphLocation
uiGraphLocation = to uiGraphLocation'
