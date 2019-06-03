module NodeEditor.Batch.Workspace where

import Common.Prelude

import qualified LunaStudio.Data.Breadcrumb    as Breadcrumb
import qualified LunaStudio.Data.GraphLocation as GraphLocation

import LunaStudio.Data.GraphLocation (GraphLocation (..))
import LunaStudio.Data.NodeLoc       (HasBreadcrumb (..))
import Path (Path, Rel, File)


data Workspace = Workspace { _currentLocation  :: GraphLocation
                           , _lastUILocation   :: Maybe GraphLocation
                           } deriving (Show, Eq, Generic)

mk :: Path Rel File -> Workspace
mk path = Workspace (GraphLocation path def) def

makeLenses ''Workspace

uiGraphLocation' :: Workspace -> GraphLocation
uiGraphLocation' w = GraphLocation library breadcrumb' where
    breadcrumb' = w ^. currentLocation . GraphLocation.breadcrumb
    library     = w ^. currentLocation . GraphLocation.filePath

upperWorkspace :: Workspace -> Workspace
upperWorkspace w = w & currentLocation . GraphLocation.breadcrumb . Breadcrumb.items .~ newItems where
    newItems = fromMaybe def $ mayInit $ w ^. currentLocation . GraphLocation.breadcrumb . Breadcrumb.items

isOnTopBreadcrumb :: Workspace -> Bool
isOnTopBreadcrumb = view (currentLocation . GraphLocation.breadcrumb . Breadcrumb.items . to null)

uiGraphLocation :: Getter Workspace GraphLocation
uiGraphLocation = to uiGraphLocation'


instance HasBreadcrumb Workspace where
    breadcrumb = currentLocation . GraphLocation.breadcrumb
