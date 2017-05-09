module NodeEditor.Batch.Workspace where

import           Common.Prelude
import           Data.Aeson                    (ToJSON)

import qualified Empire.API.Data.Breadcrumb    as Breadcrumb
import           Empire.API.Data.GraphLocation (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Node          (ExpressionNode)
import           Empire.API.Data.NodeLoc       (HasBreadcrumb (..))
import           Text.ScopeSearcher.Item       (Items)


data Workspace = Workspace { _currentLocation  :: GraphLocation
                           , _lastUILocation   :: Maybe GraphLocation
                           , _nodeSearcherData :: Items ExpressionNode
                           } deriving (Show, Eq, Generic)

instance ToJSON Workspace

mk :: FilePath -> Workspace
mk path = Workspace (GraphLocation path def) def def

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
