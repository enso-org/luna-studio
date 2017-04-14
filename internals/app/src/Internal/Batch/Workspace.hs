module Internal.Batch.Workspace where

import           Data.Aeson                    (ToJSON)
import           Internal.Prelude

import           Empire.API.Data.Breadcrumb    (Breadcrumb (..))
import           Empire.API.Data.GraphLocation (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Node       (Node)

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
