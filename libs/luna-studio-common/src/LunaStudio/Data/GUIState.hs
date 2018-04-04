module LunaStudio.Data.GUIState where

import           Data.Aeson.Types                     (FromJSON, ToJSON)
import           Data.Binary                          (Binary (get, put))
import           Data.Hashable                        (Hashable)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.Set                             (Set)
import           LunaStudio.Data.Breadcrumb           (Breadcrumb, BreadcrumbItem, Named)
import           LunaStudio.Data.CameraTransformation (CameraTransformation)
import           LunaStudio.Data.Code                 (Code)
import           LunaStudio.Data.Error                (Error, GraphError)
import           LunaStudio.Data.Graph                (Graph)
import           LunaStudio.Data.NodeSearcher         (ImportName)
import           LunaStudio.Data.TypeRep              (TypeRep)
import           LunaStudio.Data.Visualizer           (Visualizer)
import           Prologue                             hiding (TypeRep)


data GUIState = GUIState
    { _breadcrumb             :: Breadcrumb (Named BreadcrumbItem)
    , _imports                :: Set ImportName
    , _defaultVisualizers     :: HashMap TypeRep Visualizer
    , _camera                 :: CameraTransformation
    , _projectVisualizersPath :: Maybe FilePath
    , _code                   :: Code
    , _graph                  :: Either (Error GraphError) Graph
    } deriving (Eq, Generic, Show)

makeLenses ''GUIState

instance Binary   GUIState
instance NFData   GUIState
instance FromJSON GUIState
instance ToJSON   GUIState

--TODO: Replace this with better implementation for all HashMaps
instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HashMap k v) where
    put = put . HashMap.toList
    get = HashMap.fromList <$> get
