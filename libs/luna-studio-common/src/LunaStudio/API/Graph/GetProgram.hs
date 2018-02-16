module LunaStudio.API.Graph.GetProgram where

import           Data.Aeson.Types                     (ToJSON)
import           Data.Binary                          (Binary (..))
import           Data.Hashable                        (Hashable)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import qualified LunaStudio.API.Graph.Request         as G
import qualified LunaStudio.API.Graph.SaveSettings    as SaveSettings
import qualified LunaStudio.API.Request               as R
import qualified LunaStudio.API.Response              as Response
import qualified LunaStudio.API.Topic                 as T
import           LunaStudio.Data.Breadcrumb           (Breadcrumb, BreadcrumbItem, Named)
import           LunaStudio.Data.CameraTransformation (CameraTransformation)
import           LunaStudio.Data.Error                (Error, GraphError)
import           LunaStudio.Data.Graph                (Graph)
import           LunaStudio.Data.GraphLocation        (GraphLocation)
import           LunaStudio.Data.NodeSearcher         (ImportName)
import           LunaStudio.Data.Project              (LocationSettings)
import           LunaStudio.Data.TypeRep              (TypeRep)
import           LunaStudio.Data.Visualizer           (Visualizer)
import           Prologue                             hiding (TypeRep)


data Request = Request { _location             :: GraphLocation
                       , _prevLocationSettings :: Maybe (GraphLocation, LocationSettings)
                       , _retrieveLocation     :: Bool
                       } deriving (Eq, Generic, Show)

data Result  = Result  { _graph                  :: Either (Error GraphError) Graph
                       , _code                   :: Text
                       , _breadcrumb             :: Breadcrumb (Named BreadcrumbItem)
                       , _availableImports       :: [ImportName]
                       , _typeRepToVisMap        :: Maybe (HashMap TypeRep Visualizer)
                       , _camera                 :: CameraTransformation
                       , _newLocation            :: GraphLocation
                       , _projectVisualizersPath :: Maybe FilePath
                       } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Result
instance NFData Result
instance ToJSON Result
instance G.GraphRequest Request where location = location


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.program"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
