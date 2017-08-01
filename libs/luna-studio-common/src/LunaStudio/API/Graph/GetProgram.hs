module LunaStudio.API.Graph.GetProgram where

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
import           LunaStudio.Data.Graph                (Graph)
import           LunaStudio.Data.GraphLocation        (GraphLocation)
import           LunaStudio.Data.NodeValue            (Visualizer)
import           LunaStudio.Data.Project              (LocationSettings)
import           LunaStudio.Data.TypeRep              (TypeRep)
import           Prologue                             hiding (TypeRep)


data Request = Request { _location             :: GraphLocation
                       , _prevLocationSettings :: Maybe (GraphLocation, LocationSettings)
                       } deriving (Eq, Generic, Show)

type Error = String

data Result  = Result  { _graph           :: Either Error Graph
                       , _code            :: Text
                       , _breadcrumb      :: Breadcrumb (Named BreadcrumbItem)
                       , _typeRepToVisMap :: Maybe (HashMap TypeRep Visualizer)
                       , _camera          :: CameraTransformation
                       } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance NFData Request
instance Binary Result
instance NFData Result
instance G.GraphRequest Request where location = location


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.program"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
