module Empire.API.Graph.SetNodesMeta where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Graph.Request      as G
import           Empire.API.Graph.Result       (Result)
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T

type SingleUpdate = (NodeId, NodeMeta)

data Request = Request { _location :: GraphLocation
                       , _updates  :: [SingleUpdate]
                       } deriving (Generic, Eq, NFData, Show)


data Inverse = Inverse { _prevMeta :: [SingleUpdate]
                       } deriving (Generic, Show, Eq, NFData)

type Response = Response.Response Request Inverse Result
instance Response.ResponseResult Request Inverse Result

data Update   = Update { _location' :: GraphLocation
                       , _updates'  :: [SingleUpdate]
                       } deriving (Generic, Eq, NFData, Show)


makeLenses ''Request
makeLenses ''Inverse
makeLenses ''Update

instance Binary Request
instance Binary Inverse
instance Binary Update
instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.updateMeta"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
