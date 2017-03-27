module Empire.API.Graph.SetNodeExpression where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node, NodeId)
import qualified Empire.API.Response           as Response

import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T

data Request = Request { _location   :: GraphLocation
                       , _nodeId     :: NodeId
                       , _expression :: Text
                       } deriving (Generic, Eq, NFData, Show)

data Inverse = Inverse { _prevExpression :: Text
                       } deriving (Generic, Show, Eq, NFData)

type Response = Response.SimpleResponse Request Inverse
instance Response.ResponseResult Request Inverse ()

makeLenses ''Request
makeLenses ''Inverse
instance Binary Request
instance Binary Inverse
instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.updateExpression"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response