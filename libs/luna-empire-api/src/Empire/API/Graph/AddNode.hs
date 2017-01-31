module Empire.API.Graph.AddNode where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node, NodeId)
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T

data NodeType = ExpressionNode { _expression :: Text } deriving (Generic, Eq, NFData, Show)

data Request = Request { _location  :: GraphLocation
                       , _nodeType  :: NodeType
                       , _nodeMeta  :: NodeMeta
                       , _connectTo :: Maybe NodeId
                       } deriving (Generic, Eq, NFData, Show)
type Result = Node

type Response = Response.Response Request Result
instance Response.ResponseResult Request Result

data Update = Update { _location'  :: GraphLocation
                     , _node'      :: Node
                     } deriving (Generic, Eq, NFData, Show)

makeLenses ''Request
makeLenses ''Update

instance Binary NodeType
instance Binary Request
instance Binary Update

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.add"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update               where topic _ = topicPrefix <> T.update
