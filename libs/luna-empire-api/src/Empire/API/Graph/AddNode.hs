module Empire.API.Graph.AddNode where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.Connection    (Connection)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (ExpressionNode, Node, NodeId)
import           Empire.API.Data.NodeLoc       (NodeLoc)
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T

data Request = Request { _location   :: GraphLocation
                       , _nodeLoc    :: NodeLoc
                       , _expression :: Text
                       , _nodeMeta   :: NodeMeta
                       , _connectTo  :: Maybe NodeId
                       } deriving (Generic, Eq, NFData, Show)

data Result = Result { _node          :: ExpressionNode
                     , _newConns      :: [Connection]
                     , _connectedNode :: Maybe Node
                     } deriving (Generic, Eq, NFData, Show)

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

makeLenses ''Request
makeLenses ''Result
instance Binary Request
instance Binary Result

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.add"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response
