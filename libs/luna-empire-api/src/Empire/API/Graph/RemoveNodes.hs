module Empire.API.Graph.RemoveNodes where

import           Data.Binary                   (Binary)
import           Prologue                      hiding (TypeRep)

import           Empire.API.Data.Connection    (Connection)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (ExpressionNode, NodeId)
import           Empire.API.Data.NodeLoc       (NodeLoc)
import           Empire.API.Data.PortRef       (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.TypeRep       (TypeRep)
import qualified Empire.API.Graph.Request      as G
import           Empire.API.Graph.Result       (Result)
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T


data Request = Request { _location :: GraphLocation
                       , _nodeLocs :: [NodeLoc]
                       } deriving (Generic, Eq, NFData, Show)

data Inverse = Inverse { _nodes       :: [ExpressionNode]
                       , _connections :: [Connection]
                       } deriving (Generic, Show, Eq, NFData)

type Response = Response.Response Request Inverse Result
instance Response.ResponseResult Request Inverse Result

makeLenses ''Request
makeLenses ''Inverse
instance Binary Request
instance Binary Inverse

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.remove"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
