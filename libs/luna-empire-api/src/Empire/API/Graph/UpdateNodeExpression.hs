module Empire.API.Graph.UpdateNodeExpression where

import           Data.Binary                   (Binary)
import           Prologue
import qualified Data.Text.Lazy                as Text

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId, Node)
import qualified Empire.API.Response           as Response
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T
import qualified Empire.API.Request            as R

data Request = Request { _location   :: GraphLocation
                       , _nodeId     :: NodeId
                       , _expression :: Text
                       } deriving (Generic, Show, Eq)

data Inverse = Inverse { _expressionPrev :: String
                       } deriving (Generic, Show, Eq)

type Result = Maybe Node

type Response = Response.Response Request Inverse Result
instance Response.ResponseResult Request Inverse Result

makeLenses ''Request
makeLenses ''Inverse
instance Binary Request
instance Binary Inverse
instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.updateExpression"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response
