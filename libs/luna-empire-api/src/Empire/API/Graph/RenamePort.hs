module Empire.API.Graph.RenamePort where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (ExpressionNode, InputSidebar)
import           Empire.API.Data.PortRef       (OutPortRef)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T

data Request = Request { _location :: GraphLocation
                       , _portRef  :: OutPortRef
                       , _name     :: String
                       } deriving (Generic, Eq, NFData, Show)

data Inverse = Inverse { _prevName :: String
                       } deriving (Generic, Show, Eq, NFData)

data Result = Result { _sidebar      :: InputSidebar
                     , _updatedNodes :: [ExpressionNode]
                     } deriving (Generic, Show, Eq, NFData)

type Response = Response.Response Request Inverse Result
instance Response.ResponseResult Request Inverse Result

makeLenses ''Request
makeLenses ''Inverse
makeLenses ''Result
instance Binary Request
instance Binary Inverse
instance Binary Result
instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.renamePort"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
