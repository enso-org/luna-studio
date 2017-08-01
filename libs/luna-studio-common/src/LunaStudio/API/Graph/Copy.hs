module LunaStudio.API.Graph.Copy where

import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Breadcrumb    (Breadcrumb, BreadcrumbItem, Named)
import           LunaStudio.Data.Graph         (Graph)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Node          (ExpressionNode)
import           LunaStudio.Data.NodeLoc       (NodeLoc)
import           LunaStudio.Data.NodeSearcher  (Items)
import           Prologue


data Request = Request { _location :: GraphLocation
                       , _selected :: [NodeLoc]
                       } deriving (Eq, Generic, Show)

data Result  = Result  { _clipboardPlain :: String
                       , _clipboardMeta  :: String
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
topicPrefix = "empire.graph.copy"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
