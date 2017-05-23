module LunaStudio.API.Graph.AddPort where

import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import           LunaStudio.API.Graph.Result   (Result)
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.PortRef       (AnyPortRef, OutPortRef)
import           Prologue


data Request = Request { _location   :: GraphLocation
                       , _outPortRef :: OutPortRef
                       , _connectTo  :: [AnyPortRef]
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
instance Binary Request
instance G.GraphRequest Request where location = location


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.node.addPort"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
