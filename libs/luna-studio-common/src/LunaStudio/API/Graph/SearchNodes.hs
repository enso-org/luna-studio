module LunaStudio.API.Graph.SearchNodes where

import           Data.Binary                   (Binary)
import           Data.Map                      (Map)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation (GraphLocation))
import           LunaStudio.Data.Node          (ExpressionNode)
import           LunaStudio.Data.NodeSearcher  (Items)
import           Prologue


data Request = Request { _location :: Maybe GraphLocation
                       } deriving (Eq, Generic, Show)

data Result  = Result  { _globalFunctions :: [Text]
                       , _globalClasses   :: Map Text [Text]
                       } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result
instance Binary Request
instance NFData Request
instance Binary Result
instance NFData Result
instance G.GraphRequest Request where location = lens (fromMaybe (GraphLocation def def) . view location) (\r l -> r & location .~ Just l)


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.nodesearch"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
