module LunaStudio.API.Graph.AddSubgraph where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Connection    (Connection)
import           LunaStudio.Data.Diff          (Diff)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Node          (ExpressionNode)
import           Prologue


data Request = Request
    { _location    :: GraphLocation
    , _nodes       :: [ExpressionNode]
    , _connections :: [Connection]
    } deriving (Eq, Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request
instance G.GraphRequest Request where location = location


type Response = Response.Response Request () Diff
type instance Response.InverseOf Request = ()
type instance Response.ResultOf  Request = Diff

topicPrefix :: T.Topic
topicPrefix = "empire.graph.node.addSubgraph"
instance T.MessageTopic (R.Request Request) where
    topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where
    topic _ = topicPrefix <> T.response
