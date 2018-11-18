module LunaStudio.API.Graph.RemoveNodes where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import           Data.Map                      (Map)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Connection    (Connection)
import           LunaStudio.Data.Diff          (Diff)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Node          (ExpressionNode)
import           LunaStudio.Data.NodeLoc       (NodeLoc)
import           Prologue                      hiding (TypeRep)


data Request = Request
    { _location :: GraphLocation
    , _nodeLocs :: [NodeLoc]
    } deriving (Eq, Generic, Show)

data Inverse = Inverse
    { _nodes       :: [ExpressionNode]
    , _connections :: [Connection]
    } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Inverse

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Inverse
instance NFData Inverse
instance ToJSON Inverse
instance G.GraphRequest Request where location = location


type Response = Response.Response Request Inverse Diff
type instance Response.InverseOf Request = Inverse
type instance Response.ResultOf  Request = Diff

topicPrefix :: T.Topic
topicPrefix = "empire.graph.node.remove"
instance T.MessageTopic (R.Request Request) where
    topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where
    topic _ = topicPrefix <> T.response
