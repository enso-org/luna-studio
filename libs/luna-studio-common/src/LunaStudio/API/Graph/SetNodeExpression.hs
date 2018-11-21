module LunaStudio.API.Graph.SetNodeExpression where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Diff          (Diff)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.NodeId        (NodeId)
import           Prologue


data Request = Request
    { _location   :: GraphLocation
    , _nodeId     :: NodeId
    , _expression :: Text
    } deriving (Eq, Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request

instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.node.updateExpression"
