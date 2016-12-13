module Event.NodeSearcher where


import           Data.Aeson               (FromJSON, ToJSON)
import           Luna.Studio.Prelude
import           Empire.API.Data.Node     (NodeId)
import           Empire.API.JSONInstances ()


data Event = Query     Text
           | Tree      Text
           | Create    {_expression :: Text, _nodeId :: Maybe NodeId}
           | QueryCmd  Text
           | TreeCmd   Text
           | CreateCmd {_expression :: Text, _nodeId :: Maybe NodeId}
           deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON Event
instance FromJSON Event

