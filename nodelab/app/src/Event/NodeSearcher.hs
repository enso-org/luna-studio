module Event.NodeSearcher where


import           Data.Aeson           (FromJSON, ToJSON)
import           Empire.API.Data.Node (NodeId)
import           Luna.Studio.Prelude



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
