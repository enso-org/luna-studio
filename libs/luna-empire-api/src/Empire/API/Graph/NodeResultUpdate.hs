module Empire.API.Graph.NodeResultUpdate where

import           Prologue                      hiding (TypeRep, Text)
import           Data.Binary                   (Binary)
import           Data.Text                     (Text)
import           Prologue                      hiding (Text, TypeRep)

import qualified Empire.API.Data.DefaultValue  as DV (Value)
import           Empire.API.Data.Error         (Error)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.TypeRep       (TypeRep)
import qualified Empire.API.Topic              as T

data NodeValue = Value Text [DV.Value] | Error (Error TypeRep) deriving (Show, Eq, Generic, NFData)

data Update = Update { _location  :: GraphLocation
                     , _nodeId    :: NodeId
                     , _value     :: NodeValue
                     , _execTime  :: Integer
                     } deriving (Generic, Eq, NFData, Show)

makeLenses ''Update

instance Binary Update
instance Binary NodeValue

topicPrefix = "empire.graph.result"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
