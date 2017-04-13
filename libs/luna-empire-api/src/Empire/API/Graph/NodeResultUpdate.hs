module Empire.API.Graph.NodeResultUpdate where

import           Data.Binary                   (Binary)
import           Data.Text                     (Text)
import           Prologue                      hiding (Text, TypeRep)
import           Prologue                      hiding (Text, TypeRep)
import           Empire.API.Data.Error         (Error)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.PortDefault   (PortValue,VisualizationValue)
import           Empire.API.Data.TypeRep       (TypeRep)
import qualified Empire.API.Topic              as Topic

type ShortValue = Text

data NodeValue = NodeValue ShortValue [PortValue]
               | NodeError (Error TypeRep)
               deriving (Show, Eq, Generic, NFData)
makePrisms ''NodeValue

data NodeVisualization = NodeVisualization { _visualizationValue :: VisualizationValue
                                           , _visualizationId    :: Int
                                           , _isPinned           :: Bool
                                           } deriving (Show, Eq, Generic, NFData)
makePrisms ''NodeVisualization

data Update = Update { _location :: GraphLocation
                     , _nodeId   :: NodeId
                     , _value    :: NodeValue
                     , _execTime :: Integer
                     } deriving (Generic, Eq, NFData, Show)
makeLenses ''Update

instance Binary Update
instance Binary NodeValue

topicPrefix = "empire.graph.result"
instance Topic.MessageTopic Update where topic _ = topicPrefix <> Topic.update
