module LunaStudio.API.Graph.NodeResultUpdate where

import           Data.Binary                   (Binary)
import           Data.Text                     (Text)
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Error         (Error)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Node          (NodeId)
import           LunaStudio.Data.PortDefault   (VisualizationValue)
import           Prologue                      hiding (Text, TypeRep)


type ShortValue = Text

data NodeValue = NodeValue ShortValue [VisualizationValue]
               | NodeError Error
               deriving (Eq, Generic, NFData, Show)

data NodeVisualization = NodeVisualization { _visualizationValue :: VisualizationValue
                                           , _visualizationId    :: Int
                                           , _isPinned           :: Bool
                                           } deriving (Eq, Generic, NFData, Show)

data Update = Update { _location :: GraphLocation
                     , _nodeId   :: NodeId
                     , _value    :: NodeValue
                     , _execTime :: Integer
                     } deriving (Eq, Generic, NFData, Show)

makePrisms ''NodeValue
makePrisms ''NodeVisualization
makeLenses ''Update
instance Binary Update
instance Binary NodeValue


topicPrefix :: T.Topic
topicPrefix = "empire.graph.result"
instance T.MessageTopic Update where topic _ = topicPrefix <> T.update
