module Empire.API.Graph.MonadsUpdate where

import           Data.Binary                   (Binary)
import           Prologue                      hiding (TypeRep)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.TypeRep       (TypeRep)
import qualified Empire.API.Topic              as T

data Update = Update { _location :: GraphLocation
                     , _monads   :: [(TypeRep, [NodeId])]
                     } deriving (Generic, Eq, NFData, Show)

makeLenses ''Update

instance Binary Update

topicPrefix = "empire.graph.monad"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
