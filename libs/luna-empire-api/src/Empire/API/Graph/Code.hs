module Empire.API.Graph.Code where

import           Data.Binary                   (Binary)
import           Data.Text                     (Text)
import           Prologue                      hiding (Text)
import           Prologue                      hiding (Text)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node)
import qualified Empire.API.Topic              as T

data Update = Update { _location :: GraphLocation
                     , _code     :: Text
                     } deriving (Generic, Eq, NFData, Show)

makeLenses ''Update

instance Binary Update

topicPrefix = "empire.graph.code"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
