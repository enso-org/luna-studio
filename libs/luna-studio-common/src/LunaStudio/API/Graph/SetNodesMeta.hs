module LunaStudio.API.Graph.SetNodesMeta where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import           Data.Map                      (Map)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Diff          (Diff)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.NodeId        (NodeId)
import           LunaStudio.Data.NodeMeta      (NodeMeta)
import           Prologue


data Request = Request
    { _location :: GraphLocation
    , _updates  :: Map NodeId NodeMeta
    } deriving (Eq, Generic, Show)

data Update = Update
    { _location' :: GraphLocation
    , _updates'  :: Map NodeId NodeMeta
    } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Update
instance NFData Update
instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.node.updateMeta"
instance T.MessageTopic Update  where
    topic = T.topic @Request <> T.update
