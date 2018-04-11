module LunaStudio.API.Graph.SetNodesMeta where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import           Data.Map                      (Map)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
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

data Inverse = Inverse
    { _prevMeta :: Map NodeId NodeMeta
    } deriving (Eq, Generic, Show)

data Update = Update
    { _location' :: GraphLocation
    , _updates'  :: Map NodeId NodeMeta
    } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Inverse
makeLenses ''Update

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Inverse
instance NFData Inverse
instance ToJSON Inverse
instance Binary Update
instance NFData Update
instance G.GraphRequest Request where location = location


type Response = Response.Response Request Inverse Diff
instance Response.ResponseResult Request Inverse Diff

topicPrefix :: T.Topic
topicPrefix = "empire.graph.node.updateMeta"
instance T.MessageTopic (R.Request Request) where
    topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where
    topic _ = topicPrefix <> T.response
instance T.MessageTopic Update              where
    topic _ = topicPrefix <> T.update
