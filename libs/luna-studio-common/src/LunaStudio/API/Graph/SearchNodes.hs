module LunaStudio.API.Graph.SearchNodes where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import           Data.Set                      (Set)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.NodeSearcher  (ImportName, ImportsHints)
import           Prologue


data Request = Request
    { _location       :: GraphLocation
    , _missingImports :: Set ImportName
    } deriving (Eq, Generic, Show)

data Result = Result
    { _searcherHints :: ImportsHints
    } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Result
instance NFData Result
instance ToJSON Result
instance G.GraphRequest Request where location = location


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.nodesearch"
instance T.MessageTopic (R.Request Request) where
    topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where
    topic _ = topicPrefix <> T.response
