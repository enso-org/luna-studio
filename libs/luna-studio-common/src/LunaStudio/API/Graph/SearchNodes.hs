module LunaStudio.API.Graph.SearchNodes where

import Prologue

import qualified LunaStudio.API.Graph.Request as G
import qualified LunaStudio.API.Topic         as T

import Data.Aeson.Types              (ToJSON)
import Data.Binary                   (Binary)
import Data.Map                      (Map)
import Data.Set                      (Set)
import LunaStudio.Data.GraphLocation (GraphLocation)
import LunaStudio.Data.Searcher.Node (LibraryHints, LibraryName)


data Request = Request
    { _location         :: GraphLocation
    , _missingLibraries :: Set LibraryName
    } deriving (Eq, Generic, Show)

data Result = Result
    { _searcherHints :: Map LibraryName LibraryHints
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

instance T.MessageTopic Request where
    topic = "empire.graph.nodesearch"
