module LunaStudio.API.Graph.SearchNodes where

import Prologue

import qualified LunaStudio.API.Graph.Request          as G
import qualified LunaStudio.API.Topic                  as T
import qualified LunaStudio.Data.Searcher.Hint.Library as Library

import Data.Aeson.Types                      (ToJSON)
import Data.Binary                           (Binary)
import Data.Map                              (Map)
import Data.Set                              (Set)
import LunaStudio.Data.GraphLocation         (GraphLocation)
import LunaStudio.Data.Searcher.Hint.Library (SearcherLibraries)


data Request = Request
    { _location         :: GraphLocation
    , _missingLibraries :: Set Library.Name
    } deriving (Eq, Generic, Show)

data Result = Result
    { _searcherHints :: SearcherLibraries
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
