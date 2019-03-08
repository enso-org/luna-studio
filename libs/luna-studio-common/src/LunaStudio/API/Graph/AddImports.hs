module LunaStudio.API.Graph.AddImports where

import Prologue

import qualified LunaStudio.API.Graph.Request          as Graph
import qualified LunaStudio.API.Topic                  as Topic
import qualified LunaStudio.Data.Searcher.Hint.Library as Library

import Data.Aeson.Types              (ToJSON)
import Data.Binary                   (Binary)
import Data.Set                      (Set)
import LunaStudio.Data.Diff          (Diff)
import LunaStudio.Data.GraphLocation (GraphLocation)


data Request = Request
    { _location :: GraphLocation
    , _modules  :: Set Library.Name
    } deriving (Eq, Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Graph.GraphRequest Request where location = location

instance Topic.MessageTopic Request where
    topic = "empire.graph.imports.add"
