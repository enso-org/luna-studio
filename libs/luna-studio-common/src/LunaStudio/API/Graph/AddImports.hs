module LunaStudio.API.Graph.AddImports where

import Prologue

import qualified LunaStudio.API.Graph.Request as G
import qualified LunaStudio.API.Topic         as T

import Data.Aeson.Types              (ToJSON)
import Data.Binary                   (Binary)
import Data.Set                      (Set)
import LunaStudio.Data.Diff          (Diff)
import LunaStudio.Data.GraphLocation (GraphLocation)
import LunaStudio.Data.Searcher.Node (LibraryName)


data Request = Request
    { _location :: GraphLocation
    , _modules  :: Set LibraryName
    } deriving (Eq, Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request
instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.imports.add"
