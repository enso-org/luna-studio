module LunaStudio.API.Graph.Request where

import           LunaStudio.Data.GraphLocation (GraphLocation)
import           Prologue

class GraphRequest a where
  location :: Lens' a GraphLocation
