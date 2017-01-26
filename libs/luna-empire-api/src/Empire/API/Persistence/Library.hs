module Empire.API.Persistence.Library where

import           Prologue

import           Data.Binary           (Binary)
import           Empire.API.Data.Graph (Graph)

type LibraryId = Int

data Library = Library { _name    :: Maybe String
                       , _path    :: String
                       , _graph   :: Graph
                       } deriving (Generic, Eq, NFData, Show)

makeLenses ''Library

instance Binary Library
