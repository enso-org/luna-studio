module LunaStudio.API.Persistence.Library where

import           Data.Aeson.Types      (FromJSON, ToJSON)
import           Data.Binary           (Binary)
import           LunaStudio.Data.Graph (Graph)
import           Prologue


type LibraryId = Int

data Library = Library
    { _name    :: Maybe String
    , _path    :: String
    , _graph   :: Graph
    } deriving (Eq, Generic, Show)

makeLenses ''Library

instance Binary   Library
instance NFData   Library
instance FromJSON Library
instance ToJSON   Library
